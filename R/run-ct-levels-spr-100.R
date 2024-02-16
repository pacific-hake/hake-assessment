#' Run the model iteratively zoning in on a catch value that reduces the
#' SPR to 1, within the tolerance given by (`ct_levels_spr_tol`)
#'
#' @param model The SS3 model output as loaded by [create_rds_file()]
#' @param forecast_yrs A vector if the years to forecast for
#' @param ss_exe The name of executable to use or `NULL` to use the package
#' data variable [ss_executable]
#' @param keep_files Logical. If `TRUE`, keep all files in the directory,
#' if `FALSE` delete all files except for the filename contained in the
#' the `forecast_fn` variable. This is 'forecast.ss' by default for SS3
#' @param ... Absorbs other arguments intended for other functions
#'
#' @return Nothing, the 'forecast.ss' file will have the catch numbers at the
#' end of the file under 'Catch_or_F'
#' @export
run_ct_levels_spr_100 <- function(
    model,
    forecast_yrs = get_assess_yr():(get_assess_yr() + 3),
    ss_exe = NULL,
    keep_files = FALSE,
    ...){

  ss_exe <- get_ss3_exe_name(ss_exe)

  pth <- here(model$path, ct_levels_path, spr_100_path)
  run_catch_levels_copy_input_files(model, pth)
  dest_derposts_fullpath_fn <- file.path(pth, derposts_fn)
  forecast_fullpath_fn <- file.path(pth, forecast_fn)

  spr_100_catch <- vector(length = length(forecast_yrs), mode = "numeric")
  for(i in seq_along(forecast_yrs)){
    fore <- SS_readforecast(forecast_fullpath_fn,
                            Nfleets = 1,
                            Nareas = 1,
                            nseas = 1,
                            verbose = FALSE)
    fore$Ncatch <- length(forecast_yrs[1:i])

    fore_catch_label <- paste0("ForeCatch_", forecast_yrs[i])
    fore_catch_label_sym <- sym(fore_catch_label)

    spr_100_catch[i] <- model$mcmc |>
      pull(!!fore_catch_label_sym) |>
      median(na.rm = TRUE)

    iter <- 1
    prop <- 0.1
    last_catch <- spr_100_catch[i]
    last_above_1 <- NA
    repeat{
      fore$ForeCatch <- data.frame(Year = forecast_yrs[1:i],
                                   Seas = 1,
                                   Fleet = 1,
                                   Catch_or_F = spr_100_catch[1:i])
      SS_writeforecast(fore,
                       dir = pth,
                       overwrite = TRUE,
                       verbose = FALSE)
      unlink(dest_derposts_fullpath_fn, force = TRUE)

      # Make a modification to the starter file so the extra MCMC files are
      # not created
      modify_starter_mcmc_type(pth, 1)

      shell_command <- paste0("cd ", pth, " && ",
                              ss_exe, " -mceval")
      system_(shell_command, wait = TRUE, intern = TRUE)
      out <- read.table(dest_derposts_fullpath_fn, header = TRUE) |>
        as_tibble()
      spr_yr_label <- paste0("SPRratio_", forecast_yrs[i])
      spr_yr_label_sym <- sym(spr_yr_label)

      spr <- out |>
        pull(!!spr_yr_label_sym) |>
        median(na.rm = TRUE)

      message("SPR 100, for forecast year: ",
              forecast_yrs[i], " of ",
              tail(forecast_yrs, 1))
      message("SPR difference from 1: ",
              abs(spr - 1),
              " < ",
              ct_levels_spr_tol, " ? ",
              ifelse(abs(spr - 1) < ct_levels_spr_tol, "Yes", "No"))

      if(abs(spr - 1) <= ct_levels_spr_tol){
        warning("An SPR value close to 1 (", spr, ") within the tolerance of ",
                "`ct_levels_spr_tol` = ", ct_levels_spr_tol, " was reached.")
        spr_100_catch[i] <- last_catch
        break
      }
      if(iter == ct_levels_max_iter){
        warning("The maximum number of iterations (", ct_levels_max_iter,
                ") was reached for forecast year ", forecast_yrs[i],
                ". The SPR difference in the last iteration was ",
                spr - 1, "\n")
        break
      }

      if(spr > 1){
        if(!is.na(last_above_1) && !last_above_1){
          prop <- prop / 2
        }
        message("spr greater than 1, `last_catch` set to `prop` ", prop, " * ",
                f(last_catch, 2))
        last_catch <- last_catch * (1 - prop)
        last_above_1 <- TRUE
      }else{
        if(!is.na(last_above_1) && last_above_1){
          prop <- prop / 2
        }
        message("spr less than 1, `last_catch` set to `prop` ", prop, " * ",
                f(last_catch, 2))
        last_catch <- last_catch * (1 + prop)
        last_above_1 <- FALSE
      }
      last_spr <- spr
      spr_100_catch[i] <- last_catch
      message("  which is ", f(last_catch, 2))
      iter <- iter + 1
    }
  }

  if(!keep_files){
    fns <- list.files(pth)
    fns <- fns[fns != forecast_fn]
    fns <- file.path(pth, fns)
    unlink(fns, force = TRUE)
  }

  invisible()
}
