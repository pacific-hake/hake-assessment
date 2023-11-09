#' Run the model iteratively zoning in on a catch value that reduces the
#' SPR to 1, within the tolerance given by (`ct_levels_spr_tol`)
#'
#' @param model The SS3 model output as loaded by [create_rds_file()]
#' @param forecast_yrs A vector if the years to forecast for
#' @param ss_exe The name of the SS3 executable. If run standalone,
#' this will be [ss_executable]. If run from the context of of the [bookdown]
#' document, this will be set as a YAML key/tag
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
      median()
    upper <- spr_100_catch[i]
    lower <- 0
    iter <- 1
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
      system_(shell_command, wait = TRUE, intern = !show_ss_output)
      out <- read.table(dest_derposts_fullpath_fn, header = TRUE) |>
        as_tibble()
      spr_yr_label <- paste0("SPRratio_", forecast_yrs[i])
      spr_yr_label_sym <- sym(spr_yr_label)

      spr <- out |>
        pull(!!spr_yr_label_sym) |>
        median()

      message("SPR 100, for forecast year: ",
              forecast_yrs[i], " of ",
              tail(forecast_yrs, 1))
      message("SPR difference from 1: ",
              abs(spr - 1),
              " < ",
              ct_levels_spr_tol, " ? ",
              ifelse(abs(spr - 1) < ct_levels_spr_tol, "Yes", "No"))
      message("Upper catch: ", upper,
              " - Lower catch: ", lower,
              ". Difference: ",
              abs(upper - lower), " < ", ct_levels_catch_tol, " ? ",
              ifelse(abs(upper - lower) < ct_levels_catch_tol,
                     "Yes\n",
                     "No\n"))

      if(abs(spr - 1) < ct_levels_spr_tol |
         abs(upper - lower) < ct_levels_catch_tol){
        # Sometimes, upper and lower can end up close to equal,
        #  but the tolerance is still not met. In this case, assume
        #  the catch creates an SPR of 100% even though it is slightly off.
        break
      }
      if(iter == ct_levels_max_iter){
        warning("The maximum number of iterations (", ct_levels_max_iter,
                ") was reached for forecast year ", forecast_yrs[i],
                ". The SPR difference in the last iteration was ",
                spr - 1, "\n")
        break
      }

      if(spr - 1 > 0){
        upper <- spr_100_catch[i]
        lower <- (upper + lower) / 2.0
        spr_100_catch[i] <- lower
        message("spr greater than 1, upper set to ", upper,
                ", lower set to ", lower,"\n")
      }else if(spr - 1 < 0){
        lower <- spr_100_catch[i]
        upper <- upper * 1.5
        spr_100_catch[i] <- upper
        message("spr less than 1, upper set to ", upper,
                ", lower set to ", lower,"\n")
      }else{
        message("spr exactly equal to 1, breaking\n")
        break
      }
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
