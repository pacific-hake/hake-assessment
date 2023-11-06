#' Run the model iteratively zoning in on a catch value that reduces the
#' SPR to 1, within the tolerance given (`ct_levels_spr_tol`)
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param forecast_yrs A vector if the years to forecast for
#' @param ct_levels_spr_tol The tolerance to be within 1 for the SPR
#' @param ct_levels_catch_tol Catch tolerance. If the upper and lower catch
#' in the algorithm are within this, it is assumed that the SPR is close
#' enough
#' @param ct_levels_max_iter The maximum number of iterations to do
#' @param ... Not used
#' @export
run_ct_levels_spr_100 <- function(model,
                                  forecast_yrs,
                                  ct_levels_spr_tol,
                                  ct_levels_catch_tol,
                                  ct_levels_max_iter,
                                  ...){

  pth <- here::here(doc_path, spr_100_path)

  files <- list.files(model$mcmc_path)
  files <- files[files != "sso"]
  file.copy(file.path(model$mcmc_path,
                      files),
            file.path(spr_100_path,
                      files),
            copy.mode = TRUE)

  # Copy derived posteriors from the applicable directory
  file.copy(file.path(ifelse(model$extra_mcmc_exists,
                             model$extra_mcmc_path,
                             model$mcmc_path),
                      derposts_fn),
            file.path(spr_100_path,
                      derposts_fn))

  forecast_file <- file.path(spr_100_path, "forecast.ss")

  spr_100_catch <- vector(length = length(forecast_yrs), mode = "numeric")
  for(i in 1:length(forecast_yrs)){
    fore <- SS_readforecast(forecast_file,
                            Nfleets = 1,
                            Nareas = 1,
                            nseas = 1,
                            verbose = FALSE)
    fore$Ncatch <- length(forecast_yrs[1:i])
    spr_100_catch[i] <- median(model$mcmc[paste0("ForeCatch_",
                                                 forecast_yrs[i])][[1]])
    upper <- spr_100_catch[i]
    lower <- 0
    iter <- 1
    repeat{
      fore$ForeCatch <- data.frame(Year = forecast_yrs[1:i],
                                   Seas = 1,
                                   Fleet = 1,
                                   Catch_or_F = spr_100_catch[1:i])
      SS_writeforecast(fore,
                       dir = spr_100_path,
                       overwrite = TRUE,
                       verbose = FALSE)
      unlink(file.path(spr_100_path, derposts_fn),
             force = TRUE)

      # Make a modification to the starter file so the extra MCMC files are
      # not created
      modify_starter_mcmc_type(spr_100_path, 1)

      shell_command <- paste0("cd ", spr_100_path, " && ",
                              ss_executable, " -mceval")
      system_(shell_command, wait = TRUE, intern = !show_ss_output)
      out <- read.table(file.path(spr_100_path,
                                  derposts_fn),
                        header = TRUE)
      spr <- median(as.numeric(out[paste0("SPRratio_", forecast_yrs[i])][[1]]))
      message("SPR 100, for forecast year: ",
              forecast_yrs[i], "of ",
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
}
