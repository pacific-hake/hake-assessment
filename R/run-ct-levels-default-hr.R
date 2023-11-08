#' Run the model iteratively reducing the difference between the first and
#' second year projections to find a stable catch within the the given
#' tolerance
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param forecast_yrs A vector of forecast years
#' @param ss_exe The name of the SS3 executable. If run standalone,
#' this will be [ss_executable]. If run from the context of of the [bookdown]
#' document, this will be set as a YAML key/tag
#' @param ... Absorbs arguments intended for other functions
#'
#' @export
run_ct_levels_default_hr <- function(model,
                                     forecast_yrs,
                                     ss_exe = ss_executable,
                                     ...){

  pth <- here(model$path, ct_levels_path, default_hr_path)
  dir.create(pth, showWarnings = FALSE)
browser()
  files <- list.files(model$mcmc_path)
  if(!all(ss_input_files %in% files)){
    stop("`run_ct_levels_default_hr`: At least one SS3 input file missing ",
         "in directory\n`", pth, "`\nThe missing file(s) are:\n",
         paste(ss_input_files[!ss_input_files %in% files], collapse = "\n"))
  }
  copy_flag <- file.copy(file.path(model$path,
                                   ss_input_files),
                         file.path(pth, ss_input_files),
                         copy.mode = TRUE)
  if(!all(copy_flag)){
    stop("`run_ct_levels_default_hr`: At least one SS3 imput file failed ",
         "to copy from directory\n`", model$path, "` to directory\n`",
         pth, "`.\nThe file(s) not copied are:\n",
         paste(ss_input_files[!copy_flag], collapse = "\n"))
  }

  src_derposts_fn <- file.path(ifelse(model$extra_mcmc_exists,
                                      model$extra_mcmc_path,
                                      model$mcmc_path),
                               derposts_fn)
  if(!file.exists(src_derposts_fn)){
    stop("`run_ct_levels_default_hr`: The file:\n`", src_derposts_fn,
         "\n` does not exist and therefore cannot be copied to directory\n`",
         pth, "`")
  }

  # Copy derived posteriors from the applicable directory
  copy_flag <- file.copy(src_derposts_fn,
                         file.path(pth, derposts_fn))
  if(!copy_flag){
    stop("`run_ct_levels_default_hr`: The file:\n`", src_derposts_fn, "`\n",
         "could not be copied to directory\n`", pth, "`")
  }

  forecast_file <- file.path(pth, forecast_fn)
  default_hr_catch <- vector(length = length(forecast_yrs), mode = "numeric")
  for(i in 1:length(forecast_yrs)){
    out <- read.table(file.path(pth, derposts_fn), header = TRUE) |>
      as_tibble()
    fore_yr_label <- paste0("ForeCatch_", forecast_yrs[i])
    fore_yr_label_sym <- sym(fore_yr_label)
    default_hr_catch[i] <- out |>
      pull(!!fore_yr_label_sym) |>
      median()
    fore <- SS_readforecast(forecast_file,
                            Nfleets = 1,
                            Nareas = 1,
                            nseas = 1,
                            verbose = FALSE)
    fore$Ncatch <- length(forecast_yrs[1:i])
    fore$ForeCatch <- data.frame(Year = forecast_yrs[1:i],
                                 Seas = 1,
                                 Fleet = 1,
                                 Catch_or_F = default_hr_catch[1:i])
    SS_writeforecast(fore,
                     dir = pth,
                     overwrite = TRUE,
                     verbose = FALSE)
    unlink(src_derposts_fn, force = TRUE)
    message("Default HR - for forecast year: ",
            forecast_yrs[i], " of ",
            tail(forecast_yrs, 1))

    # Make a modification to the starter file so the extra MCMC files are
    # not created
    modify_starter_mcmc_type(pth, 1)

    shell_command <- paste0("cd ", pth, " && ",
                            ss_exe, " -mceval")
    system_(shell_command, wait = TRUE, intern = !show_ss_output)
  }
}
