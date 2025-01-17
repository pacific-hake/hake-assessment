#' Fetch the output from previously-run forecasting using [run_forecasts()]
#'
#' @details If the forecasts directory does not exist or there is a problem
#' loading the forecasts, return `NA`
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param first Load this many of the files. If a non-positive number, load
#' them all. Used for debugging purposes to cut down the size of the
#' lists used
#' @param forecast_yrs A vector of forecast years
#' @param verbose Logical. If `TRUE`, show messages
#' @param ... Absorbs arguments intended for other functions
#'
#' @return A list of forecast outputs as read in by [r4ss::SSgetMCMC()]
#' @export
load_forecasts <- function(model_path = NULL,
                           first = 0,
                           forecast_yrs = get_assess_yr():(get_assess_yr() + 3),
                           verbose = TRUE,
                           ...){

  if(!check_forecasts(model_path, ...)){
    stop("The forecasts do not appear to have been run, have been run ",
         "incorrectly, or only been run partially. Re-run using ",
         "`run_forecasts` before trying to attach forecasting")
  }
  fore_fullpath <- file.path(model_path, forecasts_path)
  if(verbose){
    message("\nLoading forecast data from directory `", fore_fullpath,
            "`\n")
  }

  ct_levels_lst <- load_ct_levels(model_path, ...)
  ct_levels <- ct_levels_lst$ct_levels

  # Extract the catch level names from the list into a vector
  ct_levels_catch <- map(ct_levels, ~{.x[[1]]})
  ct_levels_desc <- map_chr(ct_levels, ~{.x[[2]]})
  ct_levels_names <- map_chr(ct_levels, ~{.x[[3]]})

  # Get the directory listing and choose the last one for loading
  dir_nms <- list.files(fore_fullpath)
  dir_nms_fullpath <- file.path(model_path, forecasts_path, dir_nms)

  # Make sure only year directories exist here
  pat <- paste0("^", forecasts_prepend, "20[0-9]{2}$")
  num_forecasts <- length(grep(pat, dir_nms))
  if(num_forecasts != length(forecast_yrs)){
    stop("The number of actual forecast years run (", num_forecasts, ") ",
         "does not match the number of forecast years specified in the ",
         "model (", length(forecast_yrs), "). Check the contents of the ",
         "directory\n`", fore_fullpath, "`\n")
  }

  if(supportsMulticore()){
    plan("multicore", workers = length(forecast_yrs))
  }else{
    message(paste0("`create_rds_files_retro()`: ", parallelism_warning))
    if(interactive()){
      message("\nContinue in sequential mode? (choose a number)")
      ans <- menu(c("Yes", "No"))
      if(ans == 2){
        message("`create_rds_files_retro()`: Bailing out at the user's ",
                "request")
        return(invisible())
      }
    }
    plan("sequential")
  }
  lst <- future_map(dir_nms_fullpath, \(fore_path){

    # Get the directory listing of the forecast directory and make sure
    # it matches what the catch levels are.currently set to in
    # forecast-catch-levels.R`
    fore_subdir <- dir(fore_path)
    # Eliminate extra forecast runs, possibly done after the document was
    # created, for the JMC meeting or some other reason
    fore_paths <- ct_levels_names[dir_nms %in% ct_levels_names]
    lvls_lst <- imap(ct_levels, \(catch_level, catch_level_ind){
      fore_level_path <- file.path(fore_path, catch_level[[3]])
      message("Loading from ", fore_level_path)

      mcmc_out <- SSgetMCMC(dir = fore_level_path,
                            writecsv = FALSE,
                            verbose = FALSE)
      if(first > 0){
        mcmc_out <- mcmc_out |>
          slice_head(n = first)
      }

      # Get the values of interest, namely Spawning biomass and SPR for the two
      # decision tables in the executive summary
      depl <- mcmc_out %>%
        select(grep("Bratio_", names(.)))
      spr <- mcmc_out %>%
        select(grep("SPRratio_", names(.)))

      # Strip out the Bratio_ and SPRratio_ headers so columns are years only
      #names(sb) <- gsub("Bratio_", "", names(sb))
      names(spr) <- gsub("SPRratio_", "", names(spr))
      names(depl) <- gsub("Bratio_", "", names(depl))

      # Now, filter out the projected years only
      spr_proj_cols <- spr |>
        select(all_of(as.character(forecast_yrs)))
      depl_proj_cols <- depl |>
        select(all_of(as.character(forecast_yrs)))

      spr_proj_cols <- na.omit(spr_proj_cols)
      depl_proj_cols <- na.omit(depl_proj_cols)

      # Get forecast catches from the forecast.ss files
      case_dir_name <- ct_levels_names[catch_level_ind]
      fore <- SS_readforecast(file.path(fore_path,
                                        case_dir_name,
                                        forecast_fn),
                              verbose = FALSE)
      fore_catch <- fore$ForeCatch |>
        as_tibble() |>
        transmute(year = year, catch = `catch_or_F`) |>
        mutate(catch = ifelse(catch < 1, 0, catch))
      list(depl = apply(depl_proj_cols,
                        2,
                        quantile,
                        probs = probs_forecast,
                        na.rm = TRUE),
           spr = apply(spr_proj_cols,
                       2,
                       quantile,
                       probs = probs_forecast,
                       na.rm = TRUE) |>
             t() |>
             as_tibble(rownames = "yr") |>
             mutate(yr = as.numeric(yr)),
           mcmccalcs = calc_mcmc(mcmc_out),
           outputs = mcmc_out,
           fore_catch = fore_catch)
    })
    message("\n")
    names(lvls_lst) <- ct_levels_names
    lvls_lst
  })
  names(lst) <- forecast_yrs
  message("Finished loading forecasts")

  lst
}
