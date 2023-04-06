#' Fetch the output from previously-run forecasting using [run_forecasts()]
#'
#' @details If the forecasts directory does not exist or there is a problem
#' loading the forecasts, return `NA`
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param first Load this many of the files. If `NULL`, load them all. Used
#' for debugging purposes to cut down the size of the lists used
#' @param forecast_probs A vector of probabilities to use for calculations
#' on forecast outputs
#' @param ... Absorbs arguments intended for other functions
#'
#' @return A list of forecast outputs as read in by [r4ss::SSgetMCMC()]
#' @export
load_forecasts <- function(model,
                           first = model$nposts,
                           forecast_probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                           ...){

  if(!dir.exists(model$forecasts_path)){
    return(NA)
  }
  message("\nLoading forecast data from ", model$forecasts_path, "\n")

  if(is.na(model$ct_levels[1])){
    stop("`model$ct_levels` cannot be `NA`. There was likely a problem ",
         "with finding the path for the ct_levels outputs",
         call. = FALSE)
  }

  # Extract the catch level names from the list into a vector
  ct_levels_catch <- map(model$ct_levels, ~{.x[[1]]})
  ct_levels_desc <- map_chr(model$ct_levels, ~{.x[[2]]})
  ct_levels_names <- map_chr(model$ct_levels, ~{.x[[3]]})

  # Get the directory listing and choose the last one for loading
  dir_listing <- dir(model$forecasts_path)

  # Make sure only year directories exist here
  num_forecasts <- length(grep("^20[0-9]{2}$", dir_listing))
  if(num_forecasts != model$nforecastyears){
    stop("The number of actual forecast years run (", num_forecasts, ") ",
         "does not match the number of forecast years specified in the ",
         "model (", model$nforecastyears, "). Check the contents of the ",
         "directory `", model$forecasts_path, "`",
         call. = FALSE)
  }
  forecast_yrs <- dir_listing
  lst <- map(forecast_yrs, function(fore_yr){
    fore_path <- file.path(model$forecasts_path, fore_yr)
    if(!dir.exists(fore_path)){
      stop("Directory `", fore_path, "` does not exist",
           call. = FALSE)
    }
    # Get the directory listing of the forecast directory and make sure
    # it matches what the catch levels are.currently set to in
    # forecast-catch-levels.R`
    dir_listing <- dir(fore_path)
    if(!all(ct_levels_names %in% dir_listing)){
      stop("There is one or more missing catch level directories in the ",
           "forecasts directory:\n`", fore_path,"`\nCheck the names set up ",
           "in `set_catch_levels()` and compare to what is in the directory.",
           "It is ok to have extra forecasts present in the forecast ",
           "directory, but all forecast catch levels appearing in ",
           "`set_catch_levels()` must be present in the directory",
           call. = FALSE)
    }
    # Eliminate extra forecast runs, possibly done after the document was
    # created, for the JMC meeting or some other reason
    dir_listing <- ct_levels_names[dir_listing %in% ct_levels_names]
    lvls_lst <- imap(model$ct_levels, \(catch_level, catch_level_ind){
      fore_level_path <- file.path(fore_path, catch_level[[3]])
      message("Loading from ", fore_level_path)

      mcmc_out <- SSgetMCMC(dir = fore_level_path,
                            writecsv = FALSE,
                            verbose = FALSE) |>
        slice_head(n = first)

      # Get the values of interest, namely Spawning biomass and SPR for the two
      # decision tables in the executive summary
      sb <- mcmc_out %>%
        select(grep("Bratio_", names(.)))
      spr <- mcmc_out %>%
        select(grep("SPRratio_", names(.)))

      # Strip out the Bratio_ and SPRratio_ headers so columns are years only
      names(sb) <- gsub("Bratio_", "", names(sb))
      names(spr) <- gsub("SPRratio_", "", names(spr))

      # Now, filter out the projected years only
      sb_proj_cols <- sb |>
        select(all_of(forecast_yrs))
      spr_proj_cols <- spr |>
        select(all_of(forecast_yrs))
      sb_proj_cols <- na.omit(sb_proj_cols)
      spr_proj_cols <- na.omit(spr_proj_cols)
      # Get forecast catches from the forecast.ss files
      case_dir_name <- ct_levels_names[catch_level_ind]
      fore <- SS_readforecast(file.path(fore_path,
                                        case_dir_name,
                                        model$forecast_fn),
                              verbose = FALSE)
      fore_catch <- fore$ForeCatch |>
        as_tibble() |>
        transmute(year = Year, catch = `Catch or F`) |>
        mutate(catch = ifelse(catch < 1, 0, catch))

      list(biomass = apply(sb_proj_cols,
                           2,
                           quantile,
                           probs = forecast_probs,
                           na.rm = TRUE) |>
             t() |>
             as_tibble(rownames = "yr") |>
             mutate(yr = as.numeric(yr)),
           spr = apply(spr_proj_cols,
                       2,
                       quantile,
                       probs = forecast_probs,
                       na.rm = TRUE) |>
             t() |>
             as_tibble(rownames = "yr") |>
             mutate(yr = as.numeric(yr)),
           mcmccalcs = calc_mcmc(mcmc_out),
           outputs = mcmc_out,
           fore_catch = fore_catch)
    })
    names(lvls_lst) <- ct_levels_names
    lvls_lst
  })
  names(lst) <- forecast_yrs
  message("Finished loading forecasts")

  lst
}
