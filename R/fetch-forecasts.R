#' Fetch the output from previously-run forecasting using [run_forecasts()]
#'
#' @details If the forecasts directory does not exist or there is a problem
#' loading the forecasts, return `NA`
#'
#' @param forecasts_path Path of the forecasts
#' @param my_catch_levels The table of catch levels with same structure as
#' found in `forecast-catch-levels.R`
#' @param forecast_yrs A vector of the forecast years
#' @param ...
#'
#' @return A list of forecast outputs as read in by [r4ss::SSgetMCMC()]
#' @export
fetch_forecasts <- function(forecasts_path = NULL,
                            my_catch_levels = NULL,
                            forecast_yrs = NULL,
                            ...){

  if(is.null(forecasts_path[1])){
    stop("`forecasts_path` cannot be `NULL`",
         call. = FALSE)
  }
  if(!dir.exists(forecasts_path)){
    return(NA)
  }
  message("\nLoading forecast data from ", forecasts_path, "\n")

  if(is.null(my_catch_levels[1])){
    stop("`my_catch_levels` cannot be `NULL`",
         call. = FALSE)
  }
  if(is.null(forecast_yrs[1])){
    stop("`forecast_yrs` cannot be `NULL`",
         call. = FALSE)
  }

  # Extract the catch level names from the list into a vector
  catch_levels_catch <- map(my_catch_levels, ~{.x[[1]]})
  catch_levels_desc <- map_chr(my_catch_levels, ~{.x[[2]]})
  catch_levels_names <- map_chr(my_catch_levels, ~{.x[[3]]})

  # Get the directory listing and choose the last one for loading
  dir_listing <- dir(forecasts_path)

  lst <- map(forecast_yrs, function(fore_yr){
    fore_path <- file.path(forecasts_path, paste0("forecast-year-", fore_yr))
    if(!dir.exists(fore_path)){
      stop("Directory `fore_path` = ", fore_path, " does not exist",
           call. = FALSE)
    }
    # Get the directory listing of the forecast directory and make sure
    # it matches what the catch levels are.currently set to in
    # forecast-catch-levels.R`
    dir_listing <- dir(fore_path)
    if(!identical(catch_levels_names, dir_listing)){
      stop("There is a discrepancy between what you have set ",
           "for the catch_levels_names \n and what appears in the forecasts ",
           "directory '", fore_path,"'. \n Check the names in both and try ",
           "again.\n\n",
           call. = FALSE)
    }

    lvls_lst <- imap(my_catch_levels, function(catch_level, catch_level_ind){
      fore_level_path <- file.path(fore_path, catch_level[[3]])
      message("Loading from ", fore_level_path)

      mcmc_out <- SSgetMCMC(dir = fore_level_path,
                            writecsv = FALSE,
                            verbose = FALSE)

      # Get the values of interest, namely Spawning biomass and SPR for the two
      # decision tables in the executive summary
      sb <- mcmc_out %>% select(grep("Bratio_", names(.)))
      spr <- mcmc_out %>% select(grep("SPRratio_", names(.)))

      # Strip out the Bratio_ and SPRratio_ headers so columns are years only
      names(sb) <- gsub("Bratio_", "", names(sb))
      names(spr) <- gsub("SPRratio_", "", names(spr))

      # Now, filter out the projected years only
      sb_proj_cols <- sb |>
        select(one_of(as.character(forecast_yrs)))
      spr_proj_cols <- spr |>
        select(one_of(as.character(forecast_yrs)))
      sb_proj_cols <- na.omit(sb_proj_cols)
      spr_proj_cols <- na.omit(spr_proj_cols)
      # Get forecast catches from the forecast.ss files
      case_dir_name <- catch_levels_names[catch_level_ind]
      fore <- r4ss::SS_readforecast(file.path(fore_path,
                                              case_dir_name,
                                              "forecast.ss"),
                                    verbose = FALSE)
      fore_catch <- fore$ForeCatch |>
        as_tibble() |>
        transmute(year = Year, catch = `Catch or F`) |>
        mutate(catch = ifelse(catch < 1, 0, catch))


      list(biomass = t(apply(sb_proj_cols, 2, quantile, probs = forecast_probs, na.rm = TRUE)),
           spr = t(apply(spr_proj_cols, 2, quantile, probs = forecast_probs, na.rm = TRUE)),
           mcmccalcs = calc.mcmc(mcmc_out),
           #outputs = mcmc_out,
           fore_catch = fore_catch)
    })
    names(lvls_lst) <- catch_levels_names
    lvls_lst
  })
  #plan()
  names(lst) <- forecast_yrs
  message("Finished loading forecasts")
  lst
}
