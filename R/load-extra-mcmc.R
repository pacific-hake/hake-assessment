#' Create and return a list of output stats to attach to the main model
#' by looking in the model's path for the report files.
#'
#' @param model A model list as created by [create_rds_file()]
#' @param probs The quantile values to use on the MCMC posterior data
#' @param small Logical. If `TRUE`, only load a small set of things
#' that are required for the assessment (index fits and catchability
#' estimates) in order to reduce the memory taken up by loading all the
#' output into R for the document build.If `FALSE`, load everything that
#' the base model needs, which will make a huge RDS file
#' @param progress_n Report every time this many files are processed. Consider
#' how many posteriors there are, this should be a fairly large proportion of
#' that (around 1/8th) or there will be too much output and it will run really
#' slow
#' @param verbose Logical. If `TRUE`, show progress messages
#' @param first Load this many of the files. If `NULL`, load them all. Used
#' for debugging purposes to cut down the size of the lists used
#' @param ... Arguments passed to [extract_rep_table()]
#'
#' @return The extra MCMC list
#' @export
load_extra_mcmc <- function(model,
                            probs = c(0.025, 0.5, 0.975),
                            start_yr = NULL,
                            end_yr = NULL,
                            small = TRUE,
                            progress_n = 500,
                            verbose = TRUE,
                            first = NULL,
                            ...){

  if(is.null(model$extra_mcmc_path) || is.na(model$extra_mcmc_path)){
    if(verbose){
      message("`extra_mcmc_path` is NA or NULL, so not attempting to load ",
              "extra mcmc for the model in:\n`",
              model$path, "`\n")
    }
    return(NA)
  }

  if(verbose){
    message("Attempting to load extra MCMC for model in:\n`",
            model$extra_mcmc_path, "`\n")
  }

  if(!dir.exists(model$extra_mcmc_path)){
    if(verbose){
      message("The `", model$extra_mcmc_path,
              "` directory does not exist, so extra mcmc files were not ",
              "loaded for model located in:\n", model$path)
    }
    return(NA)
  }

  if(!exists("reps"))
    reps <-
      load_extra_mcmc_repfiles(
        model$extra_mcmc_path,
        file_pat = "Report_mce_[0-9]+\\.sso$",
        progress_n = progress_n,
        verbose = verbose,
        first = first)

  if(!exists("compreps"))
    compreps <-
      load_extra_mcmc_repfiles(
        model$extra_mcmc_path,
        file_pat = "CompReport_mce_[0-9]+\\.sso$",
        progress_n = progress_n,
        verbose = verbose,
        first = first)

  # For debugging, save these as global
  reps <<- reps
  compreps <<- compreps

  extra_mcmc <- list()

  if(!small){
    # Biomass -----------------------------------------------------------------
    biomass_lst <- load_extra_mcmc_biomass(
      reps = reps,
      probs = probs,
      start_yr = start_yr,
      end_yr = end_yr,
      progress_n = progress_n,
      verbose = verbose,
      beg_pat = "^TIME_SERIES",
      end_pat = "^SPR_SERIES",
      ...)
    extra_mcmc$total_biomass_quants <-
      biomass_lst$total_biomass_quants

    extra_mcmc$total_age2_plus_biomass_quants <-
      biomass_lst$total_age2_plus_biomass_quants

    # Selectivity -------------------------------------------------------------
    sel_fishery_lst <- load_extra_mcmc_sel(
      reps = reps,
      probs = probs,
      start_yr = start_yr,
      end_yr = end_yr - 1,
      progress_n = progress_n,
      verbose = verbose,
      beg_pat = "^COMBINED_ALK",
      end_pat = "^Fecund +NA +1963",
      model$endyr,
      type = "fishery",
      ...)
    extra_mcmc$sel_fishery_lo <- sel_fishery_lst$sel_lo
    extra_mcmc$sel_fishery_med <- sel_fishery_lst$sel_med
    extra_mcmc$sel_fishery_hi <- sel_fishery_lst$sel_hi
    sel_survey_lst <- load_extra_mcmc_sel(
      reps = reps,
      probs = probs,
      start_yr = start_yr,
      end_yr = end_yr - 1,
      progress_n = progress_n,
      verbose = verbose,
      beg_pat = "^COMBINED_ALK",
      end_pat = "^Fecund +NA +1963",
      model$endyr,
      type = "survey",
      ...)
    extra_mcmc$sel_survey_lo <- sel_survey_lst$sel_lo
    extra_mcmc$sel_survey_med <- sel_survey_lst$sel_med
    extra_mcmc$sel_survey_hi <- sel_survey_lst$sel_hi
  }

  # Selectivity * Weight ------------------------------------------------------
  selwt_pat <- paste0(model$endyr + 1, "_1_sel\\*wt")
  selwt_lst <- load_extra_mcmc_selwt(
    reps = reps,
    verbose = verbose,
    head_beg_pat = "^COMBINED_ALK",
    head_end_pat = "^Fecund +NA +1963",
    beg_pat = selwt_pat,
    end_pat = selwt_pat,
    ...)
  extra_mcmc$selwt_med <- selwt_lst$selwt_med

  # Numbers-at-age ------------------------------------------------------------
  natage_lst <- load_extra_mcmc_atage(
    reps = reps,
    verbose = verbose,
    txt = "Numbers-at-age",
    beg_pat = "^NUMBERS_AT_AGE report",
    end_pat = "^BIOMASS_AT_AGE",
    scale = 1e3,
    progress_n = progress_n,
    ...)
  extra_mcmc$natage_med <- natage_lst$med

  if(!small){
    # Biomass-at-age ----------------------------------------------------------
    batage_lst <- load_extra_mcmc_atage(
      reps = reps,
      verbose = verbose,
      txt = "Biomass-at-age",
      beg_pat = "^BIOMASS_AT_AGE",
      end_pat = "^NUMBERS_AT_LENGTH",
      scale = 1e3,
      start_yr = model$startyr,
      progress_n = progress_n,
      ...)
    extra_mcmc$batage_med <- batage_lst$med

    # Catch-at-age in numbers -------------------------------------------------
    catage_lst <- load_extra_mcmc_atage(
      reps = reps,
      verbose = verbose,
      txt = "Catch-at-age",
      beg_pat = "^CATCH_AT_AGE",
      end_pat = "^DISCARD_AT_AGE",
      scale = 1,
      start_yr = model$startyr,
      progress_n = progress_n,
      ...)
    extra_mcmc$catage_med <- catage_lst$med

    # Catch-at-age in biomass -------------------------------------------------
    extra_mcmc$cbatage_med <- load_extra_mcmc_catage_biomass(
      reps,
      catage_lst$atage,
      model$wtatage,
      verbose)

    # Exploitation-rate-at-age ------------------------------------------------
    if(verbose){
      message("Extracting Exploitation-rate-at-age...")
    }
    # Divide each catage value by its corresponding batage value
    expatage <- (select(catage_lst$atage, -c(yr, iter)) /
      as.vector(select(batage_lst$atage, -c(yr, iter)))) |>
      as_tibble() |>
      # Divide every cell by 1,000 to get thousands of tonnes,
      # then multiply by 100 for
      mutate_all(~{.x / 1000 * 100}) |>
      bind_cols(select(catage_lst$atage, yr)) |>
      select(yr, everything())

    extra_mcmc$expatage_med <- expatage |>
      group_by(yr) |>
      summarize_all(median)

  }

  # Apply selectivity to numbers-at-age ---------------------------------------
  if(verbose){
    message("Applying selectivity to numbers-at-age...")
  }

  next_yr <- model$endyr + 1
  natage <- natage_lst$atage |>
    #filter(yr %in% unique(sel_fishery_lst$sel$yr)) |>
    filter(yr == next_yr) |>
    select(-c(yr, iter))

  sel <- sel_fishery_lst$sel |>
    filter(yr == next_yr) |>
    select(-c(yr, iter))

  selwt <- selwt_lst$selwt |>
    filter(yr == next_yr) |>
    select(-c(yr, iter))

  natsel <- natage * sel
  natselwt <- natage * selwt
  extra_mcmc$natsel_prop <- natsel %>%
    mutate(rsum = rowSums(.)) |>
    mutate_at(vars(-rsum), ~{.x / rsum}) |>
    select(-rsum) |>
    as_tibble()

  extra_mcmc$natselwt_prop <- natselwt %>%
    mutate(rsum = rowSums(.)) |>
    mutate_at(vars(-rsum), ~{.x / rsum}) |>
    select(-rsum) |>
    as_tibble()

  # Catchability --------------------------------------------------------------
  if(verbose){
    message("Extracting Survey indices...")
  }
  x <- load_extra_mcmc_get_chunk(reps,
                                 beg_pat = "^INDEX_2",
                                 end_pat = "^INDEX_1")

  # This is just to get the header, x$lst is not used in this function
  # Remove survey name so that conversion to numeric does not produce a ton
  # of warnings
  x$header <- x$header[x$header != "Fleet_name"]
  x$lst <- map(x$lst, ~{
    gsub("(Age1|Acoustic)_Survey", "", .x)
  })
  ts_q <- extract_rep_table(reps_lst = x$lst,
                            header = x$header,
                            verbose = verbose,
                            ...)
  names(ts_q) <- tolower(names(ts_q))

  # CPUE table and values (Q) -------------------------------------------------
  if(verbose){
    message("Extracting index fits and catchabilities...")
  }
  # Separate by fleet, 2 is acoustic survey 2+, 3 is Age-1 survey
  index_table_age2plus <- ts_q |>
    filter(fleet == 2)
  index_table_age1 <- ts_q |>
    filter(fleet == 3)

  extra_mcmc$q_med <- ts_q |>
    mutate(calc_q = as.numeric(calc_q)) |>
    group_by(fleet, yr) |>
    summarize(value = median(calc_q))

  extra_mcmc$q_lo <- ts_q |>
    mutate(calc_q = as.numeric(calc_q)) |>
    group_by(fleet, yr) |>
    summarize(value = quantile(calc_q, probs = probs[1]))

  extra_mcmc$q_hi <- ts_q |>
    mutate(calc_q = as.numeric(calc_q)) |>
    group_by(fleet, yr) |>
    summarize(value = quantile(calc_q, probs = probs[3]))

  extra_mcmc$index_med <- ts_q |>
    mutate(exp = as.numeric(exp)) |>
    group_by(fleet, yr) |>
    summarize(value = median(exp))

  extra_mcmc$index_lo <- ts_q |>
    mutate(exp = as.numeric(exp)) |>
    group_by(fleet, yr) |>
    summarize(value = quantile(exp, probs[1]))

  extra_mcmc$index_hi <- ts_q |>
    mutate(exp = as.numeric(exp)) |>
    group_by(fleet, yr) |>
    summarize(value = quantile(exp, probs[3]))

  q <- ts_q |>
    select(iter, exp, calc_q)
  iter <- unique(q$iter)

  cpue <- q |>
    select(-calc_q) |>
    group_by(iter) |>
    group_nest()
  cpue <- do.call(cbind, cpue$data)
  names(cpue) <- iter

  extra_mcmc$cpue_table <- cpue |>
    as_tibble() |>
    map_df(~{as.numeric(.x)})

  extra_mcmc$q_vector <- index_table_age2plus |>
    group_by(iter) |>
    slice(1) |>
    pull(calc_q) |>
    as.numeric()

  # Was Q_vector_age1
  extra_mcmc$q_age1 <- index_table_age1 |>
    group_by(iter) |>
    slice(1) |>
    pull(calc_q) |>
    as.numeric()

  cpue <- apply(extra_mcmc$cpue_table,
                MARGIN = 1,
                FUN = function(x){quantile(as.numeric(x),
                                           probs = probs)
                })
  extra_mcmc$cpue_lo <- as.numeric(cpue[1, ])
  extra_mcmc$cpue_med <- as.numeric(cpue[2, ])
  extra_mcmc$cpue_hi <- as.numeric(cpue[3, ])

  if(!small){
    # Median and quantiles of expected values and Pearson ---------------------
    if(verbose){
      message("Extracting Age composition tables...")
    }
    x <- load_extra_mcmc_get_chunk(compreps,
                                   beg_pat = "^Composition_Database",
                                   end_pat = "End_comp_data")

    # This is just to get the header, x$lst is not used in this function
    ts <- extract_rep_table(reps_lst = x$lst,
                            header = x$header,
                            verbose = verbose,
                            ...)
    names(ts) <- tolower(names(ts))

    if(verbose){
      message("Calculating Pearson residuals (takes about 5 minutes)...")
    }
    comp <- ts |>
      filter(!is.na(nsamp_adj), nsamp_adj > 0) |>
      select(c(iter, yr, fleet, bin, obs, exp, pearson)) |>
      rename(age = bin)
    extra_mcmc$residuals_fishery <- comp |>
      filter(fleet == 1) |>
      select(-fleet) |>
      group_by(yr, age) |>
      summarize(exp_lo = quantile(exp, probs = probs[1]),
                exp_med = quantile(exp, probs = probs[2]),
                exp_hi = quantile(exp, probs = probs[3]),
                obs_lo = quantile(obs, probs = probs[1]),
                obs_med = quantile(obs, probs = probs[2]),
                obs_hi = quantile(obs, probs = probs[3]),
                pearson_lo = quantile(pearson, probs = probs[1]),
                pearson_med = quantile(pearson, probs = probs[2]),
                pearson_hi = quantile(pearson, probs = probs[3])) |>
      ungroup()
    extra_mcmc$residuals_fishery_med <- extra_mcmc$residuals_fishery |>
      select(yr,
             age,
             obs = obs_med,
             exp = exp_med,
             pearson = pearson_med)

    extra_mcmc$residuals_survey <- comp |>
      filter(fleet == 2) |>
      select(-fleet) |>
      group_by(yr, age) |>
      summarize(exp_lo = quantile(exp, probs = probs[1]),
                exp_med = quantile(exp, probs = probs[2]),
                exp_hi = quantile(exp, probs = probs[3]),
                obs_lo = quantile(obs, probs = probs[1]),
                obs_med = quantile(obs, probs = probs[2]),
                obs_hi = quantile(obs, probs = probs[3]),
                pearson_lo = quantile(pearson, probs = probs[1]),
                pearson_med = quantile(pearson, probs = probs[2]),
                pearson_hi = quantile(pearson, probs = probs[3])) |>
      ungroup()

    extra_mcmc$residuals_survey_median <- extra_mcmc$residuals_survey |>
      select(yr,
             age,
             obs = obs_med,
             exp = exp_med,
             pearson = pearson_med)
  }

  if(verbose){
    message("\nFinished loading Extra MCMC output")
  }

  if(small){
    # Frees a load of memory up
    extra_mcmc$natage <- NULL
    extra_mcmc$natage_median <- NULL
    extra_mcmc$natsel_prop <- NULL
    extra_mcmc$natselwt_prop <- NULL
  }

  if(verbose){
    message("Extra MCMC size = ",
            f(as.numeric(object.size(extra_mcmc) / 1e6), 2),
            "MB\n")
  }

  extra_mcmc
}
