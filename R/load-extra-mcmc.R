#' Create and return a list of output stats to attach to the main model
#' by looking in the model's path for the report files.
#'
#' @param model A model list as created by [load_ss_files()]
#' @param progress_n Report every time this many files are processed. Consider
#' how many posteriors there are, this should be a fairly large proportion of
#' that (around 1/8th) or there will be too much output and it will run slow
#' @param verbose Logical. If `TRUE`, show progress messages
#' @param first Load this many of the files. If a non-positive number, load
#' them all. Used for debugging purposes to cut down the size of the
#' lists used
#' @param ... Arguments passed to [extract_rep_table()]
#'
#' @return The extra MCMC list
#' @export
load_extra_mcmc <- function(model,
                            progress_n = 500,
                            verbose = TRUE,
                            first = 0,
                            ...){

  if(is.null(model$extra_mcmc_path) || is.na(model$extra_mcmc_path)){
    if(verbose){
      message("`extra_mcmc_path` is NA or NULL, so not attempting to load ",
              "extra mcmc for the model in:\n`",
              model$path, "`\n")
    }
    return(NA)
  }

  if(!dir.exists(model$extra_mcmc_path)){
    if(verbose){
      message("The `", model$extra_mcmc_path,
              "` directory does not exist, so extra mcmc files were not ",
              "loaded for model located in:\n", model$path, "\n\n")
    }
    return(NA)
  }

  if(first <= 0){
    first <- model$nposts
    if(is.null(first)){
      first <- model$mcmc |> nrow()
    }
  }
  if(!exists("reps") || (exists("reps") && length(reps) != first)){
    if(verbose){
      message("Loading Report files")
    }
    reps <-
      load_extra_mcmc_repfiles(
        model$extra_mcmc_path,
        file_pat = regex_extra_mcmc_report,
        progress_n = progress_n,
        verbose = verbose,
        first = first)
  }
  if(!exists("compreps") || (exists("compreps") && length(compreps) != first)){
    if(verbose){
      message("Loading CompReport files")
    }
    compreps <-
      load_extra_mcmc_repfiles(
        model$extra_mcmc_path,
        file_pat = regex_extra_mcmc_compreport,
        progress_n = progress_n,
        verbose = verbose,
        first = first)
  }
  # For debugging only, uncomment these lines to save these as global.
  # If they exist, the loading above will not happen
  reps <<- reps
  compreps <<- compreps

  extra_mcmc <- list()

  # Age compositions -----------------------------------------------------------
  # `extra_mcmc$age_comps` is a data frame with columns "iter", "yr", "fleet",
  # and 15 columns for the age comps estimated in the model labeled as the age
  extra_mcmc$age_comps <- load_extra_mcmc_age_comps(
    compreps = compreps,
    start_yr = model$startyr,
    end_yr = model$endyr + 1,
    progress_n = progress_n,
    verbose = verbose,
    beg_pat = "^Composition_Database",
    end_pat = "End_comp_data",
    ...)

  # Initial numbers-at-age -----------------------------------------------------
  ages <- model$natage |> names() |> as.numeric() |> suppressWarnings()
  ages <- ages[!is.na(ages)]
  max_age <- max(ages)

  extra_mcmc$init_natage <- load_extra_mcmc_init_nage(
    reps = reps,
    start_yr = model$startyr,
    end_yr = model$endyr + 1,
    progress_n = progress_n,
    verbose = verbose,
    beg_pat = paste0("^\\d+\\s*Early_InitAge_", max_age),
    end_pat = "^\\d+\\s*Early_InitAge_1\\s+",
    ...)

  # Recruitment deviatons ------------------------------------------------------
  ages <- model$natage |> names() |> as.numeric() |> suppressWarnings()
  ages <- ages[!is.na(ages)]
  max_age <- max(ages)
  # Test endyr is last year
  available_late_years <- gsub(
    pattern = ".+Late_RecrDev_([0-9]{4}) .+",
    replacement = "\\1",
    x = grep("Late_RecrDev", reps[[1]], value = TRUE))

  extra_mcmc$recr_devs <- load_extra_mcmc_recr_devs(
    reps = reps,
    start_yr = model$startyr,
    end_yr = model$endyr,
    progress_n = progress_n,
    verbose = verbose,
    beg_pat = paste0("^\\d+\\s+Early_InitAge_", max_age),
    end_pat = paste0("^\\d+\\s+Late_RecrDev_", tail(available_late_years, 1)),
    ...)

  # Cohort recruitments --------------------------------------------------------
  extra_mcmc$recr_cohorts <- load_extra_mcmc_recr_cohorts(
    reps = reps,
    cohorts = large_cohorts,
    start_yr = model$startyr,
    end_yr = model$endyr + 1,
    progress_n = progress_n,
    verbose = verbose,
    beg_pat = "^Recr_Initial",
    end_pat = "^SPRratio_[0-9]{1,4}",
    ...)

  # Biomass --------------------------------------------------------------------
  biomass_lst <- load_extra_mcmc_biomass(
    reps = reps,
    start_yr = model$startyr,
    end_yr = model$endyr + 1,
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
    start_yr = model$startyr,
    end_yr = model$endyr + 1,
    progress_n = progress_n,
    verbose = verbose,
    beg_pat = "^COMBINED_ALK",
    end_pat = "^Fecund +NA +[0-9]{1,4}",
    model$endyr,
    type = "fishery",
    ...)
  extra_mcmc$sel_fishery_lo <- sel_fishery_lst$sel_lo
  extra_mcmc$sel_fishery_med <- sel_fishery_lst$sel_med
  extra_mcmc$sel_fishery_hi <- sel_fishery_lst$sel_hi
  extra_mcmc$sel_fishery_end_yr <- sel_fishery_lst$sel_end_yr
  sel_survey_lst <- load_extra_mcmc_sel(
    reps = reps,
    start_yr = model$startyr,
    end_yr = model$endyr,
    progress_n = progress_n,
    verbose = verbose,
    beg_pat = "^COMBINED_ALK",
    end_pat = "^Fecund +NA +[0-9]{1,4}",
    model$endyr,
    type = "survey",
    ...)
  extra_mcmc$sel_survey_lo <- sel_survey_lst$sel_lo
  extra_mcmc$sel_survey_med <- sel_survey_lst$sel_med
  extra_mcmc$sel_survey_hi <- sel_survey_lst$sel_hi
  extra_mcmc$sel_survey_end_yr <- sel_survey_lst$sel_end_yr

  # Selectivity * Weight ------------------------------------------------------
  # AKA vulnerable biomass ----------------------------------------------------
  selwt_pat <- paste0(model$endyr + 1, "_1_sel\\*wt")
  selwt_lst <- load_extra_mcmc_selwt(
    reps = reps,
    verbose = verbose,
    head_beg_pat = "^COMBINED_ALK",
    head_end_pat = "^Fecund +NA +[0-9]{1,4}",
    beg_pat = selwt_pat,
    end_pat = selwt_pat,
    ...)
  if(is.na(selwt_lst[1])){
    extra_mcmc$selwt_med <- NA
  }else{
    extra_mcmc$selwt_med <- selwt_lst$selwt_med
  }

  # Numbers-at-age ------------------------------------------------------------
  natage_lst <- load_extra_mcmc_atage(
    reps = reps,
    verbose = verbose,
    start_yr = model$startyr,
    end_yr = model$endyr + 1,
    txt = "numbers-at-age",
    beg_pat = "^NUMBERS_AT_AGE report",
    end_pat = "^BIOMASS_AT_AGE",
    scale = 1e3,
    progress_n = progress_n,
    ...)
  extra_mcmc$natage_med <- natage_lst$med

  # Biomass-at-age ----------------------------------------------------------
  batage_lst <- load_extra_mcmc_atage(
    reps = reps,
    verbose = verbose,
    start_yr = model$startyr,
    end_yr = model$endyr + 1,
    txt = "biomass-at-age",
    beg_pat = "^BIOMASS_AT_AGE",
    end_pat = "^NUMBERS_AT_LENGTH",
    scale = 1e3,
    progress_n = progress_n,
    ...)
  extra_mcmc$batage_med <- batage_lst$med

  # Catch-at-age in numbers -------------------------------------------------
  catage_lst <- load_extra_mcmc_atage(
    reps = reps,
    verbose = verbose,
    start_yr = model$startyr,
    end_yr = model$endyr,
    txt = "catch-at-age",
    beg_pat = "^CATCH_AT_AGE",
    end_pat = "^DISCARD_AT_AGE",
    scale = 1,
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
    message("Extracting exploitation-rate-at-age...")
  }
  # Divide each catage value by its corresponding batage value
  batage_len_catage <- batage_lst$atage |>
    dplyr::filter(yr %in% catage_lst$atage$yr)
  expatage <- (select(catage_lst$atage, -c(yr, iter)) /
                 as.vector(select(batage_len_catage, -c(yr, iter)))) |>
    as_tibble() |>
    # Divide every cell by 1,000 to get thousands of tonnes,
    # then multiply by 100 for
    mutate_all(~{.x / 1000 * 100}) |>
    bind_cols(select(catage_lst$atage, yr)) |>
    select(yr, everything())

  extra_mcmc$expatage_med <- expatage |>
    group_by(yr) |>
    summarize_all(median)

  # Apply selectivity to numbers-at-age ---------------------------------------
  if(verbose){
    message("Extracting selectivity with numbers-at-age...")
  }

  natage <- natage_lst$atage |>
    dplyr::filter(yr == model$endyr + 1) |>
    select(-c(yr, iter))

  sel <- sel_fishery_lst$sel |>
    dplyr::filter(yr == model$endyr + 1) |>
    select(-c(yr))

  natsel <- natage * sel
  extra_mcmc$natsel_prop <- natsel %>%
    mutate(rsum = rowSums(.)) |>
    mutate_at(vars(-rsum), ~{.x / rsum}) |>
    select(-rsum) |>
    as_tibble()

  if(is.na(selwt_lst[1])){
    selwt <- NA
    natselwt <- NA
    extra_mcmc$natselwt_prop <- NA
  }else{
    selwt <- selwt_lst$selwt |>
      select(-c(yr, iter))
    if(nrow(natage) != nrow(selwt)){
      # Can happen if not all posteriors complete, like in 2023 for
      # sensitivity model 17 (hamel prior)
      new_nrow <- min(nrow(natage), nrow(selwt))
      diff <- abs(nrow(natage) - nrow(selwt))
      if(new_nrow == nrow(natage)){
        # Re-size selwt
        selwt <- head(selwt, -diff)
      }else{
        # Re-size natage
        natage <- head(selwt, -diff)
      }
    }
    natselwt <- natage * selwt
    extra_mcmc$natselwt_prop <- natselwt %>%
      mutate(rsum = rowSums(.)) |>
      mutate_at(vars(-rsum), ~{.x / rsum}) |>
      select(-rsum) |>
      as_tibble()
  }

  # eDNA Q and ExtraSD values --------------------------------------------------
  if(verbose){
    message("Extracting eDNA Q and ExtraSD values ...")
  }
  # Need the headers, so extract from PARAMETERS to get headers, then remove
  # all rows except for the two values we want
  x <- load_extra_mcmc_get_chunk(reps,
                                 beg_pat = "^PARAMETERS report",
                                 end_pat = "AgeSel_P1_Fishery")

  lnq_edna <-  map(x$lst, ~{grep("LnQ_base_eDNA_Survey",
                                 .x, value = TRUE)})

  q_extrasd_edna <-  map(x$lst, ~{grep("Q_extraSD_eDNA_Survey",
                                       .x, value = TRUE)})

  if(all(lengths(lnq_edna))){
    lnq_edna <- suppressWarnings(extract_rep_table(reps_lst = lnq_edna,
                                                   header = x$header,
                                                   verbose = verbose,
                                                   ...))

    names(lnq_edna) <- tolower(names(lnq_edna))

    lnq_edna_quants <- lnq_edna$value |> quantile(probs = probs)

    extra_mcmc$lnq_edna_lo <- lnq_edna_quants[1]
    extra_mcmc$lnq_edna_med <- lnq_edna_quants[2]
    extra_mcmc$lnq_edna_hi <- lnq_edna_quants[3]
  }

  if(all(lengths(q_extrasd_edna))){
    q_extrasd_edna <- suppressWarnings(extract_rep_table(reps_lst = q_extrasd_edna,
                                                         header = x$header,
                                                         verbose = verbose,
                                                         ...))

    names(q_extrasd_edna) <- tolower(names(q_extrasd_edna))

    q_extrasd_edna_quants <- q_extrasd_edna$value |> quantile(probs = probs)

    extra_mcmc$q_extrasd_edna_lo <- q_extrasd_edna_quants[1]
    extra_mcmc$q_extrasd_edna_med <- q_extrasd_edna_quants[2]
    extra_mcmc$q_extrasd_edna_hi <- q_extrasd_edna_quants[3]
  }

  # Catchability --------------------------------------------------------------
  if(verbose){
    message("Extracting survey indices...")
  }
  x <- load_extra_mcmc_get_chunk(reps,
                                 beg_pat = "^INDEX_2",
                                 end_pat = "^INDEX_1")

  # This is just to get the header, x$lst is not used in this function
  # Remove survey name so that conversion to numeric does not produce a ton
  # of warnings
  x$header <- x$header[x$header != "Fleet_name"]
  x$lst <- map(x$lst, ~{
    gsub("(Age1|Acoustic|eDNA)_Survey", "", .x)
  })

  ts_q <- extract_rep_table(reps_lst = x$lst,
                            header = x$header,
                            verbose = verbose,
                            ...)
  names(ts_q) <- tolower(names(ts_q))

  extra_mcmc$num_posts <- length(unique(ts_q$iter))

  # Index fits and catchability (q) -------------------------------------------------
  if(verbose){
    message("Extracting index fits and catchabilities...")
  }

  extra_mcmc$q_med <- ts_q |>
    mutate(calc_q = as.numeric(calc_q)) |>
    group_by(fleet, yr) |>
    summarize(value = median(calc_q)) |>
    ungroup()

  extra_mcmc$q_lo <- ts_q |>
    mutate(calc_q = as.numeric(calc_q)) |>
    group_by(fleet, yr) |>
    summarize(value = quantile(calc_q, probs = probs[1])) |>
    ungroup()

  extra_mcmc$q_hi <- ts_q |>
    mutate(calc_q = as.numeric(calc_q)) |>
    group_by(fleet, yr) |>
    summarize(value = quantile(calc_q, probs = probs[3])) |>
    ungroup()

  extra_mcmc$index_lo <- ts_q |>
    mutate(exp = as.numeric(exp)) |>
    group_by(fleet, yr) |>
    summarize(value = quantile(exp, probs[1])) |>
    ungroup() |>
    mutate(across(-c(yr, fleet), ~{.x <- .x / 1e6}))

  extra_mcmc$index_med <- ts_q |>
    mutate(exp = as.numeric(exp)) |>
    group_by(fleet, yr) |>
    summarize(value = median(exp)) |>
    ungroup() |>
    mutate(across(-c(yr, fleet), ~{.x <- .x / 1e6}))

  extra_mcmc$index_hi <- ts_q |>
    mutate(exp = as.numeric(exp)) |>
    group_by(fleet, yr) |>
    summarize(value = quantile(exp, probs[3])) |>
    ungroup() |>
    mutate(across(-c(yr, fleet), ~{.x <- .x / 1e6}))

  extra_mcmc$q_vector <- ts_q |>
    dplyr::filter(fleet == 2) |>
    select(iter, calc_q) |>
    group_by(iter) |>
    slice(1) |>
    pull(calc_q) |>
    as.numeric()

  extra_mcmc$q_vector_age1 <- ts_q |>
    dplyr::filter(fleet == 3) |>
    select(iter, calc_q) |>
    group_by(iter) |>
    slice(1) |>
    pull(calc_q) |>
    as.numeric()

  extra_mcmc$q_vector_edna <- ts_q |>
    dplyr::filter(fleet == 4) |>
    select(iter, calc_q) |>
    group_by(iter) |>
    slice(1) |>
    pull(calc_q) |>
    as.numeric()

  # `extra_mcmc$index_fit_posts` is needed for the survey fit plot with
  # many individual MCMC posteriors. It should be deleted after, inside the
  # `create_rds_file()` function
  iter <- unique(ts_q$iter)
  fleets <- unique(ts_q$fleet)
  index_fit_posts <- ts_q |>
    select(iter, yr, fleet, exp)
  yr_df_lst <- index_fit_posts |>
    select(yr, fleet) |>
    split(~fleet) |>
    map(~{.x |>
        select(-fleet) |>
        distinct()})

  index_fit_df_lst <- index_fit_posts |>
    split(~fleet) |>
    map(~{
      .x <- .x |>
        select(-c(yr, fleet)) |>
        group_by(iter) |>
        group_nest()
      do.call(cbind, .x$data)
    })

  extra_mcmc$index_fit_posts <- map2(yr_df_lst, index_fit_df_lst, ~{
    .y <- .y |>
      set_names(iter)
    # The `.name_repair` bit below silences the New names... messages
    bind_cols_quiet(.x, .y)
  }) |>
    map2_df(fleets, ~{
      .x |>
        mutate(fleet = .y) |>
        select(yr, fleet, everything())
    }) |>
    mutate(across(-c(yr, fleet), ~{.x <- .x / 1e6}))

  # Median and quantiles of expected values and Pearson ---------------------
  if(verbose){
    message("Extracting age composition tables...")
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
    message("Calculating pearson residuals...")
  }
  comp <- ts |>
    dplyr::filter(!is.na(nsamp_adj), nsamp_adj > 0) |>
    select(c(iter, yr, fleet, bin, obs, exp, pearson)) |>
    rename(age = bin)

  extra_mcmc$residuals_fishery <- comp |>
    dplyr::filter(fleet == 1) |>
    select(-fleet) |>
    group_by(yr, age) |>
    summarize(exp_lo = quantile(exp, probs = probs[1]),
              exp_med = quantile(exp, probs = probs[2]),
              exp_hi = quantile(exp, probs = probs[3]),
              obs_med = quantile(obs, probs = probs[2]),
              pearson_lo = quantile(pearson, probs = probs[1]),
              pearson_med = quantile(pearson, probs = probs[2]),
              pearson_hi = quantile(pearson, probs = probs[3])) |>
    ungroup()

  extra_mcmc$residuals_survey <- comp |>
    dplyr::filter(fleet == 2) |>
    select(-fleet) |>
    group_by(yr, age) |>
    summarize(exp_lo = quantile(exp, probs = probs[1]),
              exp_med = quantile(exp, probs = probs[2]),
              exp_hi = quantile(exp, probs = probs[3]),
              obs_med = quantile(obs, probs = probs[2]),
              pearson_lo = quantile(pearson, probs = probs[1]),
              pearson_med = quantile(pearson, probs = probs[2]),
              pearson_hi = quantile(pearson, probs = probs[3])) |>
    ungroup()

  if(verbose){
    message("\nFinished loading Extra MCMC output")
  }

  if(verbose){
    message("Extra MCMC size = ",
            f(as.numeric(object.size(extra_mcmc) / 1e6), 2),
            "MB\n")
  }

  extra_mcmc
}
