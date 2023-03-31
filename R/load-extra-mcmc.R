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
    load_extra_mcmc_repfiles(model$extra_mcmc_path,
                             file_pat = "Report_mce_[0-9]+\\.sso$",
                             progress_n = progress_n,
                             verbose = verbose,
                             first = first)

  if(!exists("compreps"))
    compreps <-
    load_extra_mcmc_repfiles(model$extra_mcmc_path,
                             file_pat = "CompReport_mce_[0-9]+\\.sso$",
                             progress_n = progress_n,
                             verbose = verbose,
                             first = first)

  # For debugging, save these as global
  reps <<- reps
  compreps <<-compreps

  # Use the first report file as a template for the regular expressions used
  # to find the locations of the tings we need to extract (speeds up the
  # map2() calls comping later)
  rep_example <- reps[[which(!is.na(reps))[1]]]
  comprep_example <- compreps[[which(!is.na(compreps))[1]]]

  posteriors_fn <- file.path(model$extra_mcmc_path, posts_file_name)
  if(!file.exists(posteriors_fn)){
    if(verbose){
      message("File `", posteriors_fn, "` does not exist.\n")
    }
    return(NA)
  }
  if(verbose){
    message("Loading posteriors file:\n`", posteriors_fn, "`")
  }
  # Suppress warnings because there is an extra whitespace at the end of
  # the header line in the file.
  suppressWarnings(
    posts <- read_table2(posteriors_fn, col_types = cols())
  )
  # Remove extra MLE run outputs. SS appends a new header followed by a
  # 0-Iter row for an MLE run. Sometimes MLEs are run by accident or on
  # purpose at another time and forgotten about, and left in the file.
  posts <- posts |>
    filter(Iter != "Iter", Iter != 0)

  extra_mcmc <- list()

  if(!small){
    # Biomass -----------------------------------------------------------------
    b <- load_extra_mcmc_biomass(reps = reps,
                                 probs = probs,
                                 progress_n = progress_n,
                                 verbose = verbose,
                                 beg_pat = "^TIME_SERIES",
                                 end_pat = "^SPR_SERIES",
                                 ...)
    extra_mcmc$total_biomass_quants <- b$total_biomass_quants
    extra_mcmc$total_age2_plus_biomass_quants <-
      b$total_age2_plus_biomass_quants

    # Selectivity -------------------------------------------------------------
    sel_fishery_lst <- load_extra_mcmc_sel(
      reps = reps,
      probs = probs,
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
    extra_mcmc$sel_fishery_endyr <- sel_fishery_lst$sel_endyr
    sel_survey_lst <- load_extra_mcmc_sel(
      reps = reps,
      probs = probs,
      progress_n = progress_n,
      verbose = verbose,
      beg_pat = "^COMBINED_ALK",
      end_pat = "^Fecund ",
      model$endyr,
      type = "survey",
      ...)
    extra_mcmc$sel_survey_lo <- sel_survey_lst$sel_lo
    extra_mcmc$sel_survey_med <- sel_survey_lst$sel_med
    extra_mcmc$sel_survey_hi <- sel_survey_lst$sel_hi
    extra_mcmc$sel_survey_endyr <- sel_survey_lst$sel_endyr

    browser()
  }
# HERE
  # Selectivity * Weight ------------------------------------------------------
  extra_mcmc$selwt <- load_extra_mcmc_vuln(
    reps,
    probs,
    progress_n,
    verbose,
    model$endyr,
    ...)

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
browser()
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
    # Divide catch-at-age-biomass by 1000 to make the same units, multiply by
    # 100 to make percentage
    if(verbose){
      message("Extracting Exploitation-rate-at-age...")
    }
    browser()
    # Divide each catcge value by its corresponding batage value
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
  # Catchability --------------------------------------------------------------
  if(verbose){
    message("Extracting Survey indices...")
  }
  q_header_ind <- grep("^INDEX_2", rep_example) + 1
  q_header <- str_split(rep_example[q_header_ind], " +")[[1]]
  q_start_ind <- q_header_ind + 1
  ncpue <- nrow(model$dat$CPUE)
  q_end_ind <- q_start_ind + ncpue - 1
  reps_q <- map(reps, ~{.x[q_start_ind:q_end_ind]})

  # Composition tables --------------------------------------------------------
  if(verbose){
    message("Extracting Age composition tables...")
  }
  comp_header_ind <- grep("^Composition_Database", comprep_example) + 1
  comp_header <- str_split(comprep_example[comp_header_ind], " +")[[1]]
  comp_start_ind <- comp_header_ind + 1
  comp_end_ind <- grep("End_comp_data", comprep_example) - 1
  compreps <- compreps[!is.na(compreps)]
  reps_comp <- map(compreps, ~{.x[comp_start_ind:comp_end_ind]})

  # Apply selectivity to numbers-at-age ---------------------------------------
  # TODO: hack of subtracting 1 - See issue #859
  if(verbose){
    message("Applying selectivity to numbers-at-age...")
  }

  next_yr <- model$endyr + 1
  natage <- natage |>
    filter(Yr == next_yr) |>
    select(-Yr)

  natsel <- natage * sel
  natselwt <- natage * selwt
  extra_mcmc$natsel_prop <- natsel %>%
    mutate(rsum = rowSums(.)) |>
    mutate_at(vars(-rsum), ~{.x / rsum}) |>
    select(-rsum)

  extra_mcmc$natselwt_prop <- natselwt %>%
    mutate(rsum = rowSums(.)) |>
    mutate_at(vars(-rsum), ~{.x / rsum}) |>
    select(-rsum)

  # CPUE table and values (Q) -------------------------------------------------
  if(verbose){
    message("Extracting index fits and catchabilities...")
  }
  index_table <- extract_rep_table(reps_q,
                                   q_header,
                                   verbose = verbose,
                                   ...)
  # Separate by fleet, 2 is acoustic survey 2+, 3 is Age-1 survey
  index_table_age2plus <- index_table |>
    filter(Fleet == 2)
  index_table_age1 <- index_table |>
    filter(Fleet == 3)

  extra_mcmc$q_med <- index_table |>
    mutate(Calc_Q = as.numeric(Calc_Q)) |>
    group_by(Fleet, Yr) |>
    summarize(value = median(Calc_Q), .groups = )

  extra_mcmc$q_lo <- index_table |>
    mutate(Calc_Q = as.numeric(Calc_Q)) |>
    group_by(Fleet, Yr) |>
    summarize(value = quantile(Calc_Q, probs = probs[1]))

  extra_mcmc$q_hi <- index_table |>
    mutate(Calc_Q = as.numeric(Calc_Q)) |>
    group_by(Fleet, Yr) |>
    summarize(value = quantile(Calc_Q, probs = probs[3]))

  extra_mcmc$index_med <- index_table |>
    mutate(Exp = as.numeric(Exp)) |>
    group_by(Fleet, Yr) |>
    summarize(value = median(Exp))

  extra_mcmc$index_lo <- index_table |>
    mutate(Exp = as.numeric(Exp)) |>
    group_by(Fleet, Yr) |>
    summarize(value = quantile(Exp, probs[1]))

  extra_mcmc$index_hi <- index_table |>
    mutate(Exp = as.numeric(Exp)) |>
    group_by(Fleet, Yr) |>
    summarize(value = quantile(Exp, probs[3]))

  q <- index_table |>
    select(Iter, Exp, Calc_Q)
  iter <- unique(q$Iter)

  cpue <- q |>
    select(-Calc_Q) |>
    group_by(Iter) |>
    group_nest()
  cpue <- do.call(cbind, cpue$data)
  names(cpue) <- iter

  extra_mcmc$cpue_table <- cpue |>
    as_tibble() |>
    map_df(~{as.numeric(.x)})

  extra_mcmc$Q_vector <- index_table_age2plus |>
    group_by(Iter) |>
    slice(1) |>
    pull(Calc_Q) |>
    as.numeric()

  # Was Q_vector_age1
  extra_mcmc$q_age1 <- index_table_age1 |>
    group_by(Iter) |>
    slice(1) |>
    pull(Calc_Q) |>
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
      message("Calculating Pearson residuals (takes about 5 minutes)...")
    }
    comp <- extract_rep_table(reps_comp,
                              comp_header,
                              verbose = verbose,
                              ...)
    iter <- unique(comp$Iter)
    comp <- comp |>
      filter(!is.na(Nsamp_adj), Nsamp_adj > 0) |>
      select(c(Iter, Yr, Fleet, Bin, Obs, Exp, Pearson)) |>
      rename(Age = Bin)
    extra_mcmc$comp_fishery <- comp |>
      filter(Fleet == 1) |>
      select(-Fleet) |>
      mutate_all(as.numeric) |>
      group_by(Yr, Age) |>
      summarize(exp_lo = quantile(Exp, probs = probs[1]),
                exp_med = quantile(Exp, probs = probs[2]),
                exp_hi = quantile(Exp, probs = probs[3]),
                obs_lo = quantile(Obs, probs = probs[1]),
                obs_med = quantile(Obs, probs = probs[2]),
                obs_hi = quantile(Obs, probs = probs[3]),
                pearson_lo = quantile(Pearson, probs = probs[1]),
                pearson_med = quantile(Pearson, probs = probs[2]),
                pearson_hi = quantile(Pearson, probs = probs[3])) |>
      ungroup() |>
      rename(yr = Yr, age = Age)
    extra_mcmc$comp_fishery_median <- extra_mcmc$comp_fishery |>
      select(yr,
             age,
             obs = obs_med,
             exp = exp_med,
             pearson = pearson_med)

    extra_mcmc$comp_survey <- comp |>
      filter(Fleet == 2) |>
      select(-Fleet) |>
      mutate_all(as.numeric) |>
      group_by(Yr, Age) |>
      summarize(exp_lo = quantile(Exp, probs = probs[1]),
                exp_med = quantile(Exp, probs = probs[2]),
                exp_hi = quantile(Exp, probs = probs[3]),
                obs_lo = quantile(Obs, probs = probs[1]),
                obs_med = quantile(Obs, probs = probs[2]),
                obs_hi = quantile(Obs, probs = probs[3]),
                pearson_lo = quantile(Pearson, probs = probs[1]),
                pearson_med = quantile(Pearson, probs = probs[2]),
                pearson_hi = quantile(Pearson, probs = probs[3])) |>
      ungroup() |>
      rename(yr = Yr, age = Age)

    extra_mcmc$comp_survey_median <- extra_mcmc$comp_survey |>
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
    extra_mcmc$natsel.prop <- NULL
    extra_mcmc$natselwt.prop <- NULL
  }

  if(verbose){
    message("Extra MCMC size = ",
            f(as.numeric(object.size(extra_mcmc) / 1e6), 2),
            "MB\n")
  }

  extra_mcmc
}
