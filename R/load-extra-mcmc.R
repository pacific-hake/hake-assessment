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
#' @param verbose Logical. If `TRUE`, show progress messages
#' @param ... Arguments passed to [extract_rep_table()]
#'
#' @return The extra MCMC list
#' @export
load_extra_mcmc <- function(model,
                            probs = c(0.025, 0.5, 0.975),
                            small = TRUE,
                            verbose = TRUE,
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
              "` directory does not exist, so extra mcmc files were not loaded ",
              "for model located in:\n", model$path)
    }
    return(NA)
  }

  # Get the number of Report.sso files in the directory
  dir_list <- dir(model$extra_mcmc_path, full.names = TRUE)
  repfile_list <- grep("/Report_mce_.*$", dir_list, value = TRUE)
  if(!length(repfile_list)){
    if(verbose){
      message("There are no report files in the `", model$extra_mcmc_path,
              "` directory. `model$extra_mcmc` will be NA for the model ",
              "located at:\n`", model$path, "`\n")
    }
    return(NA)
  }
  compfile_list <- grep("/CompReport_mce_.*$", dir_list, value = TRUE)
  if(!length(compfile_list)){
    if(verbose){
      message("There are no Compreport files in the `", model$extra_mcmc_path,
              "` directory. `model$extra_mcmc` will be NA for the model ",
              "located at:\n`", model$path, "`\n")
    }
    return(NA)
  }

  posteriors_fn <- file.path(model$extra_mcmc_path, posts_file_name)
  if(!file.exists(posteriors_fn)){
    if(verbose){
      message("File `", posteriors_fn, "` does not exist.\n")
    }
    return(NA)
  }
  if(verbose){
    message("Loading posteriors file:\n`", posteriors_fn, "`\n")
  }
  # Suppress warnings because there is an extra whitespace at the end of
  # the header line in the file.
  suppressWarnings(
    posts <- read_table2(posteriors_fn, col_types = cols())
  )
  # Remove extra MLE run outputs. SS appends a new header followed by a
  # 0-Iter row for an MLE run. Sometimes MLEs are run by accident or on
  # purpose at another time and forgotten about.
  posts <- posts |>
    filter(Iter != "Iter", Iter != 0)

  # Load all report files into a list, 1 element for each report file.
  # Elements that are `NA` had no file found
  # *Note that lapply and for loops were tested for speed and imap is
  # fastest for this application

  if(verbose){
    message("Loading ", length(repfile_list), " Report files from:\n`",
            model$extra_mcmc_path, "`\nProgress:")
  }
  reps <- imap(repfile_list, ~{
    if(.y %% 500 == 0 && verbose){
      message(.y, " Report files loaded")
    }
    if(!file.exists(.x)){
      return(NA)
    }
    readLines(.x)
  })

  # Load all compreport files into a list, 1 element for each compreport file.
  # Elements that are NA had no file found

  if(verbose){
    message("Loading ", length(repfile_list), " CompReport files from:\n`",
            model$extra_mcmc_path, "`\nProgress:")
  }
  compreps <- imap(compfile_list, ~{
    if(.y %% 500 == 0 && verbose){
      message(.y, " CompReport files loaded")
    }
    if(!file.exists(.x)){
      return(NA)
    }
    readLines(.x)
  })
  if(verbose){
    message("\n")
  }

  # For debugging, can run to here once, then run the following 2 lines
  # then comment out the above two sections to avoid reading in all the files
  # every time
  #reps <<- reps
  #compreps <<-compreps

  extra_mcmc <- list()

  # Make custom reps_ objects for each output. Only relevant portions of the
  # report file will be passed to the table-making map2() calls later
  # (speeds up the map2() calls)
  rep_example <- reps[[which(!is.na(reps))[1]]]
  comprep_example <- compreps[[which(!is.na(compreps))[1]]]

  if(!small){
    # Biomass -------------------------------------------------------------------
    if(verbose){
      message("Extracting biomass...")
    }
    bio_header_ind <- grep("^TIME_SERIES", rep_example) + 1
    bio_header_line <- rep_example[bio_header_ind]
    bio_header_line <- bio_header_line[bio_header_line != ""]
    bio_header <- str_split(bio_header_line, " +")[[1]]
    bio_start_ind <- bio_header_ind + 1
    bio_end_ind <- grep("^SPR_SERIES", rep_example) - 2
    reps <- reps[!is.na(reps)]
    reps_bio <- map(reps, ~{.x[bio_start_ind:bio_end_ind]})
    timeseries <- extract_rep_table(reps_bio,
                                    bio_header,
                                    verbose = verbose,
                                    ...) |>
      select(Iter, Bio_all, Bio_smry)
    iter <- unique(timeseries$Iter)
    bio_yrs <- enframe(model$timeseries$Yr, name = NULL, value = "Yr")
    ts <- split(timeseries, timeseries$Iter) |>
      map_df(~{
        bind_cols(bio_yrs, .x)
      }) |>
      mutate_all(as.numeric)

    calc_quants_by_group <- function(d, col){
      p <- c(0.025, 0.5, 0.975)
      p_names <- map_chr(p, ~paste0(.x*100, "%"))
      p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) |>
        set_names(nm = p_names)

      col_sym <- sym(col)
      ts_quants <- ts |>
        select(Yr, !!col_sym) |>
        group_by(Yr) |>
        summarize_at(vars(!!col_sym),
                     funs(!!!p_funs))

      ts_mean <- ts |>
        select(Yr, !!col_sym) |>
        group_by(Yr) |>
        summarize_at(vars(!!col_sym),
                     funs(mean)) |>
        rename(mean = !!col_sym)

      bind_cols(ts_quants,
                ts_mean |> select(mean))
    }
    extra_mcmc$total_biomass_quants <- calc_quants_by_group(ts, "Bio_all")
    extra_mcmc$total_age2_plus_biomass_quants <- calc_quants_by_group(ts, "Bio_smry")
    # Likelihood ----------------------------------------------------------------
    if(verbose){
      message("Extracting likelihood...")
    }
    like_start_ind <- grep("^LIKELIHOOD", rep_example) + 1
    like_end_ind <- like_start_ind + 28
    reps_like <- map(reps, ~{.x[like_start_ind:like_end_ind]})
    # like <- map2(reps_like, 1:length(reps_like), ~{
    #   if(is.na(.x[1])){
    #     return(NULL)
    #   }
    #   likes <- map(str_split(.x, " +"), ~{.x[1:4]})
    #   do.call(rbind, likes) |>
    #     as_tibble() |>
    #     dplyr::filter(!grepl("^#_", V1)) |>
    #     add_column(Iter = .y, .before = 1)
    # })
    # do.call(rbind, like) |>
    #   as_tibble()

    # Add new table of info on posterior distributions of likelihoods
    # extra_mcmc$like.info <- like_info
  }

  # Selectivity ---------------------------------------------------------------
  if(verbose){
    message("Extracting selectivities...")
  }
  # Get the age selectivity header row
  sel_header_ind <- grep("^AGE_SELEX report:32", rep_example) + 5
  sel_header_line <- rep_example[sel_header_ind]
  sel_header <- str_split(sel_header_line, " +")[[1]]

  # Extract all selectivities by age, and calculate medians and CIs for
  # each year
  end_sel_ind <-
    grep("^Fecund ",
         rep_example[(sel_header_ind + 1):length(rep_example)])[1] +
    sel_header_ind - 1
  # Extract fishery selectivities only
  # `reps_sel_yrs` is a list of the number of years of fishery selectivity,
  # each element of which is a list of three tibbles, with one row each for
  # the lower quantile, median, and upper quantile from the `probs` argument
  reps_sel_yrs <- map((sel_header_ind + 1):end_sel_ind, function(line){
    reps_sel <- map(reps, ~{
      .x[line]
    })
    yr_posts <- extract_rep_table(reps_sel,
                                  sel_header,
                                  verbose = verbose,
                                  ...)
    if(unique(yr_posts$Fleet) != 1){
      return(NULL)
    }
    yr_posts <- yr_posts |>
      select(-c("Iter",
                "Factor",
                "Fleet",
                "Seas",
                "Sex",
                "Morph",
                "Label"))
    yr_lower <- yr_posts |>
      mutate_at(vars(-Yr), ~{
        quantile(.x, probs = probs[1])
      }) |>
      slice(1)
    yr_med <- yr_posts |>
      mutate_at(vars(-Yr), ~{
        quantile(.x, probs = probs[2])
      }) |>
      slice(1)
    yr_upper <- yr_posts |>
      mutate_at(vars(-Yr), ~{
        quantile(.x, probs = probs[3])
      }) |>
      slice(1)
    list(lower = yr_lower, med = yr_med, upper = yr_upper)
  })

  # Combine the `reps_sel_yrs` list so that there are three data frames,
  # One for each of lower, median and upper, with one row per year for age
  # selectivity
  extra_mcmc$sel_fishery_lower <- map(reps_sel_yrs, ~{
    .x$lower
  }) |>
    map_df(~{.x})
  extra_mcmc$sel_fishery_median <- map(reps_sel_yrs, ~{
    .x$med
  }) |>
    map_df(~{.x})
  extra_mcmc$sel_fishery_upper <- map(reps_sel_yrs, ~{
    .x$upper
  }) |>
    map_df(~{.x})

  next_yr <- model$endyr + 1
  sel_ind <- grep(paste0(next_yr, "_1Asel"), rep_example)
  reps_sel <- map(reps, ~{.x[sel_ind]})
  sel <- extract_rep_table(reps_sel,
                           sel_header,
                           verbose = verbose,
                           ...) |>
    select(-c("Factor",
              "Fleet",
              "Seas",
              "Sex",
              "Morph",
              "Label")) |>
    filter(Yr == next_yr) |>
    select(-c(Iter, Yr))

  # End year (last year of catch) selectivities
  # Fishery
  sel_ind <- grep(paste0(model$endyr, "_1Asel"), rep_example)
  reps_sel <- map(reps, ~{.x[sel_ind]})
  extra_mcmc$sel_endyr_fishery <-
    extract_rep_table(reps_sel,
                      sel_header,
                      verbose = verbose,
                      ...) |>
    select(-c("Factor",
              "Fleet",
              "Seas",
              "Sex",
              "Morph",
              "Label")) |>
    select(-c(Iter, Yr))
  # Acoustic survey
  sel_ind <- grep(paste0(model$endyr, "_2Asel"), rep_example)
  reps_sel <- map(reps, ~{.x[sel_ind]})
  extra_mcmc$sel_endyr_survey <-
    extract_rep_table(reps_sel,
                      sel_header,
                      verbose = verbose,
                      ...) |>
    select(-c("Factor",
              "Fleet",
              "Seas",
              "Sex",
              "Morph",
              "Label")) |>
    select(-c(Iter, Yr))

  # Selectivity * Weight ------------------------------------------------------
  # TODO: hack of subtracting 1 - See issue #856
  if(verbose){
    message("Extracting vulnerable biomass...")
  }
  selwt_ind <- grep(paste0(next_yr, "_1_sel\\*wt"), rep_example)
  reps_selwt <- map(reps, ~{.x[selwt_ind]})
  selwt <- extract_rep_table(reps_selwt,
                             sel_header,
                             verbose = verbose,
                             ...)
  selwt <- selwt |>
    select(-c("Factor",
              "Fleet",
              "Seas",
              "Sex",
              "Morph",
              "Label")) |>
    map_df(as.numeric) |>
    filter(Yr == next_yr) |>
    select(-c(Iter, Yr))

  # Numbers-at-age ------------------------------------------------------------
  if(verbose){
    message("Extracting Numbers-at-age...")
  }
  natage_header_ind <- grep("^NUMBERS_AT_AGE", rep_example) + 1
  natage_header <- str_split(rep_example[natage_header_ind], " +")[[1]]
  natage_start_ind <- natage_header_ind[1] + 1
  natage_end_ind <- grep("^BIOMASS_AT_AGE", rep_example) - 2
  reps_natage <- map(reps, ~{.x[natage_start_ind:natage_end_ind]})
  extra_mcmc$natage <- extract_rep_table(reps_natage,
                                         natage_header,
                                         verbose = verbose,
                                         ...) |>
    filter(`Beg/Mid` == "B") |>
    select(-c("Area",
              "Bio_Pattern",
              "Iter",
              "Sex",
              "BirthSeas",
              "Settlement",
              "Platoon",
              "Morph",
              "Seas",
              "Beg/Mid",
              "Era",
              "Time")) |>
    map_df(as.numeric) |>
    filter(Yr >= start_yr) |>
    mutate_at(.vars = vars(-Yr), ~{.x / 1e3})

  extra_mcmc$natage_median <- extra_mcmc$natage |>
    group_by(Yr) |>
    summarize_all(median)

  if(!small){
    # Biomass-at-age ------------------------------------------------------------
    if(verbose){
      message("Extracting Biomass-at-age...")
    }
    batage_header_ind <- grep("^BIOMASS_AT_AGE", rep_example) + 1
    batage_header <- str_split(rep_example[batage_header_ind], " +")[[1]]
    batage_start_ind <- batage_header_ind + 1
    batage_end_ind <- grep("^NUMBERS_AT_LENGTH", rep_example) - 2
    reps_batage <- map(reps, ~{.x[batage_start_ind:batage_end_ind]})
    extra_mcmc$batage <- extract_rep_table(reps_batage,
                                           batage_header,
                                           verbose = verbose,
                                           ...) |>
      filter(`Beg/Mid` == "B") |>
      select(-c("Area",
                "Bio_Pattern",
                "Iter",
                "Sex",
                "BirthSeas",
                "Settlement",
                "Platoon",
                "Morph",
                "Seas",
                "Beg/Mid",
                "Era",
                "Time")) |>
      map_df(as.numeric) |>
      mutate_at(.vars = vars(-Yr), ~{.x / 1e3}) |>
      filter(Yr >= start_yr)

    extra_mcmc$batage_median <- extra_mcmc$batage |>
      group_by(Yr) |>
      summarize_all(median)

    # Catch-at-age in numbers ---------------------------------------------------
    if(verbose){
      message("Extracting Catch-at-age...")
    }
    catage_header_ind <- grep("^CATCH_AT_AGE", rep_example) + 1
    catage_header <- gsub("XX", "", str_split(rep_example[catage_header_ind], " +")[[1]])
    catage_header <- catage_header[catage_header != ""]
    catage_start_ind <- catage_header_ind + 1
    catage_end_ind <- grep("^DISCARD_AT_AGE", rep_example) - 2
    reps_catage <- map(reps, ~{gsub("XX", "", .x[catage_start_ind:catage_end_ind])})

    catage <- extract_rep_table(reps_catage,
                                catage_header,
                                verbose = verbose,
                                ...)
    extra_mcmc$catage <- catage |>
      select(-c("Area",
                "Iter",
                "Fleet",
                "Sex",
                "Type",
                "Morph",
                "Seas",
                "Era")) |>
      map_df(as.numeric) |>
      filter(Yr >= start_yr)

    extra_mcmc$catage_median <- extra_mcmc$catage |>
      group_by(Yr) |>
      summarize_all(median)
    # Catch-at-age in biomass ---------------------------------------------------
    if(verbose){
      message("Extracting Catch-at-age in biomass...")
    }
    iter <- catage |>
      pull(Iter) |>
      unique()

    wt <- model$wtatage |>
      as_tibble() |>
      filter(Yr > 0)
    actual_start_yr <- min(unique(extra_mcmc$catage$Yr))
    start_yr_wtatage <- min(wt$Yr)
    num_missing_yrs <- start_yr_wtatage - actual_start_yr
    if(num_missing_yrs > 0){
      # Need to fill in missing rows from actual_start_yr to start_yr_wtatage
      row <- model$wtatage |>
        as_tibble() |>
        filter(Yr == min(Yr), Fleet == 2)

      if(nrow(row) != 1){
        stop("The biomass-at-age table failed to build. The `model$wtatage` ",
             "table had ", nrow(row), " rows match the filter for minimum ",
             "year and Fleet == 2, when it should only be one row",
             call. = FALSE)
      }
      missing_yrs <- actual_start_yr:(start_yr_wtatage - 1)
      for(yr in rev(missing_yrs)){
        row$Yr <- yr
        wt <- rbind(row, wt)
      }
    }
    wtatage <- wt |>
      filter(Fleet == 2) |>
      select(-c(Seas, Sex, Bio_Pattern, BirthSeas, Fleet))

    wtatage <- map_df(iter, ~{wtatage})
    extra_mcmc$catage_biomass <- mapply(`*`,
                                        select(extra_mcmc$catage, -Yr),
                                        select(wtatage, -Yr)) |>
      as_tibble() |>
      bind_cols(select(extra_mcmc$catage, Yr)) |>
      select(Yr, everything())

    extra_mcmc$catage_biomass_median <- extra_mcmc$catage_biomass |>
      group_by(Yr) |>
      summarize_all(median)
    # Exploitation-rate-at-age --------------------------------------------------
    # Divide catch-at-age-biomass by 1000 to make the same units, multiply by 100 to make percentage
    if(verbose){
      message("Extracting Exploitation-rate-at-age...")
    }

    catage <- extra_mcmc$catage
    batage <- extra_mcmc$batage

    extra_mcmc$expatage <- mapply(`/`,
                                  select(catage, -Yr),
                                  select(batage, -Yr))

    # Divide every cell by 1,000 to get thousands of tonnes,
    # then multiply by 100 for
    extra_mcmc$expatage <- extra_mcmc$expatage / 1000 * 100

    extra_mcmc$expatage <- extra_mcmc$expatage |>
      bind_cols(select(extra_mcmc$catage, Yr)) |>
      select(Yr, everything())

    extra_mcmc$expatage_median <- extra_mcmc$expatage |>
      group_by(Yr) |>
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
  natage <- extra_mcmc$natage |>
    filter(Yr == next_yr) |>
    select(-Yr)

  natsel <- natage * sel
  natselwt <- natage * selwt
  extra_mcmc$natsel.prop <- natsel %>%
    mutate(rsum = rowSums(.)) |>
    mutate_at(.vars = vars(-rsum), .funs = ~{.x / rsum}) |>
    select(-rsum)

  extra_mcmc$natselwt.prop <- natselwt %>%
    mutate(rsum = rowSums(.)) |>
    mutate_at(.vars = vars(-rsum), .funs = ~{.x / rsum}) |>
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

  extra_mcmc$q.med <- index_table |>
    mutate(Calc_Q = as.numeric(Calc_Q)) |>
    group_by(Fleet, Yr) |>
    summarize(value = median(Calc_Q), .groups = )

  extra_mcmc$q.025 <- index_table |>
    mutate(Calc_Q = as.numeric(Calc_Q)) |>
    group_by(Fleet, Yr) |>
    summarize(value = quantile(Calc_Q, probs = 0.025))

  extra_mcmc$q.975 <- index_table |>
    mutate(Calc_Q = as.numeric(Calc_Q)) |>
    group_by(Fleet, Yr) |>
    summarize(value = quantile(Calc_Q, probs = 0.975))

  extra_mcmc$index.med <- index_table |>
    mutate(Exp = as.numeric(Exp)) |>
    group_by(Fleet, Yr) |>
    summarize(value = median(Exp))

  extra_mcmc$index.025 <- index_table |>
    mutate(Exp = as.numeric(Exp)) |>
    group_by(Fleet, Yr) |>
    summarize(value = quantile(Exp, 0.025))

  extra_mcmc$index.975 <- index_table |>
    mutate(Exp = as.numeric(Exp)) |>
    group_by(Fleet, Yr) |>
    summarize(value = quantile(Exp, 0.975))

  q <- index_table |>
    select(Iter, Exp, Calc_Q)
  iter <- unique(q$Iter)

  cpue <- q |>
    select(-Calc_Q) |>
    group_by(Iter) |>
    group_nest()
  cpue <- do.call(cbind, cpue$data)
  names(cpue) <- iter

  extra_mcmc$cpue.table <- cpue |>
    as_tibble() |>
    map_df(~{as.numeric(.x)})

  extra_mcmc$Q_vector <- index_table_age2plus |>
    group_by(Iter) |>
    slice(1) |>
    pull(Calc_Q) |>
    as.numeric()

  extra_mcmc$Q_vector_age1 <- index_table_age1 |>
    group_by(Iter) |>
    slice(1) |>
    pull(Calc_Q) |>
    as.numeric()

  cpue <- apply(extra_mcmc$cpue.table,
                MARGIN = 1,
                FUN = function(x){quantile(as.numeric(x),
                                           probs = probs)
                })
  extra_mcmc$cpue.0.025 <- as.numeric(cpue[1,])
  extra_mcmc$cpue.median <- as.numeric(cpue[2,])
  extra_mcmc$cpue.0.975 <- as.numeric(cpue[3,])

  if(!small){
    # Median and quantiles of expected values and Pearson -----------------------
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
      summarize(Exp_lower = quantile(Exp, probs = 0.025),
                Exp_med = quantile(Exp, probs = 0.5),
                Exp_upper = quantile(Exp, probs = 0.975),
                Obs_lower = quantile(Obs, probs = 0.025),
                Obs_med = quantile(Obs, probs = 0.5),
                Obs_upper = quantile(Obs, probs = 0.975),
                Pearson_lower = quantile(Pearson, probs = 0.025),
                Pearson_med = quantile(Pearson, probs = 0.5),
                Pearson_upper = quantile(Pearson, probs = 0.975)) |>
      ungroup()
    extra_mcmc$comp_fishery_median <- extra_mcmc$comp_fishery |>
      select(Yr, Age, Obs = Obs_med, Exp = Exp_med, Pearson = Pearson_med)
    extra_mcmc$comp_survey <- comp |>
      filter(Fleet == 2) |>
      select(-Fleet) |>
      mutate_all(as.numeric) |>
      group_by(Yr, Age) |>
      summarize(Exp_lower = quantile(Exp, probs = 0.025),
                Exp_med = quantile(Exp, probs = 0.5),
                Exp_upper = quantile(Exp, probs = 0.975),
                Obs_lower = quantile(Obs, probs = 0.025),
                Obs_med = quantile(Obs, probs = 0.5),
                Obs_upper = quantile(Obs, probs = 0.975),
                Pearson_lower = quantile(Pearson, probs = 0.025),
                Pearson_med = quantile(Pearson, probs = 0.5),
                Pearson_upper = quantile(Pearson, probs = 0.975)) |>
      ungroup()
    extra_mcmc$comp_survey_median <- extra_mcmc$comp_survey |>
      select(Yr, Age, Obs = Obs_med, Exp = Exp_med, Pearson = Pearson_med)
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
