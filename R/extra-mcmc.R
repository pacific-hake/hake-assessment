#' Create and return a list of stats to attach to the main model by
#' looking in the model's path for the report files.
#'
#' @param model_path The model directory
#' @param probs The quantile values to use on the MCMC posterior data
#'
#' @return
#' @export
fetch_extra_mcmc <- function(model_path,
                             probs = c(0.025, 0.5, 0.975),
                             ...){

  model <- load_ss_files(model_path, ...)

  if(!dir.exists(model$extra_mcmc_path)){
    message("The ", model$extra_mcmc_path,
            " directory does not exist, so the extra mcmc files were not loaded ",
            " for model located in ", model$path)
    return(NA)
  }

  # Get the number of Report.sso files in the directory
  dir_list <- dir(model$extra_mcmc_path, full.names = TRUE)
  repfile_list <- grep("/Report_mce_.*$", dir_list, value = TRUE)
  if(!length(repfile_list)){
    message("There are no report files in the ", model$extra_mcmc_path, " directory.")
    return(NA)
  }

  message("\nLoading Extra MCMCs from ", model$extra_mcmc_path)

  # Suppress warnings because there is an extra whitespace at the end of the header line in the file.
  suppressWarnings(
    posts <- read_table2(file.path(model$extra_mcmc_path, posts_file_name), col_types = cols())
  )
  # Remove extra MLE run outputs. SS appends a new header followed by a 0-Iter row for an MLE run.
  # Sometimes MLEs are run by accident or on purpose at another time and forgotten about.
  posts <- posts %>% dplyr::filter(Iter != "Iter",
                                   Iter != 0)

  message("Loading Report.sso files...")
  # Load all report files into a list, 1 element for each report file. Elements that are NA had no file found
  reps <- map(repfile_list, ~{
    if(!file.exists(.x)){
      return(NA)
    }
    readLines(.x)
  })

  message("Loading CompReport.sso files...")
  # Load all compreport files into a list, 1 element for each report file. Elements that are NA had no file found
  compfile_list <- grep("/CompReport_mce_.*$", dir_list, value = TRUE)
  compreps <- map(compfile_list, ~{
    if(!file.exists(.x)){
      return(NA)
    }
    readLines(.x)
  })

  extra_mcmc <- list()

  #' Extract the vectors from a list into a [tibble::tibble()]
  #'
  #' @param reps_lst A list of vectors, all the same length and structure,
  #' typically extracted as a portion of a Report.sso file
  #' @param header A vector of column names for the new table
  #'
  #' @return A [tibble::tibble()] representing one row for each of the list
  #'  elements found in `reps_lst`. A new column called `Iter` is prepended and
  #'  represents the list element number that the data for each row came from.
  #'  List elements that are NA will not be included in the table.
  extract_rep_table <- function(reps_lst, header){
    lst <- map2(reps_lst, 1:length(reps_lst), ~{
      if(is.na(.x[1])){
        return(NULL)
      }
      vecs <- str_split(.x, " +")
      vec_lengths <- map_int(vecs, ~{length(.x)})
      vec_maxlength <- max(vec_lengths)
      vecs <- map(vecs, ~{
        length(.x) <- vec_maxlength
        .x
      })
      suppressMessages(
        tab <- do.call(rbind, vecs) %>%
          as_tibble(.name_repair = "unique")
      )
      names(tab) <- header
      tab %>%
        add_column(Iter = .y, .before = 1)
    })
    do.call(rbind, lst) %>%
      as_tibble()
  }

  # Make custom reps_ objects for each output. Only relevant portions of the report file will be passed to
  # the table-making map2() calls later (speeds up the map2() calls)
  rep_example <- reps[[which(!is.na(reps))[1]]]
  comprep_example <- compreps[[which(!is.na(compreps))[1]]]

  # Biomass -------------------------------------------------------------------
  message("Extracting biomass...")
  bio_header_ind <- grep("^TIME_SERIES", rep_example) + 1
  bio_header_line <- rep_example[bio_header_ind]
  bio_header_line <- bio_header_line[bio_header_line != ""]
  bio_header <- str_split(bio_header_line, " +")[[1]]
  bio_start_ind <- bio_header_ind + 1
  bio_end_ind <- grep("^SPR_SERIES", rep_example) - 2
  reps <- reps[!is.na(reps)]
  reps_bio <- map(reps, ~{.x[bio_start_ind:bio_end_ind]})

  # Likelihood ----------------------------------------------------------------
  message("Extracting likelihood...")
  like_start_ind <- grep("^LIKELIHOOD", rep_example) + 1
  like_end_ind <- like_start_ind + 28
  reps_like <- map(reps, ~{.x[like_start_ind:like_end_ind]})

  # Selectivity ---------------------------------------------------------------
  message("Extracting selectivities...")
  next_yr <- model$endyr + 1
  sel_header_ind <- grep("^AGE_SELEX", rep_example) + 5
  sel_header_line <- rep_example[sel_header_ind]
  sel_header <- str_split(sel_header_line, " +")[[1]]
  sel_ind <- grep(paste0(next_yr, "_1Asel"), rep_example)
  reps_sel <- map(reps, ~{.x[sel_ind]})
  sel <- extract_rep_table(reps_sel, sel_header) %>%
    select(-c("Factor",
              "Fleet",
              "Seas",
              "Sex",
              "Morph",
              "Label")) %>%
    map_df(as.numeric) %>%
    dplyr::filter(Yr == next_yr) %>%
    select(-c(Iter, Yr))

  # Selectivity * Weight ------------------------------------------------------
  # TODO: hack of subtracting 1 - See issue #856
  message("Extracting vulnerable biomass...")
  selwt_ind <- grep(paste0(next_yr, "_1_sel\\*wt"), rep_example)
  reps_selwt <- map(reps, ~{.x[selwt_ind]})
  selwt <- extract_rep_table(reps_selwt, sel_header)
  selwt <- selwt %>%
    select(-c("Factor",
              "Fleet",
              "Seas",
              "Sex",
              "Morph",
              "Label")) %>%
    map_df(as.numeric) %>%
    dplyr::filter(Yr == next_yr) %>%
    select(-c(Iter, Yr))

  # Dynamic B0 ----------------------------------------------------------------
  # dynb_header_ind <- grep("Dynamic_Bzero", rep_example) + 1
  # if(length(dynb_header_ind) > 1){
  #   if(length(dynb_header_ind) != 3){
  #     stop("There are not 3 Dynamic_Bzero entries in the Report.sso file",
  #          call. = FALSE)
  #   }
  #   dynb_fish_header_ind <- dynb_header_ind[2]
  #   dynb_nofish_header_ind <- dynb_header_ind[3]
  # }
  # dynb_header <- c("Yr", "Area", "Value")
  # dynb_fish_start_ind <- dynb_fish_header_ind + 4
  # dynb_nofish_start_ind <- dynb_nofish_header_ind + 4
  # dynb_fish_end_ind <- grep("NUMBERS_AT_AGE_Annual_2", rep_example) - 2
  # dynb_nofish_end_ind <- grep("NUMBERS_AT_AGE_Annual_1", rep_example) - 2
  # reps_dynb_fish <- map(reps, ~{.x[dynb_fish_start_ind:dynb_fish_end_ind]})
  # reps_dynb_nofish <- map(reps, ~{.x[dynb_nofish_start_ind:dynb_nofish_end_ind]})
  # extra_mcmc$dynb_fish <- extract_rep_table(reps_dynb_fish, dynb_header) %>%
  #   select(-c(1, 3)) %>%
  #   map_df(as.numeric) %>%
  #   dplyr::filter(Yr %in% start.yr:end.yr)
  # extra_mcmc$dynb_fish_median <- extra_mcmc$dynb_fish %>%
  #   group_by(Yr) %>%
  #   summarize(median_bo = median(Value))
  # extra_mcmc$dynb_nofish <- extract_rep_table(reps_dynb_nofish, dynb_header) %>%
  #   select(-c(1, 3)) %>%
  #   map_df(as.numeric) %>%
  #   dplyr::filter(Yr %in% start.yr:end.yr)
  # extra_mcmc$dynb_nofish_median <- extra_mcmc$dynb_nofish %>%
  #   group_by(Yr) %>%
  #   summarize(median_bo = median(Value))

  # Numbers-at-age ------------------------------------------------------------
  message("Extracting Numbers-at-age...")
  natage_header_ind <- grep("^NUMBERS_AT_AGE", rep_example) + 1
  natage_header <- str_split(rep_example[natage_header_ind], " +")[[1]]
  natage_start_ind <- natage_header_ind + 1
  natage_end_ind <- grep("^BIOMASS_AT_AGE", rep_example) - 2
  reps_natage <- map(reps, ~{.x[natage_start_ind:natage_end_ind]})
  extra_mcmc$natage <- extract_rep_table(reps_natage, natage_header) %>%
    dplyr::filter(`Beg/Mid` == "B") %>%
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
              "Time")) %>%
    map_df(as.numeric) %>%
    dplyr::filter(Yr >= start.yr) %>%
    mutate_at(.vars = vars(-Yr), ~{.x / 1e3})
  extra_mcmc$natage_median <- extra_mcmc$natage %>%
    group_by(Yr) %>%
    summarize_all(median)

  # Biomass-at-age ------------------------------------------------------------
  message("Extracting Biomass-at-age...")
  batage_header_ind <- grep("^BIOMASS_AT_AGE", rep_example) + 1
  batage_header <- str_split(rep_example[batage_header_ind], " +")[[1]]
  batage_start_ind <- batage_header_ind + 1
  batage_end_ind <- grep("^NUMBERS_AT_LENGTH", rep_example) - 2
  reps_batage <- map(reps, ~{.x[batage_start_ind:batage_end_ind]})
  extra_mcmc$batage <- extract_rep_table(reps_batage, batage_header) %>%
    dplyr::filter(`Beg/Mid` == "B") %>%
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
              "Time")) %>%
    map_df(as.numeric) %>%
    mutate_at(.vars = vars(-Yr), ~{.x / 1e3}) %>%
    dplyr::filter(Yr >= start.yr)
  extra_mcmc$batage_median <- extra_mcmc$batage %>%
    group_by(Yr) %>%
    summarize_all(median)

  # Catch-at-age in numbers ---------------------------------------------------
  message("Extracting Catch-at-age...")
  catage_header_ind <- grep("^CATCH_AT_AGE", rep_example) + 1
  catage_header <- gsub("XX", "", str_split(rep_example[catage_header_ind], " +")[[1]])
  catage_header <- catage_header[catage_header != ""]
  catage_start_ind <- catage_header_ind + 1
  catage_end_ind <- grep("^DISCARD_AT_AGE", rep_example) - 2
  reps_catage <- map(reps, ~{gsub("XX", "", .x[catage_start_ind:catage_end_ind])})
  extra_mcmc$catage <- extract_rep_table(reps_catage, catage_header) %>%
    select(-c("Area",
              "Iter",
              "Fleet",
              "Sex",
              "Type",
              "Morph",
              "Seas",
              "Era")) %>%
    map_df(as.numeric) %>%
    dplyr::filter(Yr >= start.yr)
  extra_mcmc$catage_median <- extra_mcmc$catage %>%
    group_by(Yr) %>%
    summarize_all(median)

  # Catch-at-age in biomass ---------------------------------------------------
  message("Extracting Catch-at-age in biomass...")
  iter <- extract_rep_table(reps_catage, catage_header) %>%
    pull(Iter) %>%
    unique
  wtatage <- model$wtatage %>%
    as_tibble() %>%
    dplyr::filter(Fleet == 1) %>%
    select(-(2:6)) %>%
    dplyr::filter(Yr >= start.yr)
  wtatage <- map_df(iter, ~{wtatage})
  yrs <- extra_mcmc$catage$Yr
  extra_mcmc$catage_biomass <- map2(extra_mcmc$catage, wtatage, ~{.x * .y}) %>% map_df(~{.x}) %>%
    mutate(Yr = yrs)
  extra_mcmc$catage_biomass_median <- extra_mcmc$catage_biomass %>%
    group_by(Yr) %>%
    summarize_all(median)

  # Exploitation-rate-at-age --------------------------------------------------
  # Divide catch-at-age-biomass by 1000 to make the same units, multiply by 100 to make percentage
  message("Extracting Exploitation-rate-at-age...")
  yrs <- extra_mcmc$catage_biomass$Yr
  extra_mcmc$expatage <- map2(extra_mcmc$catage_biomass, extra_mcmc$batage, ~{.x / .y / 1e3 * 100}) %>% map_df(~{.x}) %>%
    mutate(Yr = yrs)
  extra_mcmc$expatage_median <- extra_mcmc$expatage %>%
    group_by(Yr) %>%
    summarize_all(median)
  # As a check, the following returns the exact same values as the above (extra_mcmc$expatage_biomass)
  # yrs <- extra_mcmc$catage$Yr
  # extra_mcmc$expatage_numbers <- map2(extra_mcmc$catage, natage_for_exp_calc, ~{.x / .y}) %>% map_df(~{.x}) %>%
  #   mutate(Yr = yrs) %>%
  #   group_by(Yr) %>%
  #   summarize_all(median)

  # Catchability --------------------------------------------------------------
  message("Extracting Survey indices...")
  q_header_ind <- grep("^INDEX_2", rep_example) + 1
  q_header <- str_split(rep_example[q_header_ind], " +")[[1]]
  q_start_ind <- q_header_ind + 1
  ncpue <- nrow(model$dat$CPUE)
  q_end_ind <- q_start_ind + ncpue - 1
  reps_q <- map(reps, ~{.x[q_start_ind:q_end_ind]})

  # Composition tables --------------------------------------------------------
  message("Extracting Age composition tables...")
  comp_header_ind <- grep("^Composition_Database", comprep_example) + 1
  comp_header <- str_split(comprep_example[comp_header_ind], " +")[[1]]
  comp_start_ind <- comp_header_ind + 1
  comp_end_ind <- grep("End_comp_data", comprep_example) - 1
  compreps <- compreps[!is.na(compreps)]
  reps_comp <- map(compreps, ~{.x[comp_start_ind:comp_end_ind]})

  # Apply selectivity to numbers-at-age ---------------------------------------
  # TODO: hack of subtracting 1 - See issue #859
  message("Applying selectivity to numbers-at-age...")
  natage <- extra_mcmc$natage %>%
    dplyr::filter(Yr == next_yr) %>%
    select(-Yr)

  natsel <- natage * sel
  natselwt <- natage * selwt
  extra_mcmc$natsel.prop <- natsel %>%
    mutate(rsum = rowSums(.)) %>%
    mutate_at(.vars = vars(-rsum), .funs = ~{.x / rsum}) %>%
    select(-rsum)
  extra_mcmc$natselwt.prop <- natselwt %>%
    mutate(rsum = rowSums(.)) %>%
    mutate_at(.vars = vars(-rsum), .funs = ~{.x / rsum}) %>%
    select(-rsum)

  # CPUE table and values (Q) -------------------------------------------------
  message("Extracting index fits and catchabilities...")
  index_table <- extract_rep_table(reps_q, q_header)
  # Separate by fleet, 2 is acoustic survey 2+, 3 is Age-1 survey
  index_table_age2plus <- index_table %>% filter(Fleet == 2)
  index_table_age1 <- index_table %>% filter(Fleet == 3)

  extra_mcmc$q.med <- index_table %>%
    mutate(Calc_Q = as.numeric(Calc_Q)) %>%
    group_by(Fleet, Yr) %>%
    summarize(value = median(Calc_Q))

  extra_mcmc$q.025 <- index_table %>%
    mutate(Calc_Q = as.numeric(Calc_Q)) %>%
    group_by(Fleet, Yr) %>%
    summarize(value = quantile(Calc_Q, probs = 0.025))

  extra_mcmc$q.975 <- index_table %>%
    mutate(Calc_Q = as.numeric(Calc_Q)) %>%
    group_by(Fleet, Yr) %>%
    summarize(value = quantile(Calc_Q, probs = 0.975))

  extra_mcmc$index.med <- index_table %>%
    mutate(Exp = as.numeric(Exp)) %>%
    group_by(Fleet, Yr) %>%
    summarize(value = median(Exp))

  extra_mcmc$index.025 <- index_table %>%
    mutate(Exp = as.numeric(Exp)) %>%
    group_by(Fleet, Yr) %>%
    summarize(value = quantile(Exp, 0.025))

  extra_mcmc$index.975 <- index_table %>%
    mutate(Exp = as.numeric(Exp)) %>%
    group_by(Fleet, Yr) %>%
    summarize(value = quantile(Exp, 0.975))

  q <- index_table %>%
    select(Iter, Exp, Calc_Q)
  iter <- unique(q$Iter)

  cpue <- q %>%
    select(-Calc_Q) %>%
    group_by(Iter) %>%
    group_nest()
  cpue <- do.call(cbind, cpue$data)
  names(cpue) <- iter

  extra_mcmc$cpue.table <- cpue %>%
    as_tibble() %>%
    map_df(~{as.numeric(.x)})

  extra_mcmc$Q_vector <- index_table_age2plus %>%
    group_by(Iter) %>%
    slice(1) %>%
    pull(Calc_Q) %>%
    as.numeric()

  extra_mcmc$Q_vector_age1 <- index_table_age1 %>%
    group_by(Iter) %>%
    slice(1) %>%
    pull(Calc_Q) %>%
    as.numeric()

  cpue <- apply(extra_mcmc$cpue.table,
                MARGIN = 1,
                FUN = function(x){quantile(as.numeric(x),
                                           probs = probs)
                })
  extra_mcmc$cpue.0.025 <- as.numeric(cpue[1,])
  extra_mcmc$cpue.median <- as.numeric(cpue[2,])
  extra_mcmc$cpue.0.975 <- as.numeric(cpue[3,])

  # Add total biomass to time series data frame -------------------------------
  message("Add biomass summary values to time series data frame...")
  timeseries <- extract_rep_table(reps_bio, bio_header) %>%
    select(Iter, Bio_all, Bio_smry)
  iter <- unique(timeseries$Iter)
  bio_yrs <- enframe(model$timeseries$Yr, name = NULL, value = "Yr")
  ts <- split(timeseries, timeseries$Iter) %>%
    map_df(~{
      bind_cols(bio_yrs, .x)
    }) %>%
    mutate_all(as.numeric)

  extra_mcmc$total_biomass_quants <- calc_quantiles_by_group(ts,
                                                             grp_col = "Yr",
                                                             col = "Bio_all",
                                                             probs = probs)
  extra_mcmc$total_age2_plus_biomass_quants <- calc_quantiles_by_group(ts,
                                                                       grp_col = "Yr",
                                                                       col = "Bio_smry",
                                                                       probs = probs)

  # Median and quantiles of expected values and Pearson -----------------------
  message("Calculating Pearson residuals...")
  comp <- extract_rep_table(reps_comp, comp_header)
  iter <- unique(comp$Iter)
  comp <- comp %>%
    dplyr::filter(!is.na(Nsamp_adj), Nsamp_adj > 0) %>%
    select(c(Iter, Yr, Fleet, Bin, Obs, Exp, Pearson)) %>%
    rename(Age = Bin)
  extra_mcmc$comp_fishery <- comp %>%
    dplyr::filter(Fleet == 1) %>%
    select(-Fleet) %>%
    mutate_all(as.numeric) %>%
    group_by(Yr, Age) %>%
    summarize(Exp_lower = quantile(Exp, probs = 0.025),
              Exp_med = quantile(Exp, probs = 0.5),
              Exp_upper = quantile(Exp, probs = 0.975),
              Obs_lower = quantile(Obs, probs = 0.025),
              Obs_med = quantile(Obs, probs = 0.5),
              Obs_upper = quantile(Obs, probs = 0.975),
              Pearson_lower = quantile(Pearson, probs = 0.025),
              Pearson_med = quantile(Pearson, probs = 0.5),
              Pearson_upper = quantile(Pearson, probs = 0.975)) %>%
    ungroup()
  extra_mcmc$comp_fishery_median <- extra_mcmc$comp_fishery %>%
    select(Yr, Age, Obs = Obs_med, Exp = Exp_med, Pearson = Pearson_med)
  extra_mcmc$comp_survey <- comp %>%
    dplyr::filter(Fleet == 2) %>%
    select(-Fleet) %>%
    mutate_all(as.numeric) %>%
    group_by(Yr, Age) %>%
    summarize(Exp_lower = quantile(Exp, probs = 0.025),
              Exp_med = quantile(Exp, probs = 0.5),
              Exp_upper = quantile(Exp, probs = 0.975),
              Obs_lower = quantile(Obs, probs = 0.025),
              Obs_med = quantile(Obs, probs = 0.5),
              Obs_upper = quantile(Obs, probs = 0.975),
              Pearson_lower = quantile(Pearson, probs = 0.025),
              Pearson_med = quantile(Pearson, probs = 0.5),
              Pearson_upper = quantile(Pearson, probs = 0.975)) %>%
    ungroup()
  extra_mcmc$comp_survey_median <- extra_mcmc$comp_survey %>%
    select(Yr, Age, Obs = Obs_med, Exp = Exp_med, Pearson = Pearson_med)


  # like <- map2(reps_like, 1:length(reps_like), ~{
  #   if(is.na(.x[1])){
  #     return(NULL)
  #   }
  #   likes <- map(str_split(.x, " +"), ~{.x[1:4]})
  #   do.call(rbind, likes) %>%
  #     as_tibble() %>%
  #     dplyr::filter(!grepl("^#_", V1)) %>%
  #     add_column(Iter = .y, .before = 1)
  # })
  # do.call(rbind, like) %>%
  #   as_tibble()

  # Add new table of info on posterior distributions of likelihoods
  # extra_mcmc$like.info <- like_info

  message("\nFinished loading Extra MCMC output\n")

  extra_mcmc
}
