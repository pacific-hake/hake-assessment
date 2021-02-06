
#' Run the extra MCMC in parallel mode
#'
#' @param model_path The full path of the model to run extra MCMC for
#' @param num_procs The number of processors to use in parallel
#' @param extra_mcmc_path Short name of the path to store extra-mcmc output in
#' @param ...
#'
#' @return Nothing
#' @export
#' @importFrom parallel detectCores
#' @importFrom future plan
#' @importFrom furrr future_map
#' @importFrom readr read_table2 cols
run_extra_mcmc <- function(model_path,
                           num_procs = detectCores() - 1,
                           extra_mcmc_path = "extra-mcmc",
                           ...){
  stopifnot(num_procs < detectCores())
  model <- load_ss_files(model_path)
  model_path <- model$path
  mcmc_path <- model$mcmcpath

  ## Read posteriors and split up into batches
  num_posts_per_proc <- nrow(model$mcmc) %/% num_procs
  num_leftover_posts <- nrow(model$mcmc) %% num_procs
  num_posts_by_proc <- c(rep(num_posts_per_proc, num_procs - 1), num_posts_per_proc + num_leftover_posts)

  from_to <- tibble(from = 1, to = num_posts_by_proc[1])
  for(i in 2:length(num_posts_by_proc)){
    nxt <- tibble(from = from_to[i - 1,]$to + 1, to = from_to[i - 1,]$to + num_posts_by_proc[i])
    from_to <- bind_rows(from_to, nxt)
  }
  posts <- read_table2(file.path(model$mcmcpath, posts_file_name), col_types = cols())
  # Remove all extra columns that start with X. These represent extra whitespace at end of header row in file
  postsxgrep <- grep("^X", names(posts))
  if(length(postsxgrep)){
    posts <- posts %>%
      select(-postsxgrep)
  }
  post_lst <- split_df(posts, from_to)
  derposts <- read_table2(file.path(model$mcmcpath, derposts_file_name), col_types = cols())
  # Remove all extra columns that start with X. These represent extra whitespace at end of header row in file
  derxgrep <- grep("^X", names(derposts))
  if(length(derxgrep)){
    derposts <- derposts %>%
      select(-derxgrep)
  }
  derpost_lst <- split_df(derposts, from_to)

  extra_mcmc_full_path <- file.path(model_path, extra_mcmc_path)
  dir.create(extra_mcmc_full_path, showWarnings = FALSE)
  unlink(file.path(extra_mcmc_full_path, "*"), recursive = TRUE)

  reports_path <- file.path(extra_mcmc_full_path, "reports")
  dir.create(reports_path, showWarnings = FALSE)
  unlink(file.path(reports_path, "*"), recursive = TRUE)

  plan("multisession")
  future_map(1:nrow(from_to), ~{
    run_extra_mcmc_chunk(model,
                         posts = post_lst[[.x]],
                         derposts = derpost_lst[[.x]],
                         extra_mcmc_path = extra_mcmc_path,
                         extra_mcmc_num = .x,
                         from_to = from_to[.x,],
                         ...)})
  plan()
}

#' Setup model files and batch files for a single extra-mcmc folder
#'
#' @param model A model as loaded by [load_ss_files()]
#' @param posts A data frame of posteriors
#' @param derposts A data frame of derived posteriors
#' @param extra_mcmc_path The short path of the extra MCMC folder
#' @param extra_mcmc_num The number of the folder inside the extra MCMC folder
#' @param from_two A two-element vector for the indices to be used to write the report files
#' @return
#' @export
#'
#' @examples
run_extra_mcmc_chunk <- function(model,
                                 posts,
                                 derposts,
                                 extra_mcmc_path = "extra-mcmc",
                                 extra_mcmc_num,
                                 from_to,
                                 ...){
  from_to <- as.numeric(from_to[1, 1]):as.numeric(from_to[1, 2])
  model_path <- model$path
  mcmc_path <- model$mcmcpath
  checksum <- 999
  ## create a table of parameter values based on labels in parameters section of Report.sso
  newpar <- data.frame(value = c(1, model$parameters$Value, checksum),
                       hash = "#",
                       label = c("dummy_parm", model$parameters$Label, "checksum999"),
                       stringsAsFactors = FALSE)

  ## add hash before first column name
  names(newpar)[1] <- "#value"

  extra_mcmc_full_path <- file.path(model_path, extra_mcmc_path)
  reports_path <- file.path(extra_mcmc_full_path, "reports")

  sub_extra_mcmc_path <- file.path(extra_mcmc_full_path, paste0(extra_mcmc_path, "-", extra_mcmc_num))
  dir.create(sub_extra_mcmc_path, showWarnings = FALSE)
  unlink(file.path(sub_extra_mcmc_path, "*"), recursive = TRUE)
  dir.create(reports_path, showWarnings = FALSE)
  unlink(file.path(reports_path, "*"), recursive = TRUE)

  ## Copy files into the subdirectory from the model/mcmc directory
  file.copy(file.path(mcmc_path, list.files(mcmc_path)), sub_extra_mcmc_path)
  write_delim(posts, file.path(sub_extra_mcmc_path, posts_file_name))
  write_delim(derposts, file.path(sub_extra_mcmc_path, derposts_file_name))
  write_delim(newpar, file.path(sub_extra_mcmc_path, par_file_name))

  start <- SS_readstarter(file.path(sub_extra_mcmc_path, starter_file_name), verbose = FALSE)
  ## Change starter file to read from par file
  start$init_values_src <- 1
  SS_writestarter(start, dir = sub_extra_mcmc_path, file = starter_file_name, overwrite = TRUE, verbose = FALSE)

  ## modify control file to make bias adjustment of recruit devs = 1.0 for all years
  ## this is required to match specification used by MCMC as noted in
  ## "Spawner-Recruitment" section of SS User Manual and described in
  ## Methot & Taylor (2011)
  ctl_lines <- readLines(file.path(sub_extra_mcmc_path, start$ctlfile))
  bias_adjust_line_num <- grep("Maximum bias adjustment in MPD", ctl_lines)
  if(length(bias_adjust_line_num) == 0){
    # alternative label used in control.ss_new file
    bias_adjust_line_num <- grep("max_bias_adj_in_MPD", ctl_lines)
  }
  ctl_lines[bias_adjust_line_num] <-
    "-1      # Maximum bias adjustment in MPD (set to -1 for extra.mcmc only)"
  writeLines(ctl_lines, file.path(sub_extra_mcmc_path, start$ctlfile))
  for(irow in 1:nrow(posts)){
    ## replace values in newpar table with posteriors values
    ## (excluding 1 and 2 for "Iter" and "Objective_function")

    newpar[newpar$label %in% names(posts), 1] <- as.numeric(posts[irow, -(1:2)])
    write_delim(newpar, file.path(sub_extra_mcmc_path, par_file_name))
    ## delete existing output files to make sure that if model fails to run,
    ## it won't just copy the same files again and again
    file.remove(file.path(sub_extra_mcmc_path, report_file_name))
    file.remove(file.path(sub_extra_mcmc_path, compreport_file_name))

    shell_command <- paste0("cd ", sub_extra_mcmc_path, " & ", ss_executable, " -maxfn 0 -phase 10 -nohess")
    shell(shell_command, wait = FALSE, intern = TRUE)

    file.copy(file.path(sub_extra_mcmc_path, par_file_name),
              file.path(reports_path, paste0("ss_output", from_to[irow], ".par")),
              overwrite = TRUE)
    file.copy(file.path(sub_extra_mcmc_path, report_file_name),
              file.path(reports_path, paste0("Report_", from_to[irow], ".sso")),
              overwrite = TRUE)
    file.copy(file.path(sub_extra_mcmc_path, compreport_file_name),
              file.path(reports_path, paste0("CompReport_", from_to[irow], ".sso")),
              overwrite = TRUE)
  }
}

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
  mcmc_path <- model$mcmcpath
  extra_mcmc_path <- file.path(model_path, extra_mcmc_path)
  extra_mcmc <- NULL

  if(!dir.exists(extra_mcmc_path)){
    message("The ", extra_mcmc_path, " directory does not exist, so the extra-mcmc wass not loaded")
    return(NA)
  }
  reports_dir <- file.path(extra_mcmc_path, extra_mcmc_reports_path)
  if(!dir.exists(reports_dir)){
    message("The ", reports_dir, " directory does not exist, so the extra-mcmc wass not loaded")
    return(NA)
  }

  # Get the extra-mcmc directories done in parallel
  extra_mcmc_dirs <- dir(extra_mcmc_path)
  extra_mcmc_dirs <- file.path(extra_mcmc_path, extra_mcmc_dirs[grepl("extra-mcmc", extra_mcmc_dirs)])

  # Get the number of Report.sso files in the directory
  dir_list <- dir(reports_dir)
  if(!length(dir_list)){
    message("There are no report files in the ", reports_dir, " directory.")
    return(NA)
  }

  message("\nLoading Extra MCMCs from ", extra_mcmc_path)

  # Suppress warnings because there is an extra whitespace at the end of the header line in the file.
  suppressWarnings(
    posts <- read_table2(file.path(mcmc_path, posts_file_name), col_types = cols())
  )
  # Remove extra MLE run outputs. SS appends a new header followed by a 0-Iter row for an MLE run.
  # Sometimes MLEs are run by accident or on purpose at another time and forgotten about.
  posts <- posts %>% filter(Iter != "Iter",
                            Iter != 0)

  # Break up the loading of report files into the number of posteriors in each extra-mcmc subdir
  num_reports_each <- map_int(extra_mcmc_dirs, ~{
    # Suppress warnings because there may be extra 'Iter' lines followed by '0' lines
    # because the SS MLE just appends these to the posteriors.sso file
    suppressWarnings(
      posts <- read_table2(file.path(.x, posts_file_name), col_types = cols())
    )
    posts <- posts %>% filter(Iter != "Iter",
                              Iter != 0)
    nrow(posts)
  })

  # from_to is a two-column data.frame with the indices from and to for each processor to load report files
  from_to <- tibble(from = 1, to = num_reports_each[1])
  for(i in 2:length(num_reports_each)){
    nxt <- tibble(from = from_to[i - 1,]$to + 1, to = from_to[i - 1,]$to + num_reports_each[i])
    from_to <- bind_rows(from_to, nxt)
  }

  # DEBUG - Make this way faster, uncomment the following line
  # from_to <- tibble(from = 1000, to = 1010)

  # Load all report files into a list, 1 element for each report file. Elements that are NA had no file found
  reps <- map(1:nrow(from_to), ~{
    inds <- as.numeric(from_to[.x, 1]):as.numeric(from_to[.x, 2])
    map(inds, ~{
      rep_file <- file.path(reports_dir, paste0("Report_", .x, ".sso"))
      if(!file.exists(rep_file)){
        return(NA)
      }
      readLines(rep_file)
    })
  }) %>%
    flatten()

  # Load all compreport files into a list, 1 element for each report file. Elements that are NA had no file found
  compreps <- map(1:nrow(from_to), ~{
    inds <- as.numeric(from_to[.x, 1]):as.numeric(from_to[.x, 2])
    map(inds, ~{
      comprep_file <- file.path(reports_dir, paste0("CompReport_", .x, ".sso"))
      if(!file.exists(comprep_file)){
        return(NA)
      }
      readLines(comprep_file)
    })
  }) %>%
    flatten()

  #' Extract the vectors from a list into a [tibble::tibble()]
  #'
  #' @param reps_lst A list of vectors, all the same length and structure,
  #' typically extracted as a portion of a Report.sso file
  #' @param header A vector of column nmaes for the new table
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
  bio_header_ind <- grep("TIME_SERIES", rep_example) + 1
  bio_header_line <- rep_example[bio_header_ind]
  bio_header_line <- bio_header_line[bio_header_line != ""]
  bio_header <- str_split(bio_header_line, " +")[[1]]
  bio_start_ind <- bio_header_ind + 1
  if(length(bio_start_ind) > 1){
    bio_start_ind <- bio_start_ind[2]
  }
  bio_end_ind <- grep("SPR_SERIES", rep_example) - 2
  if(length(bio_end_ind) > 1){
    bio_end_ind <- bio_end_ind[2]
  }
  reps <- reps[!is.na(reps)]
  reps_bio <- map(reps, ~{.x[bio_start_ind:bio_end_ind]})

  # Likelihood ----------------------------------------------------------------
  like_start_ind <- grep("LIKELIHOOD", rep_example) + 1
  if(length(like_start_ind) > 1){
    like_start_ind <- like_start_ind[2]
  }
  like_end_ind <- like_start_ind + 28
  reps_like <- map(reps, ~{.x[like_start_ind:like_end_ind]})

  # Selectivity ---------------------------------------------------------------
  next_yr <- model$endyr + 1
  sel_header_ind <- grep("Factor Fleet Yr Seas", rep_example)
  sel_header_line <- rep_example[sel_header_ind]
  sel_header <- str_split(sel_header_line, " +")[[1]]
  sel_ind <- grep(paste0(next_yr, "_1Asel"), rep_example)
  reps_sel <- map(reps, ~{.x[sel_ind]})
  sel <- extract_rep_table(reps_sel, sel_header) %>%
    select(-c(2:3, 5:8)) %>%
    map_df(as.numeric) %>%
    filter(Yr == next_yr) %>%
    select(-c(Iter, Yr))

  # Selectivity * Weight ------------------------------------------------------
  selwt_ind <- grep(paste0(next_yr, "_1_sel\\*wt"), rep_example)
  reps_selwt <- map(reps, ~{.x[selwt_ind]})
  selwt <- extract_rep_table(reps_selwt, sel_header) %>%
    select(-c(2:3, 5:8)) %>%
    map_df(as.numeric) %>%
    filter(Yr == next_yr) %>%
    select(-c(Iter, Yr))

  # Dynamic B0 ----------------------------------------------------------------
  dynb_header_ind <- grep("Dynamic_Bzero", rep_example) + 1
  if(length(dynb_header_ind) > 1){
    if(length(dynb_header_ind) != 3){
      stop("There are not 3 Dynamic_Bzero entries in the Report.sso file",
           call. = FALSE)
    }
    dynb_fish_header_ind <- dynb_header_ind[2]
    dynb_nofish_header_ind <- dynb_header_ind[3]
  }
  dynb_header <- c("Yr", "Area", "Value")
  dynb_fish_start_ind <- dynb_fish_header_ind + 4
  dynb_nofish_start_ind <- dynb_nofish_header_ind + 4
  dynb_fish_end_ind <- grep("NUMBERS_AT_AGE_Annual_2", rep_example) - 2
  dynb_nofish_end_ind <- grep("NUMBERS_AT_AGE_Annual_1", rep_example) - 2
  reps_dynb_fish <- map(reps, ~{.x[dynb_fish_start_ind:dynb_fish_end_ind]})
  reps_dynb_nofish <- map(reps, ~{.x[dynb_nofish_start_ind:dynb_nofish_end_ind]})
  extra_mcmc$dynb_fish <- extract_rep_table(reps_dynb_fish, dynb_header) %>%
    select(-c(1, 3)) %>%
    map_df(as.numeric) %>%
    filter(Yr %in% start.yr:end.yr)
  extra_mcmc$dynb_fish_median <- extra_mcmc$dynb_fish %>%
    group_by(Yr) %>%
    summarize(median_bo = median(Value))
  extra_mcmc$dynb_nofish <- extract_rep_table(reps_dynb_nofish, dynb_header) %>%
    select(-c(1, 3)) %>%
    map_df(as.numeric) %>%
    filter(Yr %in% start.yr:end.yr)
  extra_mcmc$dynb_nofish_median <- extra_mcmc$dynb_nofish %>%
    group_by(Yr) %>%
    summarize(median_bo = median(Value))

  # Numbers-at-age ------------------------------------------------------------
  natage_header_ind <- grep("NUMBERS_AT_AGE_Annual_2 With_fishery", rep_example) + 1
  natage_header <- str_split(rep_example[natage_header_ind], " +")[[1]]
  natage_start_ind <- natage_header_ind + 1
  natage_end_ind <- grep("Z_AT_AGE_Annual_2 With_fishery", rep_example) - 2
  reps_natage <- map(reps, ~{.x[natage_start_ind:natage_end_ind]})
  extra_mcmc$natage <- extract_rep_table(reps_natage, natage_header) %>%
    select(-(1:3)) %>%
    map_df(as.numeric) %>%
    filter(Yr >= start.yr) %>%
    mutate_at(.vars = vars(-Yr), ~{.x / 1e3})
  extra_mcmc$natage_median <- extra_mcmc$natage %>%
    group_by(Yr) %>%
    summarize_all(median)

  # Biomass-at-age ------------------------------------------------------------
  batage_header_ind <- grep("BIOMASS_AT_AGE", rep_example) + 1
  if(length(batage_header_ind) > 1){
    batage_header_ind <- batage_header_ind[2]
  }
  batage_header <- str_split(rep_example[batage_header_ind], " +")[[1]]
  batage_start_ind <- batage_header_ind + 1
  batage_end_ind <- grep("NUMBERS_AT_LENGTH", rep_example) - 2
  if(length(batage_end_ind) > 1){
    batage_end_ind <- batage_end_ind[2]
  }
  reps_batage <- map(reps, ~{.x[batage_start_ind:batage_end_ind]})
  extra_mcmc$batage <- extract_rep_table(reps_batage, batage_header) %>%
    filter(`Beg/Mid` == "B") %>%
    select(-c(1:8, 10:13)) %>%
    map_df(as.numeric) %>%
    mutate_at(.vars = vars(-Yr), ~{.x / 1e3}) %>%
    filter(Yr >= start.yr)
  extra_mcmc$batage_median <- extra_mcmc$batage %>%
    group_by(Yr) %>%
    summarize_all(median)

  # Catch-at-age in numbers ---------------------------------------------------
  catage_header_ind <- grep("CATCH_AT_AGE", rep_example) + 1
  if(length(catage_header_ind) > 1){
    catage_header_ind <- catage_header_ind[2]
  }
  catage_header <- gsub("XX", "", str_split(rep_example[catage_header_ind], " +")[[1]])
  catage_header <- catage_header[catage_header != ""]
  catage_start_ind <- catage_header_ind + 1
  catage_end_ind <- grep("DISCARD_AT_AGE", rep_example) - 2
  if(length(catage_end_ind) > 1){
    catage_end_ind <- catage_end_ind[2]
  }
  reps_catage <- map(reps, ~{gsub("XX", "", .x[catage_start_ind:catage_end_ind])})
  extra_mcmc$catage <- extract_rep_table(reps_catage, catage_header) %>%
    select(-c(1:6, 8:9)) %>%
    map_df(as.numeric) %>%
    filter(Yr >= start.yr)
  extra_mcmc$catage_median <- extra_mcmc$catage %>%
    group_by(Yr) %>%
    summarize_all(median)

  # Catch-at-age in biomass ---------------------------------------------------
  iter <- extract_rep_table(reps_catage, catage_header) %>%
    pull(Iter) %>%
    unique
  wtatage <- model$wtatage %>%
    as_tibble() %>%
    filter(Fleet == 1) %>%
    select(-(2:6)) %>%
    filter(Yr >= start.yr)
  wtatage <- map_df(iter, ~{wtatage})
  yrs <- extra_mcmc$catage$Yr
  extra_mcmc$catage_biomass <- map2(extra_mcmc$catage, wtatage, ~{.x * .y}) %>% map_df(~{.x}) %>%
    mutate(Yr = yrs)
  extra_mcmc$catage_biomass_median <- extra_mcmc$catage_biomass %>%
    group_by(Yr) %>%
    summarize_all(median)

  # Exploitation-rate-at-age --------------------------------------------------
  yrs <- extra_mcmc$catage_biomass$Yr
  extra_mcmc$expatage <- map2(extra_mcmc$catage_biomass, extra_mcmc$batage, ~{.x / .y}) %>% map_df(~{.x}) %>%
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
  q_header_ind <- grep("INDEX_2", rep_example) + 1
  if(length(q_header_ind) > 1){
    q_header_ind <- q_header_ind[2]
  }
  q_header <- str_split(rep_example[q_header_ind], " +")[[1]]
  q_start_ind <- q_header_ind + 1
  ncpue <- nrow(model$dat$CPUE)
  q_end_ind <- q_start_ind + ncpue - 1
  reps_q <- map(reps, ~{.x[q_start_ind:q_end_ind]})
  ## Comp tables
  comp_header_ind <- grep("Composition_Database", comprep_example) + 1
  comp_header <- str_split(comprep_example[comp_header_ind], " +")[[1]]
  comp_start_ind <- comp_header_ind + 1
  comp_end_ind <- grep("End_comp_data", comprep_example) - 1
  compreps <- compreps[!is.na(compreps)]
  reps_comp <- map(compreps, ~{.x[comp_start_ind:comp_end_ind]})


  # like <- map2(reps_like, 1:length(reps_like), ~{
  #   if(is.na(.x[1])){
  #     return(NULL)
  #   }
  #   likes <- map(str_split(.x, " +"), ~{.x[1:4]})
  #   do.call(rbind, likes) %>%
  #     as_tibble() %>%
  #     filter(!grepl("^#_", V1)) %>%
  #     add_column(Iter = .y, .before = 1)
  # })
  # do.call(rbind, like) %>%
  #   as_tibble()

  # Apply selectivity to numbers-at-age ---------------------------------------
  natage <- extra_mcmc$natage %>%
    filter(Yr == next_yr) %>%
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
  q <- extract_rep_table(reps_q, q_header) %>%
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

  extra_mcmc$Q_vector <- q %>%
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

  # Add total biomass to existing time series data frame ----------------------
  timeseries <- extract_rep_table(reps_bio, bio_header) %>%
    select(Iter, Bio_all, Bio_smry)
  iter <- unique(timeseries$Iter)
  Bio_all <- timeseries %>%
    select(Iter, Bio_all) %>%
    group_by(Iter) %>%
    group_nest()
  Bio_all <- do.call(cbind, Bio_all$data)
  names(Bio_all) <- iter
  Bio_all <- apply(Bio_all,
                    MARGIN = 1,
                    FUN = function(x){quantile(as.numeric(x),
                                               probs = probs)
                    })
  extra_mcmc$timeseries <- model$timeseries
  extra_mcmc$timeseries$Bio_all.0.025 <- as.numeric(Bio_all[1,])
  extra_mcmc$timeseries$Bio_all.median <- as.numeric(Bio_all[2,])
  extra_mcmc$timeseries$Bio_all.0.975 <- as.numeric(Bio_all[3,])

  Bio_smry <- timeseries %>%
    select(Iter, Bio_smry) %>%
    group_by(Iter) %>%
    group_nest()
  Bio_smry <- do.call(cbind, Bio_smry$data)
  names(Bio_smry) <- iter
  Bio_smry <- apply(Bio_smry,
                         MARGIN = 1,
                         FUN = function(x){quantile(as.numeric(x),
                                                    probs = probs)
                         })
  extra_mcmc$timeseries$Bio_smry.0.025 <- as.numeric(Bio_smry[1,])
  extra_mcmc$timeseries$Bio_smry.median <- as.numeric(Bio_smry[2,])
  extra_mcmc$timeseries$Bio_smry.0.975 <- as.numeric(Bio_smry[3,])

  comp <- extract_rep_table(reps_comp, comp_header)
  ## median and quantiles of expected values and Pearsons
  iter <- unique(comp$Iter)
  comp <- comp %>%
    filter(!is.na(Nsamp_adj), Nsamp_adj > 0)
  exp_table <- comp %>%
    select(Iter, Exp) %>%
    group_by(Iter) %>%
    group_nest()
  exp_table <- do.call(cbind, exp_table$data)
  names(exp_table) <- iter
  exp_table <- apply(exp_table,
                     MARGIN = 1,
                     FUN = function(x){quantile(as.numeric(x),
                                                probs = probs)
                       })
  extra_mcmc$agedbase <- model$agedbase
  extra_mcmc$agedbase$Exp.025 <- exp_table[1,]
  extra_mcmc$agedbase$Exp <- exp_table[2,]
  extra_mcmc$agedbase$Exp.975 <- exp_table[3,]

  pearson_table <- comp %>%
    select(Iter, Pearson) %>%
    group_by(Iter) %>%
    group_nest()
  pearson_table <- do.call(cbind, pearson_table$data)
  names(pearson_table) <- iter
  pearson_table <- apply(pearson_table,
                     MARGIN = 1,
                     FUN = function(x){quantile(as.numeric(x),
                                                probs = probs)
                     })
  extra_mcmc$agedbase$Pearson.025 <- pearson_table[1,]
  extra_mcmc$agedbase$Pearson <- pearson_table[2,]
  extra_mcmc$agedbase$Pearson.975 <- pearson_table[3,]

  ## add new table of info on posterior distributions of likelihoods
  ## extra_mcmc$like.info <- like_info

  message("\nFinished loading Extra MCMC output\n")

  extra_mcmc
}
