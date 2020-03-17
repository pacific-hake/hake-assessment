
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
#'
#' @examples
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
  posts <- read_table2(file.path(model$mcmcpath, posts_file_name))
  # Remove all extra columns that start with X. These represent extra whitespace at end of header row in file
  postsxgrep <- grep("^X", names(posts))
  if(length(postsxgrep)){
    posts <- posts %>%
      select(-postsxgrep)
  }
  post_lst <- split_df(posts, from_to)
  derposts <- read_table2(file.path(model$mcmcpath, derposts_file_name))
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
#' @param model The model object as output by [load_ss_files()]
#'
#' @return
#' @export
fetch_extra_mcmc <- function(model_path,
                             ...){

  model <- load_ss_files(model_path, ...)
  mcmc_path <- model$mcmcpath
  extra_mcmc_path <- file.path(model_path, extra_mcmc_path)
  if(!dir.exists(extra_mcmc_path)){
    message("The ", extra_mcmc_path, " directory does not exist, so the extra-mcmc wass not loaded")
    return(NA)
  }
  reports_dir <- file.path(extra_mcmc_path, extra_mcmc_reports_path)
  if(!dir.exists(reports_dir)){
    message("The ", reports_dir, " directory does not exist, so the extra-mcmc wass not loaded")
    return(NA)
  }

  ## Get the extra-mcmc directories done in parallel
  extra_mcmc_dirs <- dir(extra_mcmc_path)
  extra_mcmc_dirs <- file.path(extra_mcmc_path, extra_mcmc_dirs[grepl("extra-mcmc", extra_mcmc_dirs)])

  ## Get the number of Report.sso files in the directory
  dir_list <- dir(reports_dir)
  if(!length(dir_list)){
    message("There are no report files in the ", reports_dir, " directory.")
    return(NA)
  }
  report_files <- grep("^Report_[[:digit:]]+\\.sso$", dir_list)
  num_reports <- length(report_files)
  comp_files <- grep("^CompReport_[[:digit:]]+\\.sso$", dir_list)
  num_comp_reports <- length(comp_files)
  message("\nLoading Extra MCMCs from ", extra_mcmc_path)
  posts <- read_table2(file.path(mcmc_path, posts_file_name))
  ## Remove extra MLE run outputs. SS appends a new header followed by a 0-Iter row for an MLE run.
  ## Sometimes MLEs are run by accident or on purpose at another time and forgotten about.
  posts <- posts %>% filter(Iter != "Iter",
                            Iter != 0)

  ## Break up the loading of report files into the number of posteriors in each extra-mcmc subdir
  plan("multisession")
  num_reports_each <- future_map_int(extra_mcmc_dirs, ~{
    posts <- read_table2(file.path(.x, posts_file_name))
    posts <- posts %>% filter(Iter != "Iter",
                              Iter != 0)
    nrow(posts)
  })
  plan()

  ## from_to is a two-column dataframe with the indices from and to for each processor to load report files
  from_to <- tibble(from = 1, to = num_reports_each[1])
  for(i in 2:length(num_reports_each)){
    nxt <- tibble(from = from_to[i - 1,]$to + 1, to = from_to[i - 1,]$to + num_reports_each[i])
    from_to <- bind_rows(from_to, nxt)
  }

  ## Load all report files into a list, 1 element for each report file. Elements that are NA had no file found in the
  plan("multisession")
  reps <- future_map(1:nrow(from_to), ~{
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
  plan()

  ## Make custom reps_ objects for each output. Only relevant portions of the report file will be passed to
  ## the table-making map2() calls later (speeds up the map2() calls)
  rep_example <- reps[[which(!is.na(reps))[1]]]
  ## Biomass
  bio_header_text <- "^TIME_SERIES"
  bio_header_ind <- grep(bio_header_text, rep_example) + 1
  bio_header_line <- rep_example[bio_header_ind]
  bio_header <- str_split(bio_header_line, " +")[[1]]
  bio_end_text <- "^SPR_series"
  bio_start_ind <- bio_header_ind + 1
  bio_end_ind <- grep(bio_end_text, rep_example) - 2
  reps_bio <- map(reps, ~{.x[bio_start_ind:bio_end_ind]})
  # Likelihood
  like_start_ind <- grep("^LIKELIHOOD", rep_example) + 1
  like_end_ind <- like_start_ind + 17
  reps_like <- map(reps, ~{.x[like_start_ind:like_end_ind]})
  ## Selectivity
  next_yr <- model$endyr + 1
  sel_header_text <- "Factor Fleet Yr Seas"
  sel_header_ind <- grep(sel_header_text, rep_example)
  sel_header_line <- rep_example[sel_header_ind]
  sel_header <- str_split(sel_header_line, " +")[[1]]
  sel_text <- paste0(next_yr, "_1Asel")
  sel_ind <- grep(sel_text, rep_example)
  reps_sel <- map(reps, ~{.x[sel_ind]})
  ## Selectivity * Weight
  selwt_text <- paste0(next_yr, "_1_sel\\*wt")
  selwt_ind <- grep(selwt_text, rep_example)
  reps_selwt <- map(reps, ~{.x[selwt_ind]})
  ## Natage
  natage_start_text <- "NUMBERS_AT_AGE_Annual_2 With_fishery"
  natage_end_text <- "Z_AT_AGE_Annual_2 With_fishery"
  natage_header_ind <- grep(natage_start_text, rep_example) + 1
  natage_header <- str_split(rep_example[natage_header_ind], " +")[[1]]
  natage_start_ind <- grep(natage_start_text, rep_example) + 2
  natage_end_ind <- grep(natage_end_text, rep_example) - 2
  reps_natage <- map(reps, ~{.x[natage_start_ind:natage_end_ind]})
  ## Q
  q_start_text <- "^INDEX_2"
  q_header_ind <- grep(q_start_text, rep_example) + 1
  q_header <- str_split(rep_example[q_header_ind], " +")[[1]]
  q_start_ind <- grep(q_start_text, rep_example) + 2
  ncpue <- nrow(model$dat$CPUE)
  q_end_ind <- q_start_ind + ncpue - 1
  reps_q <- map(reps, ~{.x[q_start_ind:q_end_ind]})

  extract_rep_table <- function(reps_lst, header){
    lst <- map2(reps_lst, 1:length(reps_lst), ~{
      if(is.na(.x[1])){
        #return(c(.y, rep(NA, 100)))
        return(NULL)
      }
      tab <- do.call(rbind, str_split(.x, " +")) %>%
        as_tibble()
      names(tab) <- header
      tab %>%
        add_column(Iter = .y, .before = 1)
    })
    do.call(rbind, lst) %>%
      as_tibble()
  }

  ## Don't parallelize the following at all. It's slower than serial.
  out <- map(1:6, ~{
    if(.x == 1){
      extract_rep_table(reps_bio, bio_header) %>%
        select(Iter, Bio_all, Bio_smry)
    }else if(.x == 2){
      likes <- map2(reps_like, 1:length(reps_like), ~{
        if(is.na(.x[1])){
          return(NULL)
        }
        likes <- map(str_split(.x, " +"), ~{.x[1:4]})
        do.call(rbind, likes) %>%
          as_tibble() %>%
          filter(!grepl("^#_", V1)) %>%
          add_column(Iter = .y, .before = 1)
      })
      do.call(rbind, likes) %>%
        as_tibble()
    }else if(.x == 3){
      extract_rep_table(reps_sel, sel_header)
    }else if(.x == 4){
      extract_rep_table(reps_selwt, sel_header)
    }else if(.x == 5){
      extract_rep_table(reps_natage, natage_header)
    }else if(.x == 6){
      extract_rep_table(reps_q, q_header)
    }
  })

  biomass <- out[[1]]
  like <- out[[2]]
  sel <- out[[3]] %>%
    select(-(2:8)) %>%
    map_df(as.numeric)
  selwt <- out[[4]] %>%
    select(-(2:8)) %>%
    map_df(as.numeric)
  natage <- out[[5]] %>%
    select(-(2:4)) %>%
    map_df(as.numeric)
  q <- out[[6]]

  ## Calculate the natage with selectivity applied and proportions
  apply_sel <- function(natage, sel){
    nat <- natage %>%
      group_by(Iter) %>%
      group_nest()
    sw <- sel %>%
      group_by(Iter) %>%
      group_nest()
    natsel <- nat %>%
      left_join(sw,  by = "Iter")
    iter <- natsel$Iter
    map2(natsel$data.x, natsel$data.y, ~{
      sweep(.x, 2, .y %>% unlist(., use.names = FALSE), FUN = "*") %>%
        as_tibble()
    }) %>%
      map2_df(iter, ~{
        .x %>% add_column(Iter = .y, .before = 1)
      })
  }

  natsel <- apply_sel(natage, sel)
  natselwt <- apply_sel(natage, selwt)
  browser()

  # plan("multisession")
  # like_info <- map(1:nrow(from_to), ~{
  #   inds <- as.numeric(from_to[.x, 1]):as.numeric(from_to[.x, 2])
  #   ## Data frame to store likelihood components
  #   like_info <- tibble(Iter = posts$Iter,
  #                       TOTAL = 0,
  #                       Equil_catch = 0,
  #                       Survey = 0,
  #                       Age_comp = 0,
  #                       Recruitment = 0,
  #                       Forecast_Recruitment = 0,
  #                       Parm_priors = 0,
  #                       Parm_devs = 0,
  #                       Crash_Pen = 0,
  #                       Age_comp_surv = 0,
  #                       Age_comp_fishery = 0)

  browser()


  ## selected biomass by age is product of numbers*selectivity*weight at each age
  natselwt <- natage_table_slim * selwt_table_slim
  ## selected numbers by age is product of numbers*selectivity at each age
  natsel <- natage_table_slim * sel_table_slim

  ## define new objects to store proportions by age
  natsel_prop <- natsel
  natselwt_prop <- natselwt

  ## create tables of proportions by dividing by sum of each row
  for(irow in 1:num_reports){
    natsel_prop[irow,] <- natsel[irow,]/sum(natsel[irow,])
    natselwt_prop[irow,] <- natselwt[irow,]/sum(natselwt[irow,])
  }

  ## read expected proportions and Pearson values for each age comp observations
  tmp <- readLines(file.path(reports_dir, paste0("CompReport_", irow,".sso")))
  skip_row <- grep("Composition_Database", tmp)
  comp_table <- read.table(file.path(extra_mcmc_path, compreport_file_name),
                           skip = skip_row,
                           header = TRUE,
                           fill = TRUE,
                           stringsAsFactors = FALSE)

  ## loop to create columns Exp1, Exp2, ..., Exp999 and Pearson1, Pearson2, etc.
  for(irow in 1:num_comp_reports){
    if(irow %% 100 == 0){
      print(irow)
    }
    tmp <- readLines(file.path(reports_dir, paste0("CompReport_", irow,".sso")))
    skip_row <- grep("Composition_Database", tmp)
    comps <- read.table(file.path(reports_dir, paste0("CompReport_", irow, ".sso")),
                        skip = skip_row,
                        header = TRUE,
                        fill = TRUE,
                        stringsAsFactors = FALSE)
    lab1 <- paste0("Pearson", irow)
    lab2 <- paste0("Exp", irow)
    comp_table[lab1] <- comps$Pearson
    comp_table[lab2] <- comps$Exp
  }
  ## filter out values that are not included in agedbase within base model
  comp_table <- comp_table[!is.na(comp_table$N) & comp_table$N > 0,]

  ## median and quantiles of expected values and Pearsons
  exp_table <- comp_table[,names(comp_table) %in% paste0("Exp", 1:num_comp_reports)]
  Pearson_table <- comp_table[,names(comp_table) %in% paste0("Pearson", 1:num_comp_reports)]
  exp_median <- apply(exp_table, MARGIN = 1, FUN = median)
  exp_low <- apply(exp_table, MARGIN = 1, FUN = quantile, probs = 0.025)
  exp_high <- apply(exp_table, MARGIN = 1, FUN = quantile, probs = 0.975)
  Pearson_median <- apply(Pearson_table, MARGIN = 1, FUN = median)
  Pearson_low <- apply(Pearson_table, MARGIN = 1, FUN = quantile, probs = 0.025)
  Pearson_high <- apply(Pearson_table, MARGIN = 1, FUN = quantile, probs = 0.975)

  # get index fits from CPUE table
  # cpue_table <- NULL
  # Q_vector <- NULL
  # for(irow in 1:num_reports){
  #   if(irow %% 100 == 0){
  #     print(irow)
  #   }
  #   tmp <- readLines(file.path(reports_dir, paste0("Report_", irow,".sso")))
  #   skip_row <- grep("INDEX_2", tmp)[2]
  #   # number of CPUE values includes dummy values for in-between years
  #   # reading these values is needed to get expected survey biomass in those years
  #   ncpue <- nrow(model$dat$CPUE)
  #   cpue <- read.table(file.path(reports_dir, paste0("Report_", irow,".sso")),
  #                      skip = skip_row,
  #                      nrows = ncpue, ## number of survey index points
  #                      header = TRUE,
  #                      fill = TRUE,
  #                      stringsAsFactors = FALSE)
  #   lab1 <- paste0("Exp", irow)
  #   cpue_table <- cbind(cpue_table, cpue$Exp)
  #   Q_vector <- c(Q_vector, cpue$Calc_Q[1]) # values are the same for all rows
  # }

  ## Build the list of extra mcmc outputs and return
  extra_mcmc <- model

  ## add information on posterior distribution to existing agedbase data frame
  extra_mcmc$agedbase$Exp <- exp_median
  extra_mcmc$agedbase$Exp.025 <- exp_low
  extra_mcmc$agedbase$Exp.975 <- exp_high
  extra_mcmc$agedbase$Pearson <- Pearson_median
  extra_mcmc$agedbase$Pearson.025 <- Pearson_low
  extra_mcmc$agedbase$Pearson.975 <- Pearson_high

  ## add new table to output containing info on posterior distribution of index fits
  extra_mcmc$cpue.table <- cpue_table
  extra_mcmc$cpue.median <- apply(cpue_table, MARGIN = 1, FUN = median)
  extra_mcmc$cpue.025 <- apply(cpue_table, MARGIN = 1, FUN = quantile, probs = 0.025)
  extra_mcmc$cpue.975 <- apply(cpue_table, MARGIN = 1, FUN = quantile, probs = 0.975)
  extra_mcmc$Q_vector <- Q_vector

  ## add new table of info on posterior distributions of likelihoods
  extra_mcmc$like.info <- like_info

  ## add new table vectors containing expected proportions in first forecast year
  extra_mcmc$natsel.prop <- natsel_prop
  extra_mcmc$natselwt.prop <- natselwt_prop

  ## add info on distribution of total biomass to existing time series data frame
  extra_mcmc$timeseries$Bio_all <- apply(Bio_all, MARGIN = 1, FUN = median)
  extra_mcmc$timeseries$Bio_all.0.025 <- apply(Bio_all, MARGIN = 1,
                                               FUN = quantile, probs = 0.025)
  extra_mcmc$timeseries$Bio_all.0.975 <- apply(Bio_all, MARGIN = 1,
                                               FUN = quantile, probs = 0.975)
  extra_mcmc$timeseries$Bio_smry <- apply(Bio_smry, MARGIN = 1, FUN = median)
  extra_mcmc$timeseries$Bio_smry.0.025 <- apply(Bio_smry, MARGIN = 1,
                                                FUN = quantile, probs = 0.025)
  extra_mcmc$timeseries$Bio_smry.0.975 <- apply(Bio_smry, MARGIN = 1,
                                                FUN = quantile, probs = 0.975)

  message("Finished loading Extra MCMC data\n")

  extra_mcmc
}
