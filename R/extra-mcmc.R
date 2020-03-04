
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
  posts <- read_table(file.path(model$mcmcpath, "posteriors.sso"))
  post_lst <- split_df(posts, from_to)
  derposts <- read_table(file.path(model$mcmcpath, "derived_posteriors.sso"))
  derpost_lst <- split_df(derposts, from_to)

  extra_mcmc_full_path <- file.path(model_path, extra_mcmc_path)
  dir.create(extra_mcmc_full_path, showWarnings = FALSE)
  unlink(file.path(extra_mcmc_full_path, "*"), recursive = TRUE)

  reports_path <- file.path(extra_mcmc_full_path, "reports")
  dir.create(reports_path, showWarnings = FALSE)
  unlink(file.path(reports_path, "*"), recursive = TRUE)

  future::plan("multisession")
  furrr::future_map(1:nrow(from_to), ~{
    run_extra_mcmc_chunk(model,
                         posts = post_lst[[.x]],
                         derposts = derpost_lst[[.x]],
                         extra_mcmc_path = extra_mcmc_path,
                         extra_mcmc_num = .x,
                         from_to = from_to[.x,],
                         ...)})
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

  ## change labels parameters like "SR_LN(R0)" to "SR_LN.R0."
  ## to match what read.table does to posteriors.sso
  newpar$label <- gsub(pattern = "(", replacement = ".", newpar$label, fixed = TRUE)
  newpar$label <- gsub(pattern = ")", replacement = ".", newpar$label, fixed = TRUE)
  ## Remove brackets in newpar labels so that the names match column names in posts
  ## this line may be redundant with the gsub commands above
  newpar$label <- gsub("\\(([0-9])\\)", ".\\1.", newpar$label)

  extra_mcmc_full_path <- file.path(model_path, extra_mcmc_path)
  reports_path <- file.path(extra_mcmc_full_path, "reports")

  sub_extra_mcmc_path <- file.path(extra_mcmc_full_path, paste0(extra_mcmc_path, "-", extra_mcmc_num))
  dir.create(sub_extra_mcmc_path, showWarnings = FALSE)
  unlink(file.path(sub_extra_mcmc_path, "*"), recursive = TRUE)
  ## Copy files into the subdirectory from the model/mcmc directory

  file.copy(file.path(mcmc_path, list.files(mcmc_path)), sub_extra_mcmc_path)
  write_csv(posts, file.path(sub_extra_mcmc_path, "posteriors.sso"))
  write_csv(derposts, file.path(sub_extra_mcmc_path, "derived_posteriors.sso"))

  write.table(x = newpar,
              file = file.path(sub_extra_mcmc_path, "ss.par"),
              quote = FALSE,
              row.names = FALSE)

  start <- SS_readstarter(file.path(sub_extra_mcmc_path, "starter.ss"), verbose = FALSE)
  ## Change starter file to read from par file
  start$init_values_src <- 1
  SS_writestarter(start, dir = sub_extra_mcmc_path, file = "starter.ss", overwrite = TRUE, verbose = FALSE)

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
    write.table(x = newpar,
                file = file.path(sub_extra_mcmc_path, "ss.par"),
                quote = FALSE,
                row.names = FALSE)
    ## delete existing output files to make sure that if model fails to run,
    ## it won't just copy the same files again and again
    file.remove(file.path(sub_extra_mcmc_path, "Report.sso"))
    file.remove(file.path(sub_extra_mcmc_path, "CompReport.sso"))

    shell_command <- paste0("cd ", sub_extra_mcmc_path, " & ", ss_executable, " -maxfn 0 -phase 10 -nohess")
    shell(shell_command, wait = FALSE, intern = TRUE)

    file.copy(file.path(sub_extra_mcmc_path, "ss.par"),
              file.path(reports_path, paste0("ss_output", from_to[irow], ".par")),
              overwrite = TRUE)
    file.copy(file.path(sub_extra_mcmc_path, "Report.sso"),
              file.path(reports_path, paste0("Report_", from_to[irow], ".sso")),
              overwrite = TRUE)
    file.copy(file.path(sub_extra_mcmc_path, "CompReport.sso"),
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
fetch_extra_mcmc <- function(model){

  extra_mcmc_path <- model$extra.mcmc.path
  reports_dir <- file.path(extra_mcmc_path, "reports")
  if(is.na(extra_mcmc_path)){
    return(NA)
  }
  if(!dir.exists(extra_mcmc_path)){
    return(NA)
  }
  if(!dir.exists(reports_dir)){
    return(NA)
  }

  extra_mcmc_path <- model$extra.mcmc.path
  ## Get the number of Report.sso files in the directory
  dir_list <- dir(reports_dir)
  if(!length(dir_list)){
    return(NA)
  }
  num_reports <- length(grep("^Report_[[:digit:]]+\\.sso$", dir_list))
  num_comp_reports <- length(grep("^CompReport_[[:digit:]]+\\.sso$", dir_list))
  posts <- read.table(file.path(extra_mcmc_path, "posteriors.sso"),
                      header = TRUE,
                      fill = TRUE,
                      stringsAsFactors = FALSE)
  message("\nLoading Extra MCMC data from ", extra_mcmc_path)

  ## Data frame to store likelihood components
  like_info <- data.frame(Iter = posts$Iter, stringsAsFactors = FALSE)
  for(lab in c("TOTAL",
               "Equil_catch",
               "Survey",
               "Age_comp",
               "Recruitment",
               "Forecast_Recruitment",
               "Parm_priors",
               "Parm_devs",
               "Crash_Pen",
               "Age_comp_surv",
               "Age_comp_fishery")){
    like_info[[lab]] <- 0
  }

  ## Objects to store selectivity, select*wt, and numbers at age
  sel_table <- NULL
  selwt_table <- NULL
  natage_table <- NULL

  ## unique strings associated with rows reporting selectivity and numbers at age
  sel_text1 <- paste0(model$endyr + 1, "_1Asel")
  sel_text2 <- paste0(model$endyr + 1, "_1_sel*wt")
  natage_text <- "Z_AT_AGE_Annual_2 With_fishery"

  ## Objects to store total biomass and age 2+ biomass (summary biomass)
  Bio_all <- NULL
  Bio_smry <- NULL

  ## loop over all report files to extract quantities
  for(irow in 1:num_reports){
    # read full report file as strings
    rep_file <- file.path(reports_dir, paste0("Report_", irow,".sso"))
    tmp <- readLines(rep_file)
    # find section on likelihoods and read as a table
    skip_row <- grep("LIKELIHOOD", tmp)[2]
    likes <- read.table(rep_file,
                        skip = skip_row,
                        nrows = 17,
                        fill = TRUE,
                        row.names = NULL,
                        col.names = 1:4,
                        stringsAsFactors = FALSE)
    # extract likelihoods from table and make numeric
    like_info[irow, 2:10] <- as.numeric(likes$X2[3:11])  ## fleet-aggregated likelihoods
    like_info[irow, 11] <- as.numeric(likes[17, 3])      ## fleet-specific age comp likelihoods
    like_info[irow, 12] <- as.numeric(likes[17, 4])      ## fleet-specific age comp likelihoods

    # find lines in report file containing unique strings related to selectivity
    sel_line1 <- grep(sel_text1, tmp)
    sel_line2 <- grep(sel_text2, tmp, fixed = TRUE)
    message("Loading report file: ", rep_file)
    # read individual rows of selectivity info
    sel_row1 <- read.table(file = rep_file, skip = sel_line1 - 1, nrow = 1)
    sel_row2 <- read.table(file = rep_file, skip = sel_line2 - 1, nrow = 1)

    # read numbers at age table based on start and end lines and length of table
    natage_line_start <- grep("NUMBERS_AT_AGE_Annual_2 With_fishery", tmp)
    natage_line_end <- grep("Z_AT_AGE_Annual_2 With_fishery", tmp)-3
    natage_N_lines <- natage_line_end - natage_line_start
    natage_allrows <- read.table(file = rep_file, skip = natage_line_start,
                                 nrow = natage_N_lines, header = TRUE)
    ## subset all rows to select first forecast year
    nms <- colnames(natage_allrows)
    nms[nms == "Year"] <- "Yr"
    colnames(natage_allrows) <- nms
    natage_row <- natage_allrows[natage_allrows$Yr == model$endyr + 1,]

    # add rows to tables of values for each MCMC sample
    sel_table <- rbind(sel_table, sel_row1)
    selwt_table <- rbind(selwt_table, sel_row2)
    natage_table <- rbind(natage_table, natage_row)

    # read time series table to get total biomass
    # (in the future we could add more things from the timeseries table)
    ts_start <- grep("^TIME_SERIES", tmp) + 1 # row with header
    ts_end <- grep("^SPR_series", tmp) - 2 # final row
    ts <- read.table(rep_file, header = TRUE, skip = ts_start - 1, nrows = ts_end - ts_start)
    Bio_all <- cbind(Bio_all, ts$Bio_all)
    Bio_smry <- cbind(Bio_smry, ts$Bio_smry)
  }

  ## Make sure the number of rows matches the number of posteriors
  like_info <- like_info[like_info$Equil_catch != 0 &
                           like_info$Survey !=0 &
                           like_info$Age_comp != 0 &
                           like_info$Recruitment != 0 &
                           like_info$Parm_priors != 0,]

  ## Process selectivity values
  ## remove initial columns (containing stuff like Gender and Year)
  natage_table_slim <- natage_table[,-(1:3)]
  sel_table_slim <- sel_table[,-(1:7)]
  selwt_table_slim <- selwt_table[,-(1:7)]

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
  comp_table <- read.table(file.path(extra_mcmc_path, "CompReport.sso"),
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
  cpue_table <- NULL
  Q_vector <- NULL
  for(irow in 1:num_reports){
    if(irow %% 100 == 0){
      print(irow)
    }
    tmp <- readLines(file.path(reports_dir, paste0("Report_", irow,".sso")))
    skip_row <- grep("INDEX_2", tmp)[2]
    # number of CPUE values includes dummy values for in-between years
    # reading these values is needed to get expected survey biomass in those years
    ncpue <- nrow(model$dat$CPUE)
    cpue <- read.table(file.path(reports_dir, paste0("Report_", irow,".sso")),
                       skip = skip_row,
                       nrows = ncpue, ## number of survey index points
                       header = TRUE,
                       fill = TRUE,
                       stringsAsFactors = FALSE)
    lab1 <- paste0("Exp", irow)
    cpue_table <- cbind(cpue_table, cpue$Exp)
    Q_vector <- c(Q_vector, cpue$Calc_Q[1]) # values are the same for all rows
  }

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
