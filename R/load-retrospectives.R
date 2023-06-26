#' Fetch the retrospectives and return a list of each. If there are no retrospective
#' directories or there is some other problem, the program will halt
#'
#' @param retro_path The path in which the retrospective directories reside
#' @param ... Arguments passed to [load_ss_files()] and [load_extra_mcmc()]
#'
#' @return The list of retrospective outputs
#' @export
load_retrospectives <- function(model,
                                base_model_name = "Base model",
                                ...){

  retro_path <- model$retrospectives_path

  if(!dir.exists(retro_path)){
    warning("The retrospectives directory `", retro_path, "` does not ",
            "exist. Not loading any retrospectives.")
    return(NA)
  }

  fns <- dir(retro_path)
  pat <- "^retro-([0-9]+)$"
  num_fns_match <- length(grep(pat, fns))
  if(!num_fns_match){
    stop("There were no subdirectories in the `", retro_path, "` directory ",
         "that matched the pattern ", pat,
         call. = FALSE)
  }
  retro_yrs <- sort(as.numeric(gsub(pat, "\\1", fns)))

  message("Loading retrospectives from ", retro_path)

  # Cannot use parallel here because some list items are missing
  retros_lst <- imap(retro_yrs, \(x, y, ...){
    # Pad the beginning of a digit with a zero
    pad_zero <- \(num){
      num <- as.character(num)
      if(nchar(num) == 1){
        paste0("0", num)
      }else{
        num
      }
    }
    retro_sub <- paste0("retro-", pad_zero(x))
    retro_dir <- file.path(retro_path, retro_sub)
    message("Loading from ", retro_dir)
    model <- load_ss_files(retro_dir, ...)
    model$extra_mcmc <- load_extra_mcmc(model, ...)
    model$endyr <- model$endyr - y
    model$mcmc <- NULL
    model$parameters <- NULL
    model
  }, ...)

  # Retrospectives for plotting squid plots (all retros) ----
  # Base model is the first model, followed by all the retrospectives
  # available in the directory
  all_retros_lst <- c(list(model), retros_lst)
  # Extract the cohort list from the list of models
  all_cohorts <- map_dbl(all_retros_lst, \(mdl){
    mdl$endyr
  }) |>
    sort()
  all_retros_yrs <- rev(all_cohorts) + 1

  # Retrospectives for plotting sensitivity-type plots (subset of retros) ----
  # Base model is the first model, followed by the retrospectives indexed by
  # the hake::plot_retro_yrs vector of indices (1 = -1 years, 2 = -2 years,
  # etc)
  subset_retros_lst <- all_retros_lst[c(1, hake::plot_retro_yrs + 1)]
  subset_retros_model_nms <- c(base_model_name,
                              map_chr(hake::plot_retro_yrs, \(yr_ind){
                                paste0(" -",
                                       yr_ind,
                                       " year",
                                       ifelse(yr_ind == 1,
                                              "",
                                              "s"))
                              }))

  # Extract a data frame of long-format recruitment deviations containing all
  # the models in the model list (base mode plus all retrospectives)
  retro_recdevs_df <- create_group_df_recr(all_retros_lst,
                                           all_retros_yrs,
                                           devs = TRUE)

  retro_biomass_df <-
    create_group_df_biomass(subset_retros_lst,
                            subset_retros_model_nms)
  retro_rel_biomass_df <-
    create_group_df_biomass(subset_retros_lst,
                            subset_retros_model_nms,
                            rel = TRUE)
  retro_recr_df <-
    create_group_df_recr(subset_retros_lst,
                         subset_retros_model_nms)

  retro_param_est <- get_param_est_comparison_df(subset_retros_lst,
                                                 subset_retros_model_nms,
                                                 ...)

  retros_lst <- list(recdevs_df = retro_recdevs_df,
                     biomass_df = retro_biomass_df,
                     rel_biomass_df = retro_rel_biomass_df,
                     recr_df = retro_recr_df,
                     retro_param_est = retro_param_est)

  message("Finished loading retrospectives")
  retros_lst
}
