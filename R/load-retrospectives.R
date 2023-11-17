#' Fetch the retrospectives and return a list of each. If there are no retrospective
#' directories or there is some other problem, the program will halt
#'
#' @param model The SS3 model output as loaded by [load_ss_files()]
#' @param model_path The directory containing the model
#' @param ... Arguments passed to [load_ss_files()] and [load_extra_mcmc()]
#'
#' @return The list of retrospective outputs
#' @export
load_retrospectives <- function(model,
                                model_path = NULL,
                                base_model_name = "Base model",
                                verbose = TRUE,
                                ...){

  if(is.null(model)){
    if(is.null(model_path)){
      stop("Either `model` or `model_path` must be supplied")
    }
    model <- load_ss_files(model_path, ...)
  }else{
    if(is.null(model_path)){
      model_path <- model$path
    }else{
      if(model$path != model_path){
        stop("You provided both `model` and `model_path` and `model$path` ",
             "does not math `model_path`")
      }
    }
  }

  retro_fullpath <- file.path(model_path, retropectives_path)
  if(!dir.exists(retro_fullpath)){
    if(verbose){
      warning("The retrospectives directory `", retro_fullpath, "` does not ",
              "exist. Not loading any retrospectives.")
    }
    return(NA)
  }

  fns <- dir(retro_fullpath)
  pat <- paste0("^", retrospectives_prepend, "([0-9]+)$")
  num_fns_match <- length(grep(pat, fns))
  if(!num_fns_match){
    stop("There were no subdirectories in the `", retro_fullpath,
         "`\ndirectory that matched the pattern ", pat)
  }
  retro_yrs <- sort(as.numeric(gsub(pat, "\\1", fns)))

  if(verbose){
    message("Loading retrospectives from ", retro_fullpath)
  }

  plan("multicore", workers = length(retro_yrs))
  # Cannot use parallel here because some list items are missing
  retros_lst <- furrr::future_imap(retro_yrs, \(x, y, ...){
    # Pad the beginning of a digit with a zero
    retro_sub <- paste0("retro-", pad_num(x, n_digits = 2, pad_char = "0"))
    retro_dir <- file.path(retro_fullpath, retro_sub)
    if(verbose){
      message("Loading from ", retro_dir)
    }
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
  subset_retros_lst <- all_retros_lst[c(1, plot_retro_yrs + 1)]
  subset_retros_model_nms <- c(base_model_name,
                              map_chr(plot_retro_yrs, \(yr_ind){
                                paste0(" -",
                                       yr_ind,
                                       " year",
                                       ifelse(yr_ind == 1,
                                              "",
                                              "s"))
                              }))

  # Extract a data frame of long-format recruitment deviations containing all
  # the models in the model list (base mode plus all retrospectives)
  # `retro_recdevs_df` is for the squid plots
  retro_recdevs_df <- create_group_df_recr(all_retros_lst,
                                           all_retros_yrs,
                                           devs = TRUE)

  # `retro_biomass_df` and `retro_rel_biomass_df` are for plots showing
  #  biomass trajectories for retrospectives against the base model
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

  if(verbose){
    message("Finished loading retrospectives")
  }

  retros_lst
}
