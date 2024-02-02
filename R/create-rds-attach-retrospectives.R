#' Load retrospective RDS files and attach to as RDS file
#'
#' @details
#' Load retrospectives and attach it to an already-created RDS file as the list
#' element `retrospectives` and overwrite the RDS file
#'
#' @param model_path Directory name of model to be loaded
#' @param verbose Logical. If `TRUE`, write more output to the console
#' @param base_model_name The name to show in retrospective comparison plots
#' for the base model
#' @param ... Passed to [check_catch_levels()]
#'
#' @return Nothing
#' @export
create_rds_attach_retrospectives <- function(model_path = NULL,
                                             verbose = TRUE,
                                             base_model_name = "Base model",
                                             ...){

  # Check for RDS file existence
  if(is.null(model_path)){
    stop("`model_path` must be supplied")
  }

  if(length(grep("\\/$", model_path))){
    # Remove trailing slashes
    model_path <- gsub("\\/+$", "", model_path)
  }

  if(!dir.exists(model_path)){
    stop("Model directory `", model_path, "` does not exist")
  }

  # The RDS file will have the same name as the directory it is in
  rds_file <- file.path(model_path, paste0(basename(model_path), ".rds"))
  if(!file.exists(rds_file)){
    stop("The RDS file `", rds_file,"` does not exist. You must create it ",
         "first by running the `create_rds_file()` function")
  }

  model <- readRDS(rds_file)

  # Check for retrospective RDS files
  retro_path <- file.path(model_path, retropectives_path)
  if(!dir.exists(retro_path)){
    stop("The restrospectives directory\n`", retro_path, "`\ndoes not exist")
  }


  retro_subdirs <- dir(retro_path)
  rds_fns <- paste0(retro_subdirs, ".rds")
  rds_fns <- file.path(retro_path, retro_subdirs, rds_fns)

  if(!all(file.exists(rds_fns))){
    stop("One or more of the retrospective RDS files does not exist. You ",
         "must run `create_rds_files_retro()` to create them before running ",
         "this function")
  }

  all_retros_lst <- map(rds_fns, \(fn){
    readRDS(fn)
  })
  # Append the base model to this list
  all_retros_lst <- c(list(model), all_retros_lst)

  all_cohorts <- map_dbl(all_retros_lst, \(mdl){
    mdl$Retro_year
  }) |>
    sort()
  all_retros_yrs <- rev(all_cohorts) + 1
  plot_retros_yrs <- all_retros_yrs[c(1, plot_retro_yrs + 1)]

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
                                           devs = TRUE,
                                           end_yrs = all_retros_yrs)

  # `retro_biomass_df` and `retro_rel_biomass_df` are for plots showing
  #  biomass trajectories for retrospectives against the base model
  retro_biomass_df <-
    create_group_df_biomass(subset_retros_lst,
                            subset_retros_model_nms,
                            end_yrs = plot_retros_yrs)
  retro_rel_biomass_df <-
    create_group_df_biomass(subset_retros_lst,
                            subset_retros_model_nms,
                            rel = TRUE,
                            end_yrs = plot_retros_yrs)
  retro_recr_df <-
    create_group_df_recr(subset_retros_lst,
                         subset_retros_model_nms,
                         end_yrs = plot_retros_yrs)

  retro_param_est <- get_param_est_comparison_df(subset_retros_lst,
                                                 subset_retros_model_nms,
                                                 ...)

  retros_lst <- list(recdevs_df = retro_recdevs_df,
                     biomass_df = retro_biomass_df,
                     rel_biomass_df = retro_rel_biomass_df,
                     recr_df = retro_recr_df,
                     retro_param_est = retro_param_est)

  model[[retropectives_path]] <- retros_lst

  saveRDS(model, file = rds_file)
  if(file.exists(rds_file)){
    dt <- now() - file.info(rds_file)$mtime
    message("RDS file `", rds_file, "` was created ",
            f(dt[[1]], 2), " ", units(dt), " ago\n\n")
  }else{
    stop("File was not created during the `saveRDS()` call")
  }

  if(verbose){
    message("Finished attaching retrospectives")
  }

  invisible()
}