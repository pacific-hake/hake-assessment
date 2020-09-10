#' Build RDS file for a list of models by running forecasts, retrospectives, and extra-mcmc
#' where applicable
#'
#' @param model_dirs A vector of model directory names (not full paths)
#' @param run_forecasts Logical. TRUE to run forecasts on the models (when an mcmc directory is present)
#' @param run_retrospectives Logical. TRUE to run retrospectives on the models (when an mcmc directory is present)
#' @param run_extra_mcmc Logical. TRUE to run extra-mcmc routine on the models (when an mcmc directory is present)
#' @param run_catch_levels Logical. If TRUE, the catch levels calculations routine will run, overwriting any previous
#' @param catch_levels A list of lists for the forecast catch levels to run forecasts for. See
#' catch levels runs. If FALSE, catch levels will not be run if the catch-levels directory exists, but they will be run
#' if the directory does not exist and run_forecasts is TRUE because it is required for the forecasting step
#' forecast-catch-levels.R
#'
#' @return Nothing
#' @export
#' @importFrom purrr map
#'
#' @examples
#' build_rds(c("base", "nuts"), TRUE, TRUE, TRUE)
build_rds <- function(model_dirs = model_list,
                      run_forecasts = FALSE,
                      run_retrospectives = FALSE,
                      run_extra_mcmc = FALSE,
                      run_catch_levels = FALSE,
                      catch_levels = catch_levels,
                      ...){

  map(model_dirs, ~{
    if(run_forecasts |
       run_retrospectives |
       run_extra_mcmc |
       run_catch_levels){
      run(model_dir = .x,
          run_forecasts = run_forecasts,
          run_retrospectives = run_retrospectives,
          run_extra_mcmc = run_extra_mcmc,
          run_catch_levels = run_catch_levels,
          catch_levels = catch_levels)

    }
    create_rds_file(model_dir = .x, ...)
  }, ...)
  message("\nCompleted build.\n")
  invisible()
}

#' Create an rds file to hold the model's data and outputs.
#'
#' @param model_dir Directory name of model to be loaded
#'
#' @return [base::invisible()]
#' @export
create_rds_file <- function(model_dir = NULL, ...){

  stopifnot(!is.null(model_dir))

  model_fullpath <- file.path(rootd.models, model_dir)
  if(!dir.exists(model_fullpath)){
    stop("Error - the directory ", model_fullpath, " does not exist.\n",
         "Fix the problem and try again.", call. = FALSE)
  }

  # The RDS file will have the same name as the directory it is in
  rds_file <- file.path(model_fullpath, paste0(model_dir, ".rds"))
  if(file.exists(rds_file)){
    unlink(rds_file, force = TRUE)
  }

  message("Creating a new RDS file in ", model_fullpath, "\n")

  # If this point is reached, no RData file exists so it has to be built from scratch
  model <- load_ss_files(model_fullpath, ...)

  # Load forecasts. If none are found or there is a problem, model$forecasts will be NA
  if(dir.exists(file.path(model_fullpath, forecasts_path))){
    catch_levels_fullpath <- file.path(model_fullpath, catch_levels_path)
    model$catch.levels <- fetch_catch_levels(catch_levels_fullpath, catch_levels)
    model$catch.default.policy <- model$catch.levels[[catch.default.policy.ind]][[1]]
    model$forecasts <- fetch_forecasts(model_fullpath, model$catch.levels, ...)
    model$risks <- calc_risk(forecast_outputs = model$forecasts,
                             catch_levels = model$catch.levels,
                             forecast_yrs)
  }else{
    model$catch.levels <- NA
    model$catch.default.policy <- NA
    model$forecasts <- NA
    model$risks <- NA
  }

  # Load retrospectives. If none are found or there is a problem, model$retros will be NA
  model$retropath <- file.path(model_fullpath, retrospectives_path)
  if(dir.exists(model$retropath)){
    model$retros <- fetch_retrospectives(model$retropath,
                                         retrospective_yrs)
  }else{
    model$retros <- NA
  }

  # Try loading extra mcmc output. If none are found or there is a problem, model$extra.mcmc will be NA
  model$extra.mcmc.path <- file.path(model_fullpath, extra_mcmc_path)
  if(dir.exists(model$extra.mcmc.path)){
    model$extra.mcmc <- fetch_extra_mcmc(model$path)
  }else{
    model$extra.mcmc <- NA
  }

  saveRDS(model, file = rds_file)
  invisible()
}

#' Run extra models for forecasting, retrospectives, and extra MCMC (one report file per posterior)
#'
#' @details This is a wrapper function for calling [run_catch_levels()], [run_forecasts()],
#' [run_retrospectives()], and [run_extra_mcmc()] functions.
#'
#' @param model_dir The directory the models are located in
#' @param run_forecasts Logical. Run forecasting?
#' @param run_retrospectives Logical. Run restrospectives?
#' @param run_extra_mcmc Logical. Run extra-mcmc calculations?
#' @param run_catch_levels Logical. Run catch levels estimation routines?
#' @param ... Passed to the subroutines
#'
#' @return [base::invisible()]
#' @export
run <- function(model_dir = NULL,
                run_forecasts = FALSE,
                run_retrospectives = FALSE,
                run_extra_mcmc = FALSE,
                run_catch_levels = FALSE,
                ...){

  stopifnot(!is.null(model_dir))
  model_dir <- file.path(models_path, model_dir)

  if(!dir.exists(model_dir)){
    stop("The ", model_dir, " directory does not exist",
         call. = FALSE)
  }
  if(dir.exists(file.path(model_dir, "mcmc"))){
    if(run_forecasts){
      catch_levels_fullpath <- file.path(model_dir, catch_levels_path)
      if(!dir.exists(catch_levels_fullpath) | run_catch_levels){
        run_catch_levels(model_dir, ...)
      }
      run_forecasts(model_dir, ...)
    }
    if(run_retrospectives){
      run_retrospectives(model_dir, ...)
    }
    if(run_extra_mcmc){
      run_extra_mcmc(model_dir, ...)
    }
  }
}

#' Build the doc entirely from within R
#'
#' @details Make sure you have created the .RData files by sourcing *all.r* with the [create.rdata.file()] variables set to TRUE.
#' Once you have done that and run this function once within an R session, you can go into the first knitr code chunk in
#' hake-assessment.rnw and set the call to [load_models_rds()] to FALSE, which will save time for doing the build.
#'
#' @param knit.only Only knit the code, do not run latex
#' @param make.pdf Logical. TRUE to make the pdf, if FALSE it will only go as far as postscript
#' @param make.bib Logical. Run bibtex
#' @param doc.name What to name the dcument (no extension needed)
#'
#' @return [base::invisible()]
#' @export
build.doc <- function(knit.only = FALSE,
                      make.pdf  = TRUE,
                      make.bib  = TRUE,
                      doc.name  = "hake-assessment"){
  curr_path <- getwd()
  setwd(here::here("doc"))
  knit(paste0(doc.name,".rnw"))
  if(!knit.only){
    system(paste0("latex ", doc.name, ".tex"),
           invisible = FALSE,
           show.output.on.console = FALSE)
    if(make.bib){
      system(paste0("bibtex ", doc.name),
             invisible = FALSE,
             show.output.on.console = TRUE)
    }
    system(paste0("latex ", doc.name, ".tex"),
           invisible = FALSE,
           show.output.on.console = FALSE)
    system(paste0("latex ", doc.name, ".tex"),
           invisible = FALSE,
           show.output.on.console = FALSE)
    system(paste0("latex ", doc.name, ".tex"),
           invisible = FALSE,
           show.output.on.console = FALSE)
    system(paste0("latex ", doc.name, ".tex"),
           invisible = FALSE,
           show.output.on.console = FALSE)
    system(paste0("dvips ", doc.name,".dvi"),
           invisible = FALSE,
           show.output.on.console = TRUE)
    if(make.pdf){
      shell(paste0("ps2pdf ", doc.name, ".ps"))
    }
  }
  setwd(curr_path)
  invisible()
}
