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
                      ...){
  tic()
  map(model_dirs, function(x = .x, ...){
    run(model_dir = x, catch_levels_path = catch_levels_path, catch_levels = catch_levels, ...)
    create_rds_file(model_dir = x, catch_levels = catch_levels, ...)
  }, ...)
  message("\nCompleted build.\n")
  invisible()
  toc()
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
      run_forecasts(model_dir, catch_levels_path, ...)
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
#' @details Make sure you have created the .rds files by running [build_rds()] in the appropriate manner.
#' Once you have done that and run this function once within an R session, it will be much quicker since the rds
#' file contents will have already been loaded into the R session.
#'
#' @param knit_only Only knit the code, do not run latex or pdflatex.
#' @param make_pdf Logical. TRUE to make the pdf, if FALSE it will only go as far as postscript. If `use_pdflatex`
#' is set to TRUE, this argument will have no effect, a PDF will be built anyway.
#' @param make_bib Logical. Run bibtex.
#' @param png_figs Logical. If TRUE, use the pdflatex command to build (e.g. if pngs are being used for figures).
#' If FALSE, use latex, dvips, and ps2pdf to build (e.g. if pdf figures are being used)
#' @param doc_name What to name the document (no extension needed)
#'
#' @return [base::invisible()]
#' @export
build_doc <- function(knit_only = FALSE,
                      make_pdf = TRUE,
                      make_bib = TRUE,
                      png_figs = TRUE,
                      doc_name = "hake-assessment",
                      sty_name = "hake.sty"){

  latex_command <- ifelse(png_figs, "pdflatex", "latex")

  # Re-write order of figures in hake style file
  hake_sty <- readLines(sty_name)
  j <- grep("DeclareGraphicsExtensions", hake_sty)
  k <- grep(".jpeg,.JPEG\\}", hake_sty)
  if(j >= k || (k - j != 5)){
    stop("Could not find the correct entry for DeclareGraphicsExtensions in hake.sty",
         call. = FALSE)
  }
  if(png_figs){
    hake_sty[j + 1] <- ".png,.PNG,"
    hake_sty[j + 2] <- ".pdf,.PDF,"
    hake_sty[j + 3] <- ".eps,.EPS,"
    hake_sty[j + 4] <- ".jpg,.JPG,"
    hake_sty[j + 5] <- ".jpeg,.JPEG}"
  }else{
    hake_sty[j + 1] <- ".pdf,.PDF,"
    hake_sty[j + 2] <- ".eps,.EPS,"
    hake_sty[j + 3] <- ".png,.PNG,"
    hake_sty[j + 4] <- ".jpg,.JPG,"
    hake_sty[j + 5] <- ".jpeg,.JPEG}"
  }
  writeLines(hake_sty, sty_name)

  # Set dev in knitr options in assessment file for PNG or EPS figures
  hake_assess <- readLines(paste0(doc_name, ".rnw"))
  j <- grep("opts_chunk\\$set", hake_assess)
  if(!length(grep("dev", hake_assess[j + 1]))){
    stop("The line after opts_chunk$set in ", paste0(doc_name, ".rnw"), " does not contain dev=",
         call. = FALSE)
  }
  if(png_figs){
    hake_assess[j + 1] <- "dev = 'png',"
  }else{
    hake_assess[j + 1] <- "dev = 'cairo_ps',"
  }
  writeLines(hake_assess, paste0(doc_name, ".rnw"))

  curr_path <- getwd()
  setwd(here::here("doc"))
  knit(paste0(doc_name, ".rnw"))
  if(!knit_only){
    shell(paste0(latex_command, " ", doc_name, ".tex"))
    if(png_figs){
      shell(paste0(latex_command, " ", doc_name, ".tex"))
    }
    if(make_bib){
      shell(paste0("bibtex ", doc_name))
    }
    shell(paste0(latex_command, " ", doc_name, ".tex"))
    shell(paste0(latex_command, " ", doc_name, ".tex"))
    if(!png_figs){
      shell(paste0(latex_command, " ", doc_name, ".tex"))
      shell(paste0(latex_command, " ", doc_name, ".tex"))
      shell(paste0(latex_command, " ", doc_name, ".tex"))
      shell(paste0("dvips ", doc_name, ".dvi"))
      shell(paste0("ps2pdf ", doc_name, ".ps"))
    }
  }
  setwd(curr_path)
  invisible()
}

#' Build a pared-down version of the assessment. Typically used to compile figures section or
#' tables section only. Citations and other references will not be compiled properly since
#' pdflatex is only called once
#'
#' @param doc_name What to name the document (no extension needed)
#'
#' @return [base::invisible()]
#' @export
build_test <- function(doc_name = "hake-assessment-test"){
  curr_path <- getwd()
  setwd(here::here("doc"))
  knit(paste0(doc_name, ".rnw"))
  shell(paste0("pdflatex ", doc_name, ".tex"))
  setwd(curr_path)
  invisible()
}
