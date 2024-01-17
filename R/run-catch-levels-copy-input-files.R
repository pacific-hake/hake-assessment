#' Copy the necessary files from a models' work directories to the directory
#' required to run dynamic catch levels runs
#'
#' @param model The SS model output as loaded by [create_rds_file()]
#' @param pth The path to the directory in which the dynamic catch levels
#' run will occur. Example:
#' '/srv/hake/models/2023/01-version/01-base-models/01-base/catch-levels/spr-100'
#' @param inp_files_only Logical. If `TRUE`, on;ly copy the SS3 inputs files
#' which are in the package data variable [ss_input_files]. If `FALSE`,
#' include 'derposts,sso' and 'ss.psv' as well
#'
#' @return [base::invisible()] If the function returns, all files were copied
#' successfully. If they were not, an error is thrown
run_catch_levels_copy_input_files <- function(model,
                                              pth,
                                              inp_files_only = FALSE){

  dir.create(pth, showWarnings = FALSE)
  file_chmod(pth, output_permissions)
  # Delete any old files/subdirectories in `default-hr` directory that may
  # still exist from a previous run
  unlink(file.path(pth, "*"), recursive = TRUE)

  if(is.null(model$mcmc_path)){
    stop("The model mcmc path is not set. `model$mcmc_path` is `NULL`")
  }

  # SS3 input files (5 files) ----
  files <- list.files(model$mcmc_path)
  if(!all(ss_input_files %in% files)){
    stop("At least one SS3 input file missing in directory\n`", pth,
         "`\nThe missing file(s) are:\n",
         paste(ss_input_files[!ss_input_files %in% files], collapse = "\n"))
  }
  copy_flag <- file.copy(file.path(model$path,
                                   ss_input_files),
                         file.path(pth, ss_input_files),
                         copy.mode = TRUE)
  if(!all(copy_flag)){
    stop("At least one SS3 imput file failed to copy from directory\n`",
         model$path, "` to directory\n`", pth,
         "`.\nThe file(s) not copied are:\n",
         paste(ss_input_files[!copy_flag], collapse = "\n"))
  }

  if(inp_files_only){
    return(invisible())
  }

  if(is.null(model$extra_mcmc_exists)){
    stop("The model 'extra mcmc path exists' is not set. ",
         "`model$extra_mcmc_exists` is `NULL`")
  }
  if(model$extra_mcmc_exists){
    if(is.null(model$extra_mcmc_path)){
      stop("The model 'extra mcmc path' is not set. `model$extra_mcmc_path`
           is `NULL`")
    }
  }

  # Derived posteriors file ----
  src_derposts_fn <- file.path(ifelse(model$extra_mcmc_exists,
                                      model$extra_mcmc_path,
                                      model$mcmc_path),
                               derposts_fn)
  dest_derposts_fn <- file.path(pth, derposts_fn)
  if(!file.exists(src_derposts_fn)){
    stop("The file:\n`", src_derposts_fn, "\n` does not exist and therefore ",
         "cannot be copied to directory\n`", pth, "`")
  }
  copy_flag <- file.copy(src_derposts_fn, dest_derposts_fn)
  if(!copy_flag){
    stop("The file:\n`", src_derposts_fn,
         "`\ncould not be copied to directory\n`", pth, "`")
  }

  # PSV file ----
  sspsv_fn <- file.path(model$mcmc_path, psv_fn)
  if(!file.exists(sspsv_fn)){
    stop("The file:\n`", sspsv_fn, "\n` does not exist and therefore ",
         "cannot be copied to directory\n`", pth, "`")
  }
  copy_flag <- file.copy(sspsv_fn, file.path(pth, psv_fn))
  if(!copy_flag){
    stop("The file:\n`", psv_fn, "`\ncould not be copied to directory\n`",
         pth, "`")
  }

  invisible()
}