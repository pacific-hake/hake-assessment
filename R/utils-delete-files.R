#' Delete all model rds files found in the subdirectories of the `models_dir`` directory
#'
#' @param models_dir Directory in which the models reside
#' @param dont_del A vector of directory names to keep
#'
#' @return [base::invisible()]
#' @export
delete_rds_files <- function(models_dir,
                             dont_del = last_yr_base_model_dir_name){

  dirs <- dir(models_dir)
  dirs <- dirs[! dirs %in% dont_del]
  rds_files <- file.path(models_dir, dirs, paste0(dirs, ".rds"))
  unlink(rds_files, force = TRUE)
  message("Deleted ", paste0(rds_files, collapse = "\n"),
          "\nAll rds files deleted except for ", dont_del, "\n")
  invisible()
}

#' Delete all directories and files of `sub_dir``
#'
#' @param models_dir Directory name for all models location
#' @param sub_dir The subdirectory to delete recursively
#'
#' @return [base::invisible()]
#' @export
delete_dirs <- function(models_dir,
                        sub_dir = NULL){

  dirs <- dir(models_dir)
  files <- file.path(models_dir, dirs, sub_dir)
  unlink(files, recursive = TRUE, force = TRUE)
  message("All files and directories were deleted from the",
          sub_dir, "directory in each model directory.\n")
  invisible()
}
