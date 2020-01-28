#' Delete all rdata files found in the subdirectories of the *models.dir* directory
#'
#' @param models.dir Directory in which the models reside
#' @param dont.del A vector of directory names to keep
#'
#' @return [base::invisible()]
#' @export
delete.rdata.files <- function(models.dir = rootd.models,
                               dont.del = last.yr.base.model.dir.name){
  
  dirs <- dir(models.dir)
  dirs <- dirs[! dirs %in% dont.del]
  rdata.files <- file.path(models.dir, dirs, paste0(dirs, ".rdata"))
  unlink(rdata.files, force = TRUE)
  message("Deleted ", paste0(rdata.files, collapse = "\n"),
          "All rdata files deleted except for ", dont.del, "\n")
  invisible()
}

#' Delete all directories and files of sub.dir
#'
#' @param models.dir Directory name for all models location
#' @param sub.dir The subdirectory to delete recursively
#'
#' @return [base::invisible()]
#' @export
delete.dirs <- function(models.dir = model.dir,
                        sub.dir = NULL){
  
  dirs <- dir(models.dir)
  files <- file.path(models.dir, dirs, sub.dir)
  unlink(files, recursive = TRUE, force = TRUE)
  message("All files and directories were deleted from the",
          sub.dir, "directory in each model directory.\n")
  invisible()
}
