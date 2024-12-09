#' Load an object from an `.Rdat` file
#'
#' Create a file path from `dir` and `file` using [file.path()] and
#' load the object into the local environment while returning it.
#' `get_local()` assumes that there is only one object in the `.Rdat` file.
#' The utility of this function comes from being able to quickly load
#' an R object that was saved as an `.Rdat` into a function argument without
#' having to save it in your work space.
#' @param file A file path for either an existing file or
#' a file that will be written to the disk by the given
#' function. Paths can be either full or relative.
#' @param dir The path to a directory.
#' @author Kelli F. Johnson
#' @export
get_local <- function(file,
                      dir = file.path(hakedata_wd(), "extractedData")) {
  env <- new.env()
  nm <- load(file.path(dir, file), env)
  env[[nm]]
}
