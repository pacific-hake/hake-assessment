#' Stop the program without issuing an error message
#'
#' @param ... Arguments passed in to be written out as a message
#'
#' @return Nothing
#' @export
stop_quietly <- function(...) {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  message(unlist(list(...)))
  stop()
}
