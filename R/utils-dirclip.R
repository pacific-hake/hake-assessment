#' Copy the current working directory to the system clipboard
#'
#' @details
#' An error will be thrown if writing to the clipboard fails
#'
#' @return `TRUE` or `FALSE`. If `TRUE`, a character string representing the
#' current working directory is now in the system clipboard. If `FALSE`, the
#' writing to clipboard was not possible
#' @export
dirclip <- function(){
  if(clipr_available()){
    write_clip(getwd())
    return(TRUE)
  }else{
    warning("`clipr` failed to write the current directory to the ",
            "clipboard. The output from `clipr::dr_clipr()` is: ",
            dr_clipr())
    return(FALSE)
  }
}