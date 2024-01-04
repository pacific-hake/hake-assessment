#' Call [shell()] or [system()] depending on the Operating System
#'
#' @param ... Arguments to pass to either [shell()] or [system()]
#'
#' @return The output from the command function called
#' @export
system_ <- function(...){

  if(get_os() == "windows"){
    shell(...)
  }else{
    system(...)
  }
}
