#' Run [clean()] on directories used for rendering the document and
#' beamer presentations
#'
#' @details
#' Typically this needs to be done before running the [devtools::check()]
#' function to avoid those build files being analyzed
#'
#' @return Nothing
#' @export
clean_all <- function(){

  curr_dir <- getwd()
  on.exit(setwd(curr_dir))

  drs_beamer <- list.dirs(here("beamer"))
  drs_beamer <- drs_beamer[-grep("knitr", drs_beamer)]

  drs <- c(here(doc_path), drs_beamer)

  walk(drs, \(dr){
    setwd(dr)
    clean()
  })
}