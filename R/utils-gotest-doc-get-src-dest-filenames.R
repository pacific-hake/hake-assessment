#' Create the lists of source and destination filenames needed for copying
#' for the main assessment document testing
#'
#' @details
#' Meant to be called by the wrapper function [gotest()]
#'
#' @return Nothing
gotest_doc_get_src_dest_filenames <- function(bookdown_lst = NULL,
                                              figures_dir = NULL){

  figures_dir <- figures_dir %||% "image-files"

  raw_fns <- c(
    "000-launcher.rmd",
    "001-load-packages.rmd",
    "002-load-globals.rmd",
    "003-load-models.rmd",
    "003-load-models.R",
    "004-load-project-variables.rmd",
    "999-blank.rmd",
    caption_adjustments_fn,
    forecast_descriptions_fn,
    object_placement_fn,
    "preamble.tex",
    "bib/refs.bib",
    "csl/csas.csl")

  fns_exists <- file.exists(raw_fns)
  if(!all(fns_exists)){
    stop("One or more files that are required to be copied for testing ",
         "do not exist in the current directory. The file(s) that do ",
         "not exists are:\n\n",
         paste(raw_fns[!fns_exists], collapse = "\n"),
         "\n\nCheck the `gotest_doc()` function")
  }

  # Add the main figures (prebuilt figures and logos in files)
  main_figs_src_dir <- here::here("doc", figures_dir)
  main_figs_basename_fns <- list.files(main_figs_src_dir)
  main_figs_fns <- file.path(figures_dir, main_figs_basename_fns)
  raw_fns <- c(raw_fns, main_figs_fns)

  doc_dr <- file.path(here::here("doc"))
  src_fns <- file.path(doc_dr, raw_fns)
  dest_fns <- file.path("doc", raw_fns)

  list(src_fns = src_fns,
       dest_fns = dest_fns)
}