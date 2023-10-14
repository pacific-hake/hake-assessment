#' Clean the doc directory of all build output including knitr cache and
#' knitr figures directories
#'
#' @details
#' This function can be called from anywhere in the hake project and will
#' clean the "doc" directory, not the local directory. If you are testing
#' with `gotest()` and are in a temporary directory, the cleaning will be
#' applied there instead. The cleaning takes place on whatever directory
#' is at `here::here("doc")`
#'
#' @param knitr_figures_dir Directory where4 the knitr-generated
#' figures reside
#' @param knitr_cache_dir Directory where the knitr cached chunk
#' databases reside
#' @param out_csv_dir Directory where the table outputs reside
#'
#' @return Nothing
#' @export
clean <- function(knitr_figures_dir = here::here("doc/knitr-figs"),
                  knitr_cache_dir = here::here("doc/knitr-cache"),
                  out_csv_dir = here::here("doc/out-csv")){

  # Delete hake.aux, hake.tex etc or
  # hake-assessment.aux, hake-assessment.tex etc
  fns <- list.files(path = here::here("doc"),
                    pattern = paste0("hake(\\-assessment)?\\.(",
                                     "tex|",
                                     "Rmd|",
                                     "aux|",
                                     "bbl|",
                                     "blg|",
                                     "log|",
                                     "lof|",
                                     "lot|",
                                     "pdf|",
                                     "ps|",
                                     "md|",
                                     "toc|",
                                     "txt|",
                                     "upa|",
                                     "upb)"),
                    full.names = TRUE)

  unlink(fns, force = TRUE)

  # Delete build directories
  dirs <- c(knitr_figures_dir,
            knitr_cache_dir,
            out_csv_dir)

  unlink(dirs, recursive = TRUE, force = TRUE)
  message("Done cleaning the `", here::here("doc"), "` directory\n")
}