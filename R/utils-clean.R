#' Clean (delete) all build output including knitr cache and knitr figures
#' directories from the current directory
#'
#' @details
#' This function can be called from anywhere in the hake project and will
#' clean the "doc" directory, not the local directory. If you are testing
#' with `gotest()` and are in a temporary directory, the cleaning will be
#' applied there instead. The cleaning takes place on whatever directory
#' is at `here::here("doc")`
#'
#' @param knitr_figures_dir Directory where the knitr-generated
#' figures reside
#' @param knitr_cache_dir Directory where the knitr cached chunk
#' databases reside
#' @param out_csv_dir Directory where the table outputs reside
#'
#' @return Nothing
#' @export
clean <- function(knitr_figures_dir = "knitr-figs",
                  knitr_cache_dir = "knitr-cache",
                  out_csv_dir = "out-csv"){

  curr_dir <- getwd()
  knitr_figures_dir <- file.path(curr_dir, "knitr-figs")
  knitr_cache_dir <- file.path(curr_dir, "knitr-cache")
  out_csv_dir <- file.path(curr_dir, "out-csv")

  # Possible names of the docs to delete without extensions
  docs_pat <- c("hake",
                "hake-assessment",
                "JTC-assessment",
                "JTC-data",
                "JTC-management",
                "JTC-requests",
                "JTC-sensitivity",
                "JMC",
                "JMC-Canada",
                "JMC-US")

  # The extensions of the above docs to delete
  extensions_pat <- paste0("(",
                           "aux|",
                           "bbl|",
                           "blg|",
                           "log|",
                           "lof|",
                           "lot|",
                           "md|",
                           "nav|",
                           "pdf|",
                           "ps|",
                           "Rmd|",
                           "snm|",
                           "tex|",
                           "toc|",
                           "txt|",
                           "upa|",
                           "upb)")
  for(i in seq_along(docs_pat)){
    full_pat <- paste0(docs_pat[i],
                       "\\.(",
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
                       "upb)")

    # Delete hake.aux, hake.tex etc or
    # hake-assessment.aux, hake-assessment.tex etc
    fns <- list.files(path = curr_dir,
                      pattern = full_pat,
                      full.names = TRUE)
    unlink(fns, force = TRUE)
  }

  unlink(c("hake.Rmd",
           "hake-assessment.Rmd"),
         force = TRUE)

  # Delete build directories
  dirs <- c(knitr_figures_dir,
            knitr_cache_dir,
            out_csv_dir)

  unlink(dirs, recursive = TRUE, force = TRUE)
  message("Done cleaning the `", curr_dir, "` directory\n")
}