#' Clean (delete) all build output including knitr cache and knitr figures
#' directories from the current directory
#'
#' @details
#' This function will clean the current working directory. It works from
#' within the main `doc` directory as well as in all the beamer presentation
#' directories. If you are testing with `gotest()` and are in a temporary
#' directory, in the subdirectory `doc`, the cleaning will be applied there.
#'
#' @param ... Optional quoted or unquoted names of chunks that should be
#' 'cleaned'.The chunk name(s) will be used to delete only the objects
#' relating to them from the `knitr_figures_dir` and `knitr-cache`.as well
#' as all the LaTeX and intermediate RMD and MD files
#' @param knitr_figures_dir Directory where the knitr-generated
#' figures reside
#' @param knitr_cache_dir Directory where the knitr cached chunk
#' databases reside
#'
#' @return Nothing
#' @export
clean <- function(...,
                  knitr_figures_dir = "knitr-figs",
                  knitr_cache_dir = "knitr-cache"){

  chunks <- enquos(...)
  if(length(chunks)){
    # Only delete files related to the chunks given
    figs <- list.files(knitr_figures_dir, full.names = TRUE)
    cache <- list.files(knitr_cache_dir, full.names = TRUE)
    walk(chunks, ~{
      nm <- gsub(" - ", "-", as_label(.x))
      nm <- gsub('"', "", nm)
      fns <- grep(nm, figs, value = TRUE)
      if(length(fns)){
        result <- unlink(fns, force = TRUE)
        if(!result){
          message("Deleted file(s): ", paste(fns, collapse = ", "))
        }
      }
      fns <- grep(nm, cache, value = TRUE)
      if(length(fns)){
        result <- unlink(fns, force = TRUE)
        if(!result){
          message("Deleted file(s): ", paste(fns, collapse = ", "))
        }
      }
    })
  }else{
    # Delete knitr directories recursively (all files)
    dirs <- c(knitr_figures_dir,
              knitr_cache_dir,
              out_csv_path)
    unlink(dirs, recursive = TRUE, force = TRUE)
  }

  curr_dir <- getwd()
  knitr_figures_dir <- file.path(curr_dir, knitr_figures_dir)
  knitr_cache_dir <- file.path(curr_dir, knitr_cache_dir)
  out_csv_dir <- file.path(curr_dir, out_csv_path)

  # Possible names of the docs to delete without extensions
  docs_pat <- c("hake",
                "hake-assessment",
                "JTC-assessment",
                "JTC-data",
                "JTC-management",
                "JTC-requests",
                "JTC-responses",
                "JTC-sensitivity",
                "JTC-stock-assessment-JMC",
                "JTC-stock-assessment-JMC-Canada",
                "JTC-stock-assessment-JMC-US",
                "JTC-data-december",
                "test",
                "fmin")

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
                           "upb",
                           ")")
  for(fn_base in docs_pat){
    fns <- list.files(path = curr_dir,
                      pattern = paste0(fn_base,
                                       "\\.",
                                       extensions_pat),
                      full.names = TRUE)

    # Delete files that match above combinations
    unlink(fns, force = TRUE)
  }

  message("Done cleaning the `", curr_dir, "` directory\n")
}