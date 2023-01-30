#' Build the assessment document entirely from within an R session
#'
#' @details Make sure you have created the .rds files by running [build_rds()] in the appropriate manner.
#' Once you have done that and run this function once within an R session, it will be a little quicker since the RDS
#' file contents will have already been loaded into the R session.
#'
#' @param doc_name What to name the document (no extension)
#' @param doc_dir Directory the main document RNW file resides in
#' @param alt_text The knitting process creates the global `alt_fig_text`, but only if the figures were not cached.
#' This must only be set to `TRUE` when building from scratch, without any cached figures
#' @param knit_only Only knit the code, do not run latex engine.
#' @param make_pdf Logical. `TRUE` to make the pdf, if `FALSE` it will only go as far as postscript. If `png_figs`
#' is set to `TRUE`, this argument will have no effect, a PDF will be built anyway
#' @param make_bib Logical. Run bibtex
#' @param ... Arguments to pass to [add_alt_text()]
#'
#' @return [base::invisible()]
#' @export
build_doc <- function(doc_name = "hake-assessment",
                      doc_dir = here::here("doc"),
                      alt_text = FALSE,
                      knit_only = FALSE,
                      make_pdf = TRUE,
                      make_bib = TRUE,
                      ...){

  curr_dir <- getwd()
  on.exit(setwd(curr_dir))
  setwd(doc_dir)

  latex_command <- "lualatex"

  knit(paste0(doc_name, ".rnw"))
  # The knitting process creates the global `alt_fig_text`, but only if the figures were not cached
  if(alt_text){
    add_alt_text(paste0(doc_name, ".tex"), alt_fig_text, ...)
  }

  if(!knit_only){
    system(paste0(latex_command, " ", doc_name, ".tex"), wait = TRUE)
    system(paste0(latex_command, " ", doc_name, ".tex"), wait = TRUE)
    if(make_bib){
      system(paste0("bibtex ", doc_name), wait = TRUE)
    }
    system(paste0(latex_command, " ", doc_name, ".tex"), wait = TRUE)
    system(paste0(latex_command, " ", doc_name, ".tex"), wait = TRUE)
  }
  invisible()
}

#' Build a pared-down version of the assessment. Typically used to compile figures section or
#' tables section only. Citations and other references will not be compiled properly since
#' the latex engine, e.g., `lualatex`, is only called once
#'
#' @param doc_name What to name the document (no extension needed)
#'
#' @return [base::invisible()]
#' @export
build_test <- function(doc_name = "hake-assessment-test"){

  curr_dir <- getwd()
  on.exit(setwd(curr_dir))
  setwd(here::here("doc"))
  knit(paste0(doc_name, ".rnw"))
  # The knitting process creates the global `alt_fig_text`
  #add_alt_text(paste0(doc_name, ".tex"), alt_fig_text)
  system(paste0("lualatex ", doc_name, ".tex"), wait = TRUE)
  invisible()
}

#' Build the executive summary section only
#'
#' @param doc_name What to name the document (no extension needed)
#'
#' @return [base::invisible()]
#' @export
build_es <- function(){
  build_test(doc_name = "hake-assessment-es-only")
}

#' Build the ADNUT diagnostics document entirely from within an R session
#'
#' @details Make sure you have created the .rds files by running [build_rds()] in the appropriate manner.
#' Once you have done that and run this function once within an R session, it will be a little quicker since the RDS
#' file contents will have already been loaded into the R session. Only has the
#' PNG option since EPS files produce a huge final PDF; no need for bibtex
#'   since no references. If those are needed then just adapt `build_doc` again.
#'
#' @param knit_only Only knit the code, do not run latex engine
#' @param doc_name What to name the document (no extension)
#'
#' @return [base::invisible()]
#' @export
build_adnuts_doc <- function(knit_only = FALSE,
                             doc_name = "adnuts-diagnostics",
                             ...){

  latex_command <- "lualatex"
  curr_path <- getwd()
  setwd(here::here("doc-adnuts-diagnostics"))

  knit(paste0(doc_name, ".rnw"))

  if(!knit_only){
    system(paste0(latex_command, " ", doc_name, ".tex"), wait = TRUE)
    system(paste0(latex_command, " ", doc_name, ".tex"), wait = TRUE)
    # system(paste0(latex_command, " ", doc_name, ".tex"), wait = TRUE)  # two should be enough
  }
  setwd(curr_path)
  invisible()
}
