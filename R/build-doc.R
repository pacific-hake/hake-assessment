#' Build the assessment document entirely from within an R session
#'
#' @details Make sure you have created the .rds files by running [build_rds()] in the appropriate manner.
#' Once you have done that and run this function once within an R session, it will be a little quicker since the RDS
#' file contents will have already been loaded into the R session.
#'
#' @param knit_only Only knit the code, do not run latex or pdflatex
#' @param make_pdf Logical. `TRUE` to make the pdf, if `FALSE` it will only go as far as postscript. If `png_figs`
#' is set to `TRUE`, this argument will have no effect, a PDF will be built anyway
#' @param make_bib Logical. Run bibtex
#' @param png_figs Logical. If `TRUE`, use the pdflatex command to build (e.g. if PNGs are being used for figures).
#' If `FALSE`, use latex, dvips, and ps2pdf to build (e.g. if EPS figures are being used)
#' @param doc_name What to name the document (no extension)
#'
#' @return [base::invisible()]
#' @export
build_doc <- function(knit_only = FALSE,
                      make_pdf = TRUE,
                      make_bib = TRUE,
                      png_figs = TRUE,
                      doc_name = "hake-assessment",
                      ...){

  mod_code_for_build(png_figs = png_figs, ...)

  latex_command <- ifelse(png_figs, "pdflatex", "latex")
  curr_path <- getwd()
  setwd(here::here("doc"))
  if(png_figs){
    knit_alttext(doc_name)
  }else{
    knit(paste0(doc_name, ".rnw"))
  }
  if(!knit_only){
    shell(paste0(latex_command, " ", doc_name, ".tex"))
    if(png_figs){
      shell(paste0(latex_command, " ", doc_name, ".tex"))
    }
    if(make_bib){
      shell(paste0("bibtex ", doc_name))
    }
    shell(paste0(latex_command, " ", doc_name, ".tex"))
    shell(paste0(latex_command, " ", doc_name, ".tex"))
    if(!png_figs){
      shell(paste0(latex_command, " ", doc_name, ".tex"))
      shell(paste0(latex_command, " ", doc_name, ".tex"))
      shell(paste0(latex_command, " ", doc_name, ".tex"))
      shell(paste0("dvips ", doc_name, ".dvi"))
      shell(paste0("ps2pdf ", doc_name, ".ps"))
    }
  }
  setwd(curr_path)
  invisible()
}

#' Build a pared-down version of the assessment. Typically used to compile figures section or
#' tables section only. Citations and other references will not be compiled properly since
#' pdflatex is only called once
#'
#' @param doc_name What to name the document (no extension needed)
#'
#' @return [base::invisible()]
#' @export
build_test <- function(doc_name = "hake-assessment-test"){

  curr_path <- getwd()
  setwd(here::here("doc"))
  knit_alttext(doc_name)
  shell(paste0("pdflatex ", doc_name, ".tex"))
  setwd(curr_path)
  invisible()
}

#' Modify code in the RNW file and the STY file to ensure the build works for either PNG figures or EPS figures
#'
#' @param png_figs Logical. If `TRUE`, write the RNW and sty file settings to reflect PNG figure inclusion in the document,
#' If `FALSE`, write the RNW and STY file settings to reflect EPS/PDF figure inclusion in the document
#' @param rnw_file File name for the assessment code RNW file
#' @param sty_file File name for the style code STY file
#' @param ... Absorb other arguments not intended for this function
#'
#' @return Nothing
#' @export
mod_code_for_build <- function(png_figs = TRUE,
                               rnw_file = "hake-assessment.rnw",
                               sty_file = "hake.sty",
                               ...){

  if(!file.exists(rnw_file)){
    stop("The file ", rnw_file, " does not exist.",
         call. = FALSE)
  }
  if(!file.exists(sty_file)){
    stop("The file ", sty_file, " does not exist.",
         call. = FALSE)
  }
  # Re-write order of figures in hake style file
  hake_sty <- readLines(sty_file)
  j <- grep("DeclareGraphicsExtensions", hake_sty)
  if(!length(j)){
    stop("Could not find any entry for 'DeclareGraphicsExtensions' in hake.sty",
         call. = FALSE)
  }
  if(length(j) > 1){
    stop("There were multiple definitions of 'DeclareGraphicsExtensions' in hake.sty",
         call. = FALSE)
  }
  k <- grep(".jpeg,.JPEG\\}", hake_sty)
  if(!length(k)){
    stop("Could not find the ending entry for 'DeclareGraphicsExtensions' in hake.sty. ",
         "There must be a line after 'DeclareGraphicsExtensions' which contains '.jpeg,.JPEG}'",
         call. = FALSE)
  }
  if(length(j) > 1){
    stop("There were multiple lines containing '.jpeg,.JPEG}' in hake.sty",
         call. = FALSE)
  }
  if(j >= k){
    stop("The line containing 'DeclareGraphicsExtensions' in hake.sty occurred after the ",
         "line containing '.jpeg,.JPEG}'",
         call. = FALSE)
  }

  if(k - j != 5){
    stop("The 'DeclareGraphicsExtensions' declaration must span exactly 6 lines in hake.sty. ",
         "It currently spans ", k - j + 1, " lines.",
         call. = FALSE)
  }
  if(png_figs){
    hake_sty[j + 1] <- ".png,.PNG,"
    hake_sty[j + 2] <- ".pdf,.PDF,"
    hake_sty[j + 3] <- ".eps,.EPS,"
    hake_sty[j + 4] <- ".jpg,.JPG,"
    hake_sty[j + 5] <- ".jpeg,.JPEG}"
  }else{
    hake_sty[j + 1] <- ".pdf,.PDF,"
    hake_sty[j + 2] <- ".eps,.EPS,"
    hake_sty[j + 3] <- ".png,.PNG,"
    hake_sty[j + 4] <- ".jpg,.JPG,"
    hake_sty[j + 5] <- ".jpeg,.JPEG}"
  }
  writeLines(hake_sty, sty_file)

  # Set dev in knitr options in assessment file for PNG or EPS figures
  hake_assess <- readLines(rnw_file)
  j <- grep("dev *=", hake_assess)
  if(!length(j)){
    stop("The line 'dev = ' was not found in the ", rnw_file, " file",
         call. = FALSE)
  }
  if(length(j) > 1){
    stop("The line 'dev = ' occurs more than once in the ", rnw_file, " file",
         call. = FALSE)
  }
  hake_assess[j] <- ifelse(png_figs, "dev = 'png',", "dev = 'cairo_ps',")
  # Set fig.path and cache.path in assessment file for PNG or EPS figures
  j <- grep("fig.path *=", hake_assess)
  if(!length(j)){
    stop("The line 'fig.path = ' was not found in the ", rnw_file, " file",
         call. = FALSE)
  }
  if(length(j) > 1){
    stop("The line 'fig.path = ' occurs more than once in the ", rnw_file, " file",
         call. = FALSE)
  }
  hake_assess[j] <- paste0("fig.path = 'knitr-cache-", ifelse(png_figs, "png", "eps"), "/',")
  k <- grep("cache.path *=", hake_assess)
  if(!length(k)){
    stop("The line 'cache.path = ' was not found in the ", rnw_file, " file",
         call. = FALSE)
  }
  if(length(k) > 1){
    stop("The line 'cache.path = ' occurs more than once in the ", rnw_file, " file",
         call. = FALSE)
  }
  hake_assess[k] <- paste0("cache.path = 'knitr-cache-", ifelse(png_figs, "png", "eps"), "/',")
  writeLines(hake_assess, rnw_file)
}

