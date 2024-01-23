#' Post-process landscape figures in TEX code
#'
#' @details
#' Searches the TeX code in `x` for the keywords `BEGIN LANDSCAPE` and
#' `END LANDSCAPE` (all in caps) that surround either a knitr chunk that
#' produces a figure or a Bookdown image import statement with this format:
#' `![Caption](path/to/filename)`
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_landscape_figures <- function(x,
                                           ...){

  # Add fancy landscape page type to landscape pages to remove the sideways
  # headers and footers, and add a page number at the bottom
  lscape_inds <- grep("^BEGIN LANDSCAPE", x)
  if(!length(lscape_inds)){
    return(x)
  }
  lst <- post_process_extract_chunks(x, lscape_inds, lscape_inds)
  # Replace the begin landscape line with the KOMA options lines
  lst$between <- map(lst$between, \(lscape_line){
    c("%",
      "% The following code was injected by",
      "% hake::post_process_landscape_figures()",
      "%",
      "\\KOMAoptions{paper = landscape, DIV = last}",
      paste0("\\newgeometry{",
             "hmargin = 1in, ",
             "top = 0.25in, ",
             "bottom = 1in, ",
             "height = 7in, ",
             "includehead}"),
      "\\fancyheadoffset{0pt}",
      "%",
      "% End of injected code",
      "%")
  })

  x <- post_process_interlace_chunks(lst)

  # Replace the end landscape line with the KOMA options lines
  # portrait mode
  lscape_end_inds <- grep("^END LANDSCAPE", x)

  lst <- post_process_extract_chunks(x, lscape_end_inds, lscape_end_inds)
  lst$between <- map(lst$between, \(lscape_line){
    c("%",
      "% The following code was injected by",
      "% hake::post_process_landscape_figures()",
      "%",
      "\\clearpage",
      "\\KOMAoptions{paper = portrait, DIV = last}",
      "\\restoregeometry",
      "\\fancyheadoffset{0pt}",
      "%",
      "% End of injected code",
      "%")
  })

  x <- post_process_interlace_chunks(lst)

  x
}
