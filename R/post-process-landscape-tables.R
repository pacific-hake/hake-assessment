#' Post-process landscape tables in TEX code
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_landscape_tables <- function(x,
                                          ...){

  # Add fancy landscape page type to landscape pages to remove the sideways
  # headers and footers, and add a page number at the bottom
  lscape_inds <- grep("^\\\\begin\\{landscape\\}", x)
  if(!length(lscape_inds)){
    return(x)
  }

  lst <- post_process_extract_chunks(x, lscape_inds, lscape_inds)
  # Replace the begin landscape line with the KOMA options lines
  lst$between <- map(lst$between, \(lscape_line){
    # Extract font information
    fnt_line <- gsub("^\\\\begin\\{landscape\\}\\\\begingroup(.*)$",
                     "\\1",
                     lscape_line)
    c("%",
      "% The following code was injected by",
      "% hake::post_process_landscape_tables()",
      "%",
      "\\KOMAoptions{paper = landscape, DIV = last}",
      paste0("\\newgeometry{",
             "hmargin = 1in, ",
             "top = 0.25in, ",
             "bottom = 1in, ",
             "height = 7in, ",
             "includehead}"),
      "\\fancyheadoffset{0pt}",
      fnt_line,
      "%",
      "% End of injected code",
      "%")
  })

  x <- post_process_interlace_chunks(lst)

  # Replace the \end{landscape] with the KOMA options lines
  # portrait mode
  lscape_endgroup_inds <- grep("\\\\endgroup\\{\\}", x)
  lscape_end_inds <- grep("\\\\end\\{landscape\\}", x)
  wch <- which((lscape_endgroup_inds + 1) %in% lscape_end_inds)
  lscape_endgroup_inds <- lscape_endgroup_inds[wch]
  lscape_end_inds <- lscape_endgroup_inds + 1

  lst <- post_process_extract_chunks(x, lscape_endgroup_inds, lscape_end_inds)
  lst$between <- map(lst$between, \(lscape_line){
    c("%",
      "% The following code was injected by",
      "% hake::post_process_landscape_tables()",
      "%",
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
