#' Post-process landscape tables in TEX code
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_landscape_tables <- function(x){

  # Add fancy landscape page type to landscape pages to remove the sideways
  # headers and footers, and add a page number at the bottom
  lscape_inds <- grep("^\\\\begin\\{landscape\\}", x)
  if(!length(lscape_inds)){
    return(x)
  }
  lst <- post_process_extract_chunks(x, lscape_inds, lscape_inds)
  # Replace the begin landscape line with the KOMA options lines
  lst$between <- map(lst$between, \(lscape_line){
    c("\\KOMAoptions{paper = landscape, DIV = last}",
      paste0("\\newgeometry{",
             "hmargin = 1in, ",
             "bottom = 1in, ",
             "height = 7in, ",
             "includehead}"),
      "\\fancyheadoffset{0pt}")
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
    c("\\KOMAoptions{paper = portrait, DIV = last}",
      "\\restoregeometry",
      "\\fancyheadoffset{0pt}")
  })

  x <- post_process_interlace_chunks(lst)

  # If there are now any instances where there has been a `\KOMAoptions`
  # command to switch to portrait followed immediately by a `\KOMAoptions`
  # command to switch to landscape, remove them because it just means there
  # are two landscape tables in a row. If this isn't done, there will be
  # a newline between all landscape tables that follow one another
  lscape_koma_inds <- grep(
    "^\\\\KOMAoptions\\{paper = portrait, DIV \\= last\\}", x)
  lscape_fancy_inds <- grep(paste0("^\\\\fancyheadoffset\\{0pt\\}"), x)
  wch <- which((lscape_koma_inds + 6) %in% lscape_fancy_inds)
  lscape_koma_inds <- lscape_koma_inds[wch]
  lscape_fancy_inds <- lscape_koma_inds + 6

  lst <- post_process_extract_chunks(x, lscape_koma_inds, lscape_fancy_inds)
  lst$between <- map(lst$between, \(lscape_line){
    ""
  })

  x <- post_process_interlace_chunks(lst)

  # Rotate section page if the first table is a landscape table
  lscape_koma_inds <- grep(
    "^\\\\KOMAoptions\\{paper = landscape, DIV \\= last\\}", x)
  lscape_section_inds <- grep(paste0("^\\\\section\\{"), x)
  wch <- which((lscape_koma_inds - 3) %in% lscape_section_inds)
  lscape_koma_inds <- lscape_koma_inds[wch]
  lscape_section_inds <- lscape_koma_inds - 3
  if(length(lscape_section_inds)){
    # There is at least one section page that needs rotating because a
    # landscape table is the first table of the section
    # Search 20 lines above to find the closest \\newpage and put
    # landscape code above it
    newpage_inds <- map_dbl(lscape_section_inds, ~{
      .x + grep("^\\\\newpage", x[(.x - 20):(.x)]) - 20 - 1
    })
    lst <- post_process_extract_chunks(x, newpage_inds, newpage_inds)
    lst$between <- map(lst$between, \(lscape_line){
      c("\\KOMAoptions{paper = landscape, DIV = last}",
        paste0("\\newgeometry{",
               "hmargin = 1in, ",
               "bottom = 1in, ",
               "height = 7in, ",
               "includehead}"),
        "\\fancyheadoffset{0pt}",
        "\\newpage")
    })
    x <- post_process_interlace_chunks(lst)

    # Remove the `\KOMAoptions` lines that add landscape from above the table
    # since it was just placed above the `\newpage` command
    lst <- post_process_extract_chunks(x,
                                       lscape_koma_inds + 3,
                                       lscape_koma_inds + 5)
    lst$between <- map(lst$between, \(lscape_line){
      ""
    })
    x <- post_process_interlace_chunks(lst)
  }

  x
}
