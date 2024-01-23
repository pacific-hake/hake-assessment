#' Remove landscape/portrait code between tables/figures which are sequential.
#' If this is not done, there will be a blank portrait page between
#' sequential landscape pages
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_fix_landscape_issues <- function(x,
                                              ...){

  la <- grep(
    "^\\\\KOMAoptions\\{paper = landscape, DIV \\= last\\}$", x)
  po <- grep(
    "^\\\\KOMAoptions\\{paper = portrait, DIV \\= last\\}$", x)
  if(length(la)){
    if(length(la) != length(po)){
      stop("The number of KOMA landscape chunks does not equal the number ",
           "of KOMA portrait chunks. This means that the landscape page ",
           "algorithm broke and you need to fix it. See ",
           "post_process_landscape_tables() and ",
           "post_process_landscape_figures()")
    }
    lst <- post_process_extract_chunks(x, la, po + 2)
    # Each `between` chunk of `lst` is a full landscape table or figure
    # with the KOMA header set to landscape and the KOMA footer chunk set
    # to portrait.
    # If the `lst$inbetween` for this list are all `""` or `clearpage`,
    # then the portrait setting on the end of the previous `lst$between` list
    # must be removed and the list interlaced again
    inbetween_inds <- seq(ifelse(lst$first, 1, 2), length(lst$inbetween))
    between_inds <- seq_along(lst$between)

    # If the code does not end with a landscape table/figure, ignore the
    # remaining code
    if(length(inbetween_inds) > length(between_inds)){
      inbetween_inds <- inbetween_inds[-length(inbetween_inds)]
    }

    new_between <- list()
    new_inbetween <- list()
    for(i in seq_along(inbetween_inds)){

      inb_ind <- inbetween_inds[i]
      b_ind <- between_inds[i]
      tbls_are_together <- all(grepl("^$|^\\\\clearpage$",
                                     lst$inbetween[[inb_ind]]))

      if(tbls_are_together){
        # The two tables on either side of the current `inbetween` chunk
        # are together
        # Remove all empty lines (strings), and add 1 back
        lst$inbetween[[inb_ind]] <-
          lst$inbetween[[inb_ind]][lst$inbetween[[inb_ind]] != ""]
        lst$inbetween[[inb_ind]] <- c(lst$inbetween[[inb_ind]], "")

        # Find the end KOMA chunk for removal
        end_koma_ind <- grep(
          "^\\\\KOMAoptions\\{paper = portrait, DIV \\= last\\}$",
          lst$between[[b_ind]])
        # Remove the end KOMA chunk
        lst$between[[b_ind]] <- lst$between[[b_ind]][1:(end_koma_ind - 1)]
        # Find the start KOMA chunk in the next table/figure for removal
        beg_koma_ind <- grep(
          "^\\\\KOMAoptions\\{paper = landscape, DIV \\= last\\}$",
          lst$between[[b_ind + 1]])
        # Remove the starting KOMA chunk
        lst$between[[b_ind + 1]] <-
          lst$between[[b_ind + 1]][(beg_koma_ind + 3):length(lst$between[[b_ind + 1]])]

      }
    }
    x <- post_process_interlace_chunks(lst)
  }

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
      gr <- grep("^\\\\newpage", x[(.x - 20):(.x)])
      if(length(gr)){
        .x + gr - 20 - 1
      }else{
        NA_real_
      }
    })
    newpage_inds <- newpage_inds[!is.na(newpage_inds)]
    if(length(newpage_inds)){
      lst <- post_process_extract_chunks(x, newpage_inds, newpage_inds)
      lst$between <- map(lst$between, \(lscape_line){
        c("%",
          "% The following code was injected by",
          "% hake::post_process_fix_landscape_issues()",
          "%",
          "\\KOMAoptions{paper = landscape, DIV = last}",
          paste0("\\newgeometry{",
                 "hmargin = 1in, ",
                 "top = 0.25in, ",
                 "bottom = 1in, ",
                 "height = 7in, ",
                 "includehead}"),
          "\\fancyheadoffset{0pt}",
          "\\newpage",
          "%",
          "% End of injected code",
          "%")
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
  }

  x
}
