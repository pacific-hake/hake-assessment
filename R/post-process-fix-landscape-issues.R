#' Remove landscape/portrait code between tables/figures which are sequential.
#' If this is not done, there will be a blank portrait page between
#' sequential landscape pages
#'
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_fix_landscape_issues <- function(x){

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
           "post_process_landscape_figures()",
           call. = FALSE)
    }
    lst <- post_process_extract_chunks(x, la, po + 2)
    # Each `between` chunk of `lst` is a full landscape table or figure
    # with the KOMA header set to landscape and the KOMA footer chunk set
    # to portrait.
    # If the `lst$inbetween` for this list are all `""` or `clearpage`,
    # then the portrait setting on the end of the previous `lst$between` list
    # can be removed and the list interlaced again

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

  x
}
