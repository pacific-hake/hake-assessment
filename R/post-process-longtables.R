#' Post-process LaTeX longtables in TEX code
#'
#' @details
#' Injects some LaTeX code which adds 'Continued on next page ...' and
#' '... Continued from previous page' to the longtables that split pages.
#' Removes the caption for the subsequent pages, but leaves the header row.
#' Assumes that the table TeX was created using the following syntax:
#' `kable(longtable = TRUE) |> kable_styling(latex_options = c("repeat_header"))`
#' If `repeat_header` is not used to create the TeX code an error will be
#' thrown which will explain that `\\endfirsthead` cannot be found.
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#' @param cont_page_just The justification for the continue page lines.
#' Must be one of "l", "c", or "r"
#' @param ... Absorbs arguments meant for other functions
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_longtables <- function(x,
                                    cont_page_just = c("l", "c", "r"),
                                    ...){

  cont_page_just <- match.arg(cont_page_just)

  beg_inds <- grep("\\\\begin\\{longtable\\}", x)
  end_inds <- grep("\\\\end\\{longtable\\}", x)
  if(length(beg_inds) != length(end_inds)){
    len_beg <- ifelse(
      length(beg_inds) == 1,
      "There is 1 `\\begin{longtable}` command ",
      paste0("There are ",
             length(beg_inds),
             "  `\\begin{longtable}` commands "))
    len_end <- ifelse(
      length(end_inds) == 1,
      "and 1 `\\begin{longtable}` command. ",
      paste0("and ",
             length(end_inds),
             " `\\end{longtable}` commands. "))
    stop(len_beg, len_end, "There must be the same number of each")
  }
  if(!length(beg_inds)){
    return(x)
  }

  lst <- post_process_extract_chunks(x, beg_inds, end_inds)

  n_col <- map_dbl(lst$between, \(tbl){
    # Get the first line starting with a year (first row in table)
    gr <- grep("^\\\\endlastfoot$", tbl)
    if(!length(gr)){
      stop("There was a longtable defined but no \\endlastfoot associated ",
           "with it.\nThis was likely caused by missing the `repeat_header` ",
           "part in the call:\n\n",
           "`kable(longtable = TRUE) |> kable_styling(latex_options = ",
           "c('repeat_header'))`\nIn addition, booktabs must be set to ",
           "`TRUE`.\n\nThe chunk with the error is:\n",
           paste(tbl, collapse = "\n"), "\n")
    }
    first_yr_line <- tbl[gr + 1]
    # Extract the number of columns in the table
    str_count(first_yr_line, "&") + 1
  })

  # Get location of `\\endfirsthead` and paste "Continued from" line
  lst$between <- imap(lst$between, function(tbl, i){
    efh <- grep("endfirsthead", tbl)
    pre <-tbl[1:(efh)]
    post <- tbl[(efh + 1):length(tbl)]
    tmp <-
      c(pre,
        paste0(
          "\\multicolumn{",
          n_col[i],
          "}{",
          cont_page_just,
          "}{\\textit{... Continued from previous page}} \\\\ \\midrule"),
        post)

    efh <- grep("endhead", tmp)
    pre <-tmp[1:(efh)]
    post <- tmp[(efh + 1):length(tmp)]
    tmp <- c(pre,
             paste0(
               "\\hline \\multicolumn{",
               n_col[i],
               "}{",
               cont_page_just,
               "}{\\textit{Continued on next page ...}} \\\\"),
             post)

    cap <- grep("caption\\[\\]", tmp)
    pre <- tmp[1:(cap - 1)]
    # Assumes `\\toprule` follows `\\caption[]` directly (+ 2)
    post <- tmp[(cap + 2):length(tmp)]
    tmp <- c(pre, post)

    tmp
  })

  post_process_interlace_chunks(lst)
}
