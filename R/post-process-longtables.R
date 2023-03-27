#' Post-process LaTeX longtables in TEX code
#'
#' @details
#' Injects some LaTeX code which adds 'Continued on next page ...' and
#' '... Continued from previous page' to the longtables that split pages.
#' Removes the caption for the subsequent pages, but leaves the header row
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_longtables <- function(x){

  beg_inds <- grep("\\\\begin\\{longtable\\}", x)
  end_inds <- grep("\\\\end\\{longtable\\}", x)
  if(length(beg_inds) != length(end_inds)){
    len_beg <- ifelse(
      length(beg_inds) == 1,
      "There was 1 `\\begin{longtable}` command ",
      paste0("There ", length(beg_inds), "  `\\begin{longtable}` commands "))
    len_end <- ifelse(
      length(end_inds) == 1,
      "and 1 `\\begin{longtable}` command. ",
      paste0("and ", length(end_inds), " `\\end{longtable}` commands. "))
    stop(len_beg, len_end, "There must be the same number of each",
         call. = FALSE)
  }
  if(!length(beg_inds)){
    message("There were no `\\begin{longtable}` macros found in the TeX ",
            "code.\n")
    return(x)
  }

  lst <- post_process_extract_chunks(x, beg_inds, end_inds)
  n_col <- map(lst$between, function(tbl){
    # Get the first line starting with a year (first row in table)
    first_yr_line <- tbl[grep("^\\\\endlastfoot$", tbl) + 1]
    # Extract the number of columns in the table
    str_count(first_yr_line, "&") + 1
  })
  # Get location of `\\endfirsthead` and paste "Continued from" line
  lst$between <- map2(lst$between, n_col, function(tbl, nc){
    efh <- grep("endfirsthead", tbl)
    pre <-tbl[1:(efh)]
    post <- tbl[(efh + 1):length(tbl)]
    c(pre,
      paste0(
        "\\multicolumn{",
        nc,
        "}{l}{\\textit{... Continued from previous page}} \\\\ \\hline"),
      post)
  })
  # Get location of `\\endhead` and paste "... Continued on" line
  lst$between <- map2(lst$between, n_col, function(tbl, nc){
    efh <- grep("endhead", tbl)
    pre <-tbl[1:(efh)]
    post <- tbl[(efh + 1):length(tbl)]
    c(pre,
      paste0(
        "\\hline \\multicolumn{",
        nc,
        "}{l}{\\textit{Continued on next page ...}} \\\\"),
      post)
  })
  # Remove caption and toprule from second page
  lst$between <- map(lst$between, function(tbl){
    cap <- grep("caption\\[\\]", tbl)
    pre <- tbl[1:(cap - 1)]
    # Assumes `\\toprule` follows `\\caption[]` directly (+ 2)
    post <- tbl[(cap + 2):length(tbl)]
    c(pre, post)
  })

  post_process_interlace_chunks(lst)
}
