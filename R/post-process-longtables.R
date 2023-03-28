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
  # Check to see if landscape. If so, copy label and header and the
  # `\\endhead` lines in (More complex method than portrait mode)
  # `is_landscape_lst` is a vector of indices in the between list which are
  # landscape longtables
  is_landscape_lst <- map_lgl(beg_inds, \(ind){
    as.logical(length(grep("landscape", x[ind - 2])))
  })

  n_col <- map2(lst$between, is_landscape_lst, function(tbl, is_landscape){
    if(is_landscape){
      # Get the first line starting with a year (first row in table)
      first_yr_line <- tbl[grep("^\\\\midrule$", tbl) + 1]
    }else{
      # Get the first line starting with a year (first row in table)
      first_yr_line <- tbl[grep("^\\\\endlastfoot$", tbl) + 1]
      # Extract the number of columns in the table
    }
    str_count(first_yr_line, "&") + 1
  })

  # Get location of `\\endfirsthead` and paste "Continued from" line
  lst$between <- imap(lst$between, function(tbl, i){
    if(is_landscape_lst[i]){
      # Landscape table
      hdr_beg <- grep("\\\\toprule", tbl)
      hdr_end <- grep("\\\\midrule", tbl)
      hdr_names <- tbl[hdr_beg:(hdr_end - 1)]
      pre <- tbl[1:hdr_end]
      post <- tbl[(hdr_end + 1):length(tbl)]
      c(pre,
        "\\endfirsthead",
             paste0(
               "\\multicolumn{",
               n_col[i],
               "}{l}{\\textit{... Continued from previous page}} \\\\"),
        hdr_names,
        "\\midrule",
        "\\endhead",
        paste0("\\hline \\multicolumn{",
               n_col[i],
               "}{l}{\\textit{Continued on next page ...}} \\\\"),
        "\\endfoot",
        "\\bottomrule",
        "\\endlastfoot",
        post)
    }else{
      # Portrait longtable
      efh <- grep("endfirsthead", tbl)
      pre <-tbl[1:(efh)]
      post <- tbl[(efh + 1):length(tbl)]
      tmp <-
        c(pre,
          paste0(
            "\\multicolumn{",
            n_col[i],
            "}{l}{\\textit{... Continued from previous page}} \\\\ \\hline"),
          post)

      efh <- grep("endhead", tmp)
      pre <-tmp[1:(efh)]
      post <- tmp[(efh + 1):length(tmp)]
      tmp <- c(pre,
               paste0(
                 "\\hline \\multicolumn{",
                 n_col[i],
                 "}{l}{\\textit{Continued on next page ...}} \\\\"),
               post)

      cap <- grep("caption\\[\\]", tmp)
      pre <- tmp[1:(cap - 1)]
      # Assumes `\\toprule` follows `\\caption[]` directly (+ 2)
      post <- tmp[(cap + 2):length(tmp)]
      tmp <- c(pre, post)

      tmp
    }
  })

  x <- post_process_interlace_chunks(lst)

  # Remove extra bottom line of landscape longtables
  longend_inds <- grep("\\\\end\\{longtable\\}", x)
  if(length(longend_inds)){
    longend_prev_inds <- longend_inds - 1
    longend_has_bottomrule <- grepl("\\\\bottomrule", x[longend_prev_inds])
    if(any(longend_has_bottomrule)){
      x <- x[-longend_prev_inds[longend_has_bottomrule]]
    }
  }

  x
}
