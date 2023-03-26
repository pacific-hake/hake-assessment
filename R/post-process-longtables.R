#' Post-process the longtables in TEX code
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_longtables <- function(x){

  beg_inds <- grep("\\\\begin\\{longtable\\}", x)
  end_inds <- grep("\\\\end\\{longtable\\}", x)
  if(!length(beg_inds)){
    return(x)
  }
  if(length(beg_inds) > length(end_inds)){
    stop("There were more `\\begin{longtable}` commands than ",
         "`\\end{longtable}` commands.",
         call. = FALSE)
  }
  if(length(beg_inds) < length(end_inds)){
    stop("There were less `\\begin{longtable}` commands than ",
         "`\\end{longtable}` commands.",
         call. = FALSE)
  }
  j <- extract_chunks(x, beg_inds, end_inds)
  # get the first row starting with a year (table data)
  k <- grep("^[0-9]{4}", j$between[[1]], value = TRUE)[1]
  # Extract the number of columns in the table
  n_col <- str_count(k, "&") + 1
  # Get location of `\\endfirsthead` and paste "Continued from" line after it
  kk <- j$between[[1]]
  efh <- grep("endfirsthead", kk)
  pre <- kk[1:(efh)]
  post <- kk[(efh + 1):length(kk)]
  kk <- c(pre,
          paste0(
            "\\multicolumn{",
            n_col,
            "}{l}{\\textit{... Continued from previous page}} \\\\ \\hline"),
          post)
  # Get location of `\\endhead` and paste "Continued on" line after it
  efh <- grep("endhead", kk)
  pre <- kk[1:(efh)]
  post <- kk[(efh + 1):length(kk)]
  kk <- c(pre,
          paste0(
            "\\hline \\multicolumn{",
            n_col,
            "}{l}{\\textit{Continued on next page ...}} \\\\"),
          post)
  # Remove caption and toprule from second page
  cap2 <- grep("caption\\[\\]", kk)
  pre <- kk[1:(cap2 - 1)]
  # Assumes `\\toprule` follows `\\caption[]` directly
  post <- kk[(cap2 + 2):length(kk)]
  kk <- c(pre, post)

  j$between[[1]] <- kk

  interlace_chunks(j)
}
