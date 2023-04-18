#' Move certain table captions, usually to left-justify. There is no way
#' to center kable tables and left-justify the captions to the left side
#' of the page
#'
#' @param x Tex code, as a vector of lines read in from a TeX file by
#' [readLines()]
#'
#' @return The modified Tex code, as a vector
#' @export
post_process_move_table_captions <- function(x){

  # Move a single caption horizontally
  # @param x Tex code, as a vector of lines read in from a TeX file by
  # [readLines()]
  # @param tag The chunk tag found in the knitr code chunk header in which
  # the table is created. Includes leading `tab:` part
  # @param pts The number of `pts` to move the caption. Can be positive or
  # negative
  move_cap <- \(x, tag, pts){

    pat <- paste0("\\\\caption\\{\\\\label\\{",
                  tag,
                  "\\}")
    ind <- grep(pat, x)
    if(length(ind)){
      lst <- post_process_extract_chunks(x, ind, ind)
      lst$inbetween[[1]] <- c(lst$inbetween[[1]],
                              paste0("\\captionsetup{margin=", pts, "pt}"))
      post_process_interlace_chunks(lst)
    }else{
      warning("Did not match the table tag `", tag, "` in the TeX file. ",
              "The table caption was not set be moved as intended")
      x
    }
  }

  # Note that all these tags have to be in the ORDER IN WHICH THEY APPEAR
  # IN THE DOCUMENT! The values are determined by trial-and-error looking at
  # the document after it's built
  x <- move_cap(x, "tab:main-catches-us", -4)
  x <- move_cap(x, "tab:main-assessment-changes-table", -26)
  x <- move_cap(x, "tab:main-est-numbers-at-age-table", -12)
  x <- move_cap(x, "tab:main-est-exp-rate-at-age-table", -14)
  x <- move_cap(x, "tab:main-est-catch-at-age-table", -6)
  x <- move_cap(x, "tab:main-ci-posterior-table", -16)
  x <- move_cap(x, "tab:main-risk-forecast-year-1-table", -4)
  x <- move_cap(x, "tab:main-risk-forecast-year-2-table", -4)
  x <- move_cap(x, "tab:main-risk-forecast-year-3-table", -4)
  x <- move_cap(x, "tab:main-parameter-estimates-sens-1", -26)

  x
}