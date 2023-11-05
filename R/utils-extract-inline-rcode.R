#' Replace all occurrences of inline R chunks with the evaluation of the
#' code inside them for an arbitrarily complex string
#'
#' @param str The string to evaluate, containing zero or more instances of
#' `` `r code_here` `` inline R chunks
#'
#' @return A string, without any inline r chunks
#' @export
#'
#' @examples
#' extract_inline_rcode("Testing `r Sys.time()` one two")
extract_inline_rcode <- function(str){

  if(is.null(str)){
    # The presentations need this because they do not have header/footer
    # text
    return(NULL)
  }

  x <- strsplit(str, "`r")[[1]]

  if(length(x) == 1){
    return(str)
  }
  wch_code <- grep("`", x)
  # Store trailing spaces for non-code
  code <- gsub("(.*)`.*", "\\1", x[wch_code])
  noncode <- gsub(".*`(.*)", "\\1", x[wch_code])

  merged <- map2(trimws(code), noncode, ~{
    code <- eval(parse(text = .x))
    paste0(code, .y)
  })
  x[wch_code] <- merged
  paste(unlist(x), collapse = "")
}

test_extract_inline_rcode <- function(){

  a <- paste0("DRAFT -- `r sp` ",
              "assessment `r assess_yr`")
  message("Original: ", a)
  message("Output: ", extract_inline_rcode(a), "\n")

  a <- paste0("Hello there XXX `lnjkljn`")
  message("Original: ", a)
  message("Output: ", extract_inline_rcode(a), "\n")

  a <- "Testing `r Sys.time()` one two"
  message("Original: ", a)
  message("Output: ", extract_inline_rcode(a), "\n")

  a <- paste0("`r sp` ",
              "assessment `r assess_yr`")
  message("Original: ", a)
  message("Output: ", extract_inline_rcode(a), "\n")

  a <- paste0("`r sp`")
  message("Original: ", a)
  message("Output: ", extract_inline_rcode(a), "\n")

  a <- paste0("`r sp` `r f(last_data_yr, 2)` `r assess_yr` `r Sys.time()`")
  message("Original: ", a)
  message("Output: ", extract_inline_rcode(a), "\n")

  a <- paste0("XX `r sp``r f(last_data_yr, 2)``r assess_yr``r Sys.time()`  test")
  message("Original: ", a)
  message("Output: ", extract_inline_rcode(a), "\n")

}
