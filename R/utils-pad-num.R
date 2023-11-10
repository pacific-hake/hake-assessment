#' Pad the beginning of numbers in a vector with zeroes so they all have
#' the same number of characters
#'
#' @param x A vector of the numbers to pad
#' @param n_digits The number of characters that the resulting strings
#' should have
#' @param pad_char The character to pad with. Typically zero but can be any
#' character
#'
#' @return A vector of strings of the padded numbers
#' @export
pad_num <- function(x,
                    n_digits = 2,
                    pad_char = "0"){

  if(nchar(pad_char) != 1){
    stop("The `pad_char` must be a single character")
  }
  if(n_digits < 1){
    stop("The number of digits to make the strings (`n_digits`) must be ",
         "1 or greater")
  }

  if(any(x < 0)){
    stop("This function only pads positive numbers")
  }

  x_chr <- as.character(x)
  n_char_x <- nchar(x_chr)
  greater_flags <- n_char_x > n_digits

  if(any(greater_flags)){
    stop("Some values are greater than `n_digits` (", n_digits, ")")
  }

  n_lead_0 <- n_digits - n_char_x
  map2_chr(x_chr, n_lead_0, \(str, n_0){
    lead_zero_str <- paste(rep(pad_char, n_0), collapse = "")
    paste0(lead_zero_str, str)
  })
}

