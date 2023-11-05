#' Place a LaTeX `\\makecell{}` macro around some text
#'
#' @details
#' This function is a wrapper around [kableExtra::linebreak()] which itself
#' does not wrap the makecell macro around strings without the linebreak
#' character. For table headers, this needs to be done to keep all headers
#' aligned the same way with respect to each other.
#'
#' @param x A vector of character strings
#' @param align Choose from "l", "c" or "r" for "left", "center", and "right"
#' respectively
#' @param ... Arguments passed to [kableExtra::linebreak()]
#'
#' @return A vector of modified character strings or `NULL` if `x` is `NULL` or
#' `NA` if `x` is `NA`. Any empty strings will remain the same, without
#' wrapping text
#' @export
linebreaker <- function(x,
                        align = c("l", "c", "r"),
                        ...){

  align <- match.arg(align)
  modded_x <- linebreak(x, align = align, ...)

  # Wrap the strings not wrapped by `linebreak()` above
  inds <- grepl("^\\\\makecell.*\\}", modded_x)
  modded_x[!inds] <- paste0("\\makecell[", align, "]{", modded_x[!inds], "}")

  # Replace empty strings if there were any
  inds <- grepl("^\\\\makecell\\[(l|c|r)\\]\\{\\}$", modded_x)
  modded_x[inds] <- ""

  # Replace NA strings if there were any
  inds <- grepl("^\\\\makecell\\[(l|c|r)\\]\\{NA\\}$", modded_x)
  modded_x[inds] <- NA

  # If NULL was passed in, we will have a zero-length vector here
  if(!length(modded_x)){
    return(NULL)
  }

  modded_x
}
