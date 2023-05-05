#' Updates [ggplot2::label_parsed()] to accommodate spaces
#'
#' @param labels Labels to use
label_parsed_space <- function(labels) {

  labels <- label_value(labels, multi_line = TRUE)
  labels <- lapply(labels, function(y) gsub(" ", "~", y))
  lapply(unname(labels), lapply, function(values) {
    c(parse(text = as.character(values)))
  })
}
