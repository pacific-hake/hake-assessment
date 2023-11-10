#' Ramp colors given to the length of the number of ages in the data frame `w`
#'
#' @param wa A data frame containing columns named by numbers which are ages
#'
#' @param cols A vector of the colors to use to build the color ramp
#' @param ... Absorbs arguments intended for other functions
#'
#' @return A vector of the colors ramped by the function. The vector will
#' be the number of ages minus 1 found in the `wa` data frame. The color
#' ramp is provided by [grDevices::colorRampPalette()]
heatmap_set_colors <- function(wa = NULL,
                               cols = c("red",
                                        "yellow",
                                        "green",
                                        "dodgerblue"),
                                        ...){

  stopifnot(!is.null(wa))
  stopifnot(is.data.frame(wa))

  ages <- names(wa) %>%
    grep("^\\d+$", ., value = TRUE) |>
    as.numeric()
  if(!length(ages)){
    stop("No numeric column names were found in the supplied ",
         "data frame `wa`")
  }
  num_ages <- length(ages)
  col_func <- colorRampPalette(cols)
  colors <- col_func(num_ages - 1)

  colors
}