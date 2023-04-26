#' Ramp colors given to the length of the number of ages in the data frame `w`
#'
#' @param w A data frame containing a column named `age` which is numeric
#' @param cols A vector of the colors to use to build the color ramp
#' @param ... Absorbs arguments intended for other functions
#'
#' @return A vector of the colors ramped by the function. The vector will
#' be the number of ages minus 1 found in the `age` column of the `w` data
#' frame
#' [grDevices::colorRampPalette()]
heatmap_set_colors <- function(w = NULL,
                               cols = c("red",
                                        "yellow",
                                        "green",
                                        "dodgerblue"),
                                        ...){

  stopifnot(!is.null(w))
  stopifnot("age" %in% names(w))

  ages <- as.numeric(levels(unique(w$age)))
  nage <- length(ages)
  seed_cols <- cols
  ncols <- nage - 1
  col_func <- colorRampPalette(seed_cols)
  colors <- col_func(nage - 1)

  colors
}