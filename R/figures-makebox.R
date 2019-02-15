#' Make a Box Plot from a Data Frame
#' 
#' @param data A data frame of quantiles by year. The function
#' assumes that the depth data are in meters.
#' @param col The default is to use gray for all boxes, but
#' one can change to "colour" which will use custom colours
#' per year.
#' @param lty To change the line type.
#' @param labels Character values for the horizontal and
#' vertical axes labels.
#' @param ... Items passed to \code{bxp}. Typically this will
#' include things such as "main".
#' @author Kelli Faye Johnson
#' @return A boxplot.
#' 
makebox <- function(data, col = c("gray", "colour"), lty = 1, 
  labels = c("Year", "Depth (m)"), ...) {
  colnames(data) <- tolower(colnames(data))
  keepcols <- c("lower95", "lowerhinge", "median", 
    "upperhinge", "upper95")
  x <- list(
    "stats" = t(data[, keepcols]),
    "out" = NULL, "group" = NULL, "conf" = NULL,
    "names" = data$year)
  col <- match.arg(col, several.ok = FALSE)
  if (col == "colour") {
    col <- plotcolour(nrow(x$stats))
  }
  pars <- list(boxfill = col, boxlty = 1)
  if (any(c("black", "#000000") %in% col) && 
    !"border" %in% names(pars)) {
    par("fg" = "gray") 
  }
  bxp(x, pars = pars, lty = lty, border = "gray", las = 1, 
    xlab = labels[1], ylab = labels[2], ...)
}