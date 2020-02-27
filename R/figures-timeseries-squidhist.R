#' Histogram of the Ratio of Squid-Plot Estimates Across Cohorts
#' 
#' Histogram of the ratio of the recruitment estimate at a specific age
#' relative to the current estimate of that cohort across all cohorts.
#' Output from \code{\link{make.squid.plot}}, a data frame, is manipulated
#' to compute the ratios and plotted in the histogram.
#' 
#' @details
#' Histograms are reported using a log-scaled x axis because ratios are not
#' symmetric about zero. Results should be centered around one.
#' 
#' @param squidoutput A data frame returned from \code{\link{make.squid.plot}}.
#' @param minage The minimum age to appear in the histogram.
#' @param maxage The maximum age to appear in the histogram.
#' @param plot A logical value specifying if a figure should be generated.
#' @param ylim The y-axis limits if \code{plot = TRUE}. The default,
#' \code{NULL}, allows \code{\link[ggplot2]{ggplot}} to pick the limits.
#' @param file A path to save the plot to.
#' @import ggplot2
#' @import importFrom stats reshape
#' @export
#' @author Kelli Faye Johnson
#'
make.squid.hist <- function(
  squidoutput,
  minage = 2,
  maxage = 3,
  plot = TRUE,
  ylim = NULL,
  file = NULL) {
  n <- max(unique(squidoutput$age))
  squidoutput <- squidoutput[order(squidoutput$cohort, squidoutput$age), ]
  crap <- tapply(seq(NROW(squidoutput)), squidoutput$cohort,
    function(x) {
      temp <- rep(NA, n+1)
      temp[squidoutput$age[x]+1] <- squidoutput$yval[x] / tail(squidoutput$yval[x], 1)
      return(temp)
    })
  out <- as.data.frame(t(do.call(rbind, crap)))
  rownames(out) <- 0:(NROW(out)-1)
  longd <- out
  colnames(longd) <- paste("variable", colnames(longd), sep = "_")
  longd <- stats::reshape(longd, direction = "long", sep = "_",
    varying = seq(NCOL(longd)),
    timevar = "cohort", idvar = "age")
  longd$age <- longd$age - 1
  longd <- longd[!is.na(longd$variable), ]
  rownames(longd) <- NULL
  g <- ggplot(longd[longd$age >= minage & longd$age <= maxage, ],
    aes(x = variable, color = age, fill = age, group = factor(age))) +
    geom_histogram() +
    scale_x_log10() +
    xlab("Ratio to most recent estimate across cohorts") +
    theme(legend.background = element_rect(color = NA),
      legend.position = c(0.9,0.9)) +
    coord_cartesian(xlim = NULL, ylim = ylim)
  if (plot) {
    print(g)
  }
  if (!is.null(file)) {
    ggsave(g, file = file)
  }
  return(g)
}
