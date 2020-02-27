#' @param squidoutput A data frame returned from \code{\link{make.squid.plot}}.
#' @param smooth An integer value passed to \code{\link[ggplot2]{geom_density}}
#' that specifies the amount of binning in the histogram.
#' @param minage The minimum age to appear in the histogram.
#' @param plot A logical value specifying if a figure should be generated.
#' @param xlim The x-axis limits if \code{plot = TRUE}. The default,
#' \code{NULL}, allows \code{\link[ggplot2]{ggplot}} to pick the limits.
#' @param ylim The y-axis limits if \code{plot = TRUE}. The default,
#' \code{NULL}, allows \code{\link[ggplot2]{ggplot}} to pick the limits.
#' @param file A path to save the plot to.
#' @import ggplot2
#' @export
#' @author Kelli Faye Johnson
make.squid.hist <- function(
  squidoutput,
  smooth = 4,
  minage = 2,
  maxage = 3,
  plot = TRUE,
  xlim = NULL,
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
    aes(x = variable, color = age, group = factor(age))) +
    geom_density(adjust = smooth) +
    xlab("Ratio to most recent estimate") +
    theme(legend.background = element_rect(color = NA),
      legend.position = c(0.9, 0.85)) +
    coord_cartesian(xlim = xlim, ylim = ylim)
  if (plot) {
    print(g)
  }
  if (!is.null(file)) {
    ggsave(g, file = file)
  }
  return(out)
}
