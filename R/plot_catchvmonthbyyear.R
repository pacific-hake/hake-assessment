#' Plot a line of time-series data
#'
#' Plot a line of time-series data by month.
#'
#' @param x A vector of months in numeric form.
#' Also, commonly known as the `period`.
#' @param y Dependent variable in a vector that is the same length as `x`.
#' Typically, this will be monthly catches for a given year.
#' @param plotType The type of plot you wish to create, where the y-axis
#' type will change based on the entry. The default of `default` will lead
#' to the raw data being plotted. `proportion` plots the proportion and
#' `cumulative` plots the cumulative value for that year.
#' @param ... Arguments passed to [graphics::lines]
#' @return A line of the dependent variable by month
#' is added to the current figure.
#'
lines.bymonth <- function(x, y,
                          plotType = c("default", "proportion", "cumulative"),
                          ...) {
  plotType <- match.arg(plotType, several.ok = FALSE)
  if (plotType[1] == "proportion") {
    y <- cumsum(y) / sum(y)
  }
  if (plotType[1] == "cumulative") {
    y <- cumsum(y)
  }
  xx <- 1:12
  yy <- rep(NA, 12)
  for (i in xx) {
    if (any(x == i)) { # is the month present in the data
      yy[i] <- y[which(x == i)]
    } else {
      if (i == 1) {
        yy[i] <- 0
      } else {
        if (plotType[1] == "default") {
          yy[i] <- 0
        } else {
          yy[i] <- yy[i - 1]
        }
      }
    }
  }

  graphics::lines(xx, yy, type = "b", pch = 20, ...)
}

#' Plot catch versus month by year
#'
#' @param A data frame of landings by month. For example,
#'   `us_ms_catch_by_month_df`.
#' @param file A path to a png file where you want the plot to be saved to. The
#'   default is `NULL`, which leads to the figure being returned rather than
#'   saved to a file.
#' @param title todo: document
#' @param Yrs A vector of years that you want to plot.
#' @param quotas A named vector of quotas for the fleet of interest. The years
#'   in `Yrs` must be present in the vector.
#' @param leg.cex The font size for the legend.
#' @param divisor A numeric value used to determine if you want the figure to
#'   be in metric tons or in kg (i.e., 1000 mt).
#'
plot_catchvmonthbyyear <- function(data,
                                   file = NULL,
                                   title,
                                   Yrs,
                                   quotas = NULL,
                                   leg.cex = 1.2,
                                   divisor = 1000) {
  data <- as.data.frame(data)
  lineWds <- c(rep(2, length(Yrs) - 1), 3)
  lineTypes <- rep(1, length(Yrs))
  cols <- plot_color(length(Yrs))
  # Take hake  and
  # plots year specific catches by month
  # Does not discriminate by fleet
  # Assumes that there is one observation of Month in each year

  plot.base <- function(ylim = c(0, 1.05), ylab) {
    plot(1, 1,
      xlim = c(1, 12), ylim = ylim,
      type = "n", xaxt = "n",
      xlab = "", ylab = ylab, yaxs = "i"
    )
  }

  if (!is.null(file)) {
    png(
      height = 5, width = 10,
      units = "in", pointsize = 10, res = 300,
      file = file
    )
    on.exit(dev.off(), add = TRUE)
  }
  cex.title <- c(3, 1.3)[1]
  par(
    mfrow = c(2, 2),
    mar = c(0.5, 3.1, 0.5, 0.1),
    oma = c(4, 0.1, ceiling(cex.title), 0.1),
    mgp = c(2.1, 0.75, 0),
    las = 1
  )
  units <- paste0("(", ifelse(divisor != 1, paste(divisor, " "), ""), "mt)")
  dat.yr <- split(
    data[data[, "year"] %in% Yrs, ],
    data$year[data[, "year"] %in% Yrs]
  )
  plot.base(
    ylab = paste("Catch ", units),
    ylim = c(0, max(data[data[, "year"] %in% Yrs, "catch"]) / divisor)
  )
  for (i in 1:length(Yrs)) {
    lines.bymonth(
      dat.yr[[Yrs[i]]]$month,
      dat.yr[[Yrs[i]]]$catch / divisor,
      col = cols[i], lwd = lineWds[i], lty = lineTypes[i]
    )
  }
  axis(side = 1, mgp = c(1.0, 0, 0), labels = NA)

  plot.base(
    ylab = paste0("Cumulative Catch ", units),
    ylim = c(0, max(
      unlist(lapply(dat.yr, function(x) {
        sum(x$catch)
      }))[Yrs],
      na.rm = TRUE
    ) / divisor)
  )
  for (i in 1:length(Yrs)) {
    lines.bymonth(
      dat.yr[[Yrs[i]]]$month,
      dat.yr[[Yrs[i]]]$catch / divisor,
      plotType = "cumulative",
      col = cols[i], lwd = lineWds[i], lty = lineTypes[i]
    )
  }
  legend("topleft",
    legend = Yrs, col = cols,
    lty = lineTypes, lwd = lineWds, cex = leg.cex, bty = "n"
  )
  axis(side = 1, mgp = c(1.0, 0, 0), labels = NA)

  plot.base(ylab = "Proportion of Total Catch")
  abline(h = 1, col = gray(0.5))
  for (i in 1:length(Yrs)) {
    lines.bymonth(
      dat.yr[[Yrs[i]]]$month,
      dat.yr[[Yrs[i]]]$catch / divisor,
      plotType = "proportion",
      col = cols[i], lwd = lineWds[i], lty = lineTypes[i]
    )
  }
  axis(side = 1)

  plot.base(ylab = "Proportion of Sector Quota")
  abline(h = 1, col = gray(0.5))
  for (i in 1:length(Yrs)) {
    if (is.null(dim(dat.yr[[Yrs[i]]])[1])) next
    dat.yr[[Yrs[i]]] <- rbind(
      dat.yr[[Yrs[i]]],
      c(
        13,
        as.numeric(Yrs[i]),
        quotas[[Yrs[i]]] - sum(dat.yr[[Yrs[i]]][, "catch"])
      )
    )
    lines.bymonth(
      dat.yr[[Yrs[i]]]$month,
      dat.yr[[Yrs[i]]]$catch / divisor,
      plotType = "proportion",
      col = cols[i], lwd = lineWds[i], lty = lineTypes[i]
    )
  }
  axis(side = 1)
  mtext(
    side = 1, outer = TRUE,
    text = "Month",
    line = 1.5
  )
  mtext(
    side = 3, outer = TRUE,
    text = title,
    line = -0.1, cex = cex.title
  )
}
