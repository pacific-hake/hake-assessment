#' Plot catch data as a stacked barplot
#'
#' @param ct The data frame which is read in from
#' `inst/extdata/data/landings-tac-history.csv`
#' @param mar See [par()]
#' @param leg.y.loc Y-position for legend
#' @param leg.cex Text size for legend
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_catches <- function(ct,
                         mar = c(4, 4, 6, 2) + 0.1,
                         leg.y.loc = 430,
                         leg.cex = 1){

  oldpar <- par()
  on.exit(par(oldpar))

  yrs <- ct$Year
  ct[is.na(ct)] <- 0
  ct <- ct |>
    select(`Canada Foreign`,
           `Canada Joint-venture`,
           `Canada Shoreside`,
           `Canada Freezer-trawler`,
           `U.S. Foreign`,
           `U.S. Joint-venture`,
           `U.S. Mothership`,
           `U.S. Catcher-processor`,
           `U.S. Shore-based`)

  cols <- c(rgb(0, 0.8, 0),
            rgb(0, 0.6, 0),
            rgb(0.8, 0, 0),
            rgb(0.4, 0, 0),
            rgb(0, 0.2, 0),
            rgb(0, 0.4, 0),
            rgb(0, 0, 0.7),
            rgb(0, 0, 0.4),
            rgb(0, 0, 1))

  legOrder <- c(6, 5, 2, 1, 4, 3, NA, NA, 9, 8, 7)

  par(las = 1,
      mar = mar,
      cex.axis = 0.9)

  # Gives x-values for tick marks (since years not in ct)
  tmp <- barplot(t(as.matrix(ct)) / 1000,
                 beside = FALSE,
                 names = ct$year,
                 col = cols,
                 xlab = "Year",
                 ylab = "",
                 cex.lab = 1,
                 xaxt = "n",
                 xaxs = "i",
                 mgp = c(2.2, 1, 0),
                 ylim = c(0, 475))

  grid(NA, NULL, lty = 1, lwd = 1)
  mtext("Catch (thousand t)",
        side = 2,
        line = 2.8,
        las = 0,
        cex = 1.3)
  barplot(t(as.matrix(ct)) / 1000,
          beside = FALSE,
          names = ct$year,
          col = cols,
          xlab = "Year",
          ylab = "",
          cex.lab = 1,
          xaxt = "n",
          xaxs = "i",
          add = TRUE,
          mgp = c(2.2, 1, 0))
  # Big tick every five years:
  axis(1,
       at = tmp[(yrs %% 5) == 0],
       lab = yrs[(yrs %% 5) == 0])
  axis(1,
       at = tmp,
       lab = rep("",length(tmp)), tcl = -0.3)

  legend(x = 0,
         y = leg.y.loc,
         names(ct)[legOrder],
         bg = "white",
         horiz = FALSE,
         xpd = NA,
         cex = leg.cex,
         ncol = 3,
         fill = cols[legOrder],
         border = cols[legOrder],
         bty = "n")
}
