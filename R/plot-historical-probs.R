#' Plots to compare historical and current probabilities
#'
#' Plot either:
#' (a) base model's estimate of P(B_t+1 < B_t) and compare with the estimate of
#' that from the year t assessment (using the known catch in year t)
#' (b) base model's estimate of P(B_t+1 < B_40%) and compare with the estimates
#' of that from the year t assessment (using the known catch in year
#' t).
#' Included in management presentation.
#'
#' @param model A model object, created by [create_rds_file()]
#' @param hist_probs A data frame containing the data found in
#' `assessment-history-probs.csv`
#' @param type "decline" to show probs of spawning biomass declining in year
#' after historical assessment year, "decline_one_year" to show that for just
#' one year (to explain in a talk), "bforty" to show prob of being below
#' `B_40\\%` in the year after the historical assessment year
#' @param end final year for calculations
#' @param xLim range of x (years) axis
#' @param add.50 Whether to add horizontal line at 50%
#' @param add.50.col Color for 50% line
#' @param one.year A single year to plot (may automatically work for more
#' years)
#' @param add.projs Whether to add future projections from current base model
#' @param num.projs Number of projection catch levels to show
#' @param colors The colors to use for the lines in the plot
#' @param pch change legend call also if change this
#' @param lwd.val Line width
#' @param legend.cex Legend font size
#' @param legend.loc Legend location
#' @param main.title Main title text
#' @param legend.text Legent title text
#' @param legend.inset Vector for how much to shift the legend, gets parsed to
#'   `legend( , inset)` value, so see `?legend`. Will depend on figure size.
#' @param add.to.plot whether to add to an existing plot
#' @param ... Arguments passed to [combine_historical_probs()]
#'
#' @return A base R plot
#' @export
plot_historical_probs <- function(
    model,
    hist_probs,
    type = "decline",
    end = assess_yr - 1,
    xLim = NULL,
    add.50 = TRUE,
    add.50.col = "darkgrey",
    one.year = 2019,
    add.projs = FALSE,
    num.projs = ct_levels_num,
    colors = c("red", "blue"),
    pch = c(16, 17),
    lwd.val = 1,
    legend.cex = 1,
    legend.loc = "bottomright",
    main.title = NULL,
    legend.text =
      c("From year t's assessment",
        "From current base model"),
    legend.inset = c(0,0),
    add.to.plot = FALSE,
    ...){

  res <- combine_historical_probs(model = model,
                                  hist_probs = hist_probs,
                                  end = end,
                                  ...)
  if(add.to.plot){
    if(type == "decline"){
      points(res$year,
             res$P_decline_curr,
             type = "o",
             pch = pch[2],
             col = colors[2],
             lwd = lwd.val)
      if(add.projs){
        segments(rep(assess_yr - 1, num.projs),
                 rep(res$P_decline[length(res$P_decline)], num.projs),
                 rep(assess_yr, num.projs),
                 model$risks[[1]][1:num.projs,
                                  paste0("SSB_", assess_yr + 1, "<SSB_",
                                         assess_yr)] %>% pull(),
                 pch = pch[1],
                 col = c("pink",
                         rep(colors[1], num.projs - 1)),
                 lwd = lwd.val)
        points(rep(assess_yr, num.projs),
               model$risks[[1]][1:num.projs,
                                paste0("SSB_", assess_yr + 1, "<SSB_",
                                       assess_yr)] %>% pull(),
               pch = pch[1],
               col = c("pink",
                       rep(colors[1], num.projs - 1)),
               lwd = lwd.val)
      }
    }

    if(type == "bforty"){
      points(res$year,
             res$P_below_B40_curr,
             type = "o",
             pch = pch[2],
             col = colors[2],
             lwd = lwd.val)
      if(add.projs){
        segments(rep(assess_yr - 1, num.projs),
                 rep(res$P_below_B40[length(res$P_below_B40)], num.projs),
                 rep(assess_yr, num.projs),
                 model$risks[[1]][1:num.projs,
                                  paste0("Bratio_", assess_yr + 1, "<0.40")] %>%
      pull(),
                 pch = pch[1],
                 col = c("pink",
                         rep(colors[1], num.projs - 1)),
                 lwd = lwd.val)
        points(rep(assess_yr, num.projs),
               model$risks[[1]][1:num.projs,
                                paste0("Bratio_", assess_yr + 1, "<0.40")] %>% pull(),
               pch = pch[1],
               col = c("pink",
                       rep(colors[1], num.projs - 1)),
               lwd = lwd.val)
      }
    }
  } else {      # create new plot
    if(is.null(xLim)) xLim = range(res$year) + c(0, 1)
    oldpar <- par("mar", "xpd")
    on.exit(par(oldpar))
    par(mar = c(4.5, 4.5, 1, 1))

    if(type == "decline"){
      plot(res$year,
           res$P_decline,
           xlim = xLim,
           type = "o",
           pch = pch[1],
           col = colors[1],
           xlab = "Year, t",
           ylab = "P(biomass declines from t to t+1)",
           ylim = c(0, 100),
           main = main.title,
           lwd = lwd.val)
      points(res$year,
             res$P_decline_curr,
             type = "o",
             pch = pch[2],
             col = colors[2],
             lwd = lwd.val)
      if(add.projs){
        segments(rep(assess_yr - 1, num.projs),
                 rep(res$P_decline[length(res$P_decline)], num.projs),
                 rep(assess_yr, num.projs),
                 model$risks[[1]][1:num.projs,
                                  paste0("SSB_", assess_yr + 1, "<SSB_",
                                         assess_yr)] %>% pull(),
                 pch = pch[1],
                 col = c("pink",
                         rep(colors[1], num.projs - 1)),
                 lwd = lwd.val)
        points(rep(assess_yr, num.projs),
               model$risks[[1]][1:num.projs,
                                paste0("SSB_", assess_yr + 1, "<SSB_",
                                       assess_yr)] %>% pull(),
               pch = pch[1],
               col = c("pink",
                       rep(colors[1], num.projs - 1)),
               lwd = lwd.val)
      }
    }

    if(type == "decline_one_year"){
      plot(res$year,
           res$P_decline,
           xlim = xLim,
           type = "o",
           pch = pch[1],
           col = "white",    # to get axes correct
           xlab = "Year, t",
           ylab = "P(biomass declines from t to t+1)",
           ylim = c(0, 100),
           main = main.title,
           lwd = lwd.val)

      res_one_year <- res[which(res$year == one.year), ]

      points(res_one_year$year,
             res_one_year$P_decline,
             type = "o",
             pch = pch[1],
             col = colors[1],
             lwd = lwd.val)

      points(res_one_year$year,
             res_one_year$P_decline_curr,
             type = "o",
             pch = pch[2],
             col = colors[2],
             lwd = lwd.val)

      text(rep(res_one_year$year, 2),
           c(res_one_year$P_decline,
             res_one_year$P_decline_curr),
           labels = c(paste0(res_one_year$P_decline, "%"),
                      paste0(round(res_one_year$P_decline_curr), "%")),
           col = colors,
           pos=2)
    }


    if(type == "bforty"){
      plot(res$year,
           res$P_below_B40,
           xlim = xLim,
           type = "o",
           pch = pch[1],
           col = colors[1],
           xlab = "Year, t",
           ylab = "P(biomass is below B40% in year t+1)",
           ylim = c(0, 100),
           lwd = lwd.val)
      points(res$year,
             res$P_below_B40_curr,
             type = "o",
             pch = pch[2],
             col = colors[2],
             lwd = lwd.val)

      if(add.projs){
        segments(rep(assess_yr - 1, num.projs),
                 rep(res$P_below_B40[length(res$P_below_B40)], num.projs),
                 rep(assess_yr, num.projs),
                 model$risks[[1]][1:num.projs,
                                  paste0("Bratio_", assess_yr + 1, "<0.40")] %>%
        pull(),
                 pch = pch[1],
                 col = c("pink",
                         rep(colors[1], num.projs - 1)),
                 lwd = lwd.val)
        points(rep(assess_yr, num.projs),
               model$risks[[1]][1:num.projs,
                                paste0("Bratio_", assess_yr + 1, "<0.40")] %>% pull(),
               pch = pch[1],
               col = c("pink",
                       rep(colors[1], num.projs - 1)),
               lwd = lwd.val)
      }

    }

    if(add.50){
      abline(h = 50,
             col = add.50.col) # extended past axes for some reason
    }

    legend(legend.loc,
           legend.text,
           col = colors,
           lty = 1,
           lwd = lwd.val,
           pch = pch,
           cex = legend.cex,
           bty = "y",
           bg = "transparent",
           inset = legend.inset)
  }
}
