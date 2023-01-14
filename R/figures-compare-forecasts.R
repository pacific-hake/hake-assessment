make.forecast.catch.posterior.plot <- function(model,         ## model is the model for which mcmc runs with different forecasts
                                                              ## has been made
                                               fore.yr,       ## Forecast year to make the plot with
                                               xmax = 4000,   ## max x value (could be more dynamic)
                                               do.plot = TRUE ## If FALSE, no plot will be drawn, but the return values will be returned
                                               ){
  if(do.plot){
    oldpar <- par()
  }
  dat <- model$mcmc
  dat <- eval(parse(text = paste0("dat$ForeCatch_", fore.yr))) / 1000.0
  med.catch <- median(dat)
  dens <- density(dat, from = min(dat))
  dens.orig <- dens
  if(do.plot){
    plot(dens,
       yaxt = "n",
       ylab = "",
       xlab = paste0("Projected ",fore.yr," catch based on the default harvest policy (1,000 t)"),
       type = "l",
       lty = 1,
       pch = 16,
       xlim = c(0, xmax),
       xaxs = "i",
       yaxs = "i",
       ylim = c(0, max(dens$y) * 1.02), lwd = 3, main="", xaxt = "n")

    axis(1, at = seq(0, xmax, 200), lab=rep("", length(seq(0, xmax, 200))))
    axis(1, at = seq(0, xmax, 400))
    mtext("Density", side = 2, line = 0.5, las = 0)
  }
  yy <- dens
  # vector of x and y values associated with output from "density"
  yy$x <- c(min(dens$x), dens$x, max(dens$x))
  yy$y <- c(-1, dens$y, -1)
  if(do.plot){
    polygon(yy$x, yy$y, col = gray(0.9), lwd = 3)
  }
  # subsetting values for those within the 95% interval
  ind <- dens$x >= quantile(dat, 0.025) & dens$x <= quantile(dat, 0.975)
  dens$x <- dens$x[ind]
  dens$y <- dens$y[ind]
  yy$x <- c(min(dens$x), dens$x, max(dens$x))
  yy$y <- c(-1, dens$y, -1)
  if(do.plot){
    polygon(yy$x, yy$y, col = rgb(0, 0, 1, 0.3), lty = 0)
    lines(dens.orig, lwd = 3)
  }
  if(do.plot){
    tmpy <- dens$y[min(abs(dens$x - med.catch)) == abs(dens$x - med.catch)]
    lines(c(med.catch, med.catch), c(0, tmpy), lwd = 2)
    text(med.catch, mean(c(0, tmpy)), paste0("Median = ", round(med.catch,3)), srt = 90, adj = c(0.5, -0.5))
    box()
    par <- oldpar
  }
  if(!do.plot){
    # return interval (but exact, not rounded to nearest value of the
    # 512 x-values used by "density"
    ret.vec <-c(quantile(dat, 0.025), med.catch, quantile(dat, 0.975))
    names(ret.vec) <- c("lower", "median", "upper")
    return(ret.vec)
  }
}

make.forecast.risk.comparison.plot <- function(model,        ## model is the model for which mcmc runs with different forecasts
                                                             ## has been made
                                               forecast_yrs, ## a vector of the years which were forecast,
                                                             ## used in conjunction with fore.yr to determine which
                                                             ##  forecast list element is used
                                               fore.yr,      ## Forecast year for probabilities
                                               colors = c("black","blue","green","orange","red","tan"),
                                                             ## color is a vector of colors for the lines. This must be the same length
                                                             ## as the number of rows model$risks data frames has
                                               pch = c(16,17,17,17,15,18),
                                                             ## pch is a vector of symbols. This must be the same length
                                                             ## as the number of rows model$risks data frames has
                                               legend.cex = 0.7,   ## Text size for the legend
                                               legend.loc = "topleft",
                                               xlim = NULL){
  oldpar <- par("mar", "xpd")
  on.exit(par(oldpar))
  par(mar = c(4.5, 4.5, 1, 1), xpd = TRUE) # xpd=TRUE allows points to overlap box around plot
  prob.dat <- model$risks[fore.yr == forecast_yrs][[1]]
  ## Sort the table by catches
  prob.dat <- prob.dat[order(prob.dat[, 1]),]
  ## Divide all the percentages by 100 to get probabilities
  prob.dat[,-1] <- prob.dat[, -1] / 100.0
  ## Divide all catches by 1000 to get megatonnes
  catches <- round(prob.dat[, 1] / 1000.0, 0)
  ## Remove catches from the table
  prob.dat <- prob.dat[, -1]
  ## Remove last three columns, which are DFO reference points
  prob.dat <- prob.dat[,-((ncol(prob.dat) - 2):ncol(prob.dat))]

  legend.text <- c(paste0("P(B", fore.yr + 1, "<B", fore.yr, "): Stock declines in ", fore.yr + 1),
                   paste0("P(B", fore.yr + 1, "<B40%)"),
                   paste0("P(B", fore.yr + 1, "<B25%)"),
                   paste0("P(B", fore.yr + 1, "<B10%)"),
                   paste0("P(", fore.yr, " relative fishing intensity > 100%)"),
                   paste0("P(", fore.yr + 1, " default harvest policy catch < ", fore.yr, " catch)"))
  if(is.null(xlim)){
    xlim <- c(0, max(catches, na.rm = TRUE))
  }
  # make empty plot
  matplot(catches,
          prob.dat,
          xlim = xlim,
          ylim = c(0, 1),
          las = 1,
          yaxs = "i",
          xaxt = "n",
          ylab = "Probability",
          xlab = paste0("Catch in ", fore.yr, " (1,000 t)"),
          type = "n")
  # add x-axis
  axis(1,
       at = catches,
       cex.axis = 0.9,
       las = 2)
  # add plot with points (now on top of axes)
  matplot(catches,
          prob.dat,
          add = TRUE,
          type = "b",
          lty = 2,
          pch = pch,
          col = colors)
  # restore clipping to plot region
  par(xpd = FALSE)
  # add line at 50% and horizontal axis
  abline(h = 0.5,
         lty = 2,
         lwd = 1,
         col = "grey")

  # add legend
  legend(legend.loc,
         legend.text,
         col = colors,
         lty = 1,
         lwd = 2,
         pch = pch,
         cex = legend.cex,
         bty = "n")
}

make.forecast.risk.comparison.plot.short <- function(model,        ## model is the model for which mcmc runs with different forecasts
                                               ## has been made
                                               forecast_yrs, ## a vector of the years which were forecast,
                                               ## used in conjunction with fore.yr to determine which
                                               ##  forecast list element is used
                                               fore.yr,      ## Forecast year for probabilities
                                               colors = c("black","blue","tan"),
                                               ## color is a vector of colors for the lines. This must be the same length
                                               ## as the number of rows model$risks data frames has
                                               pch = c(16,17,18),
                                               ## pch is a vector of symbols. This must be the same length
                                               ## as the number of rows model$risks data frames has
                                               legend.cex = 0.7,   ## Text size for the legend
                                               legend.loc = "topleft",
                                               xlim = NULL){
  oldpar <- par("mar", "xpd")
  on.exit(par(oldpar))
  par(mar = c(4.5, 4.5, 1, 1), xpd = TRUE) # xpd=TRUE allows points to overlap box around plot
  prob.dat <- model$risks[fore.yr == forecast_yrs][[1]]
  ## Sort the table by catches
  prob.dat <- prob.dat[order(prob.dat[, 1]),]
  ## Divide all the percentages by 100 to get probabilities
  prob.dat[,-1] <- prob.dat[, -1] / 100.0
  ## Divide all catches by 1000 to get megatonnes
  catches <- round(prob.dat[, 1] / 1000.0, 0)
  ## Remove catches from the table
  prob.dat <- prob.dat[, -1]
  ## Remove last three columns, which are DFO reference points
  prob.dat <- prob.dat[,-((ncol(prob.dat) - 2):ncol(prob.dat))]

  prob.dat <- prob.dat[, c(1, 2, 4)]

  legend.text <- c(paste0("P(B", fore.yr + 1, "<B", fore.yr, "): Stock declines in ", fore.yr + 1),
                   paste0("P(B", fore.yr + 1, "<B40%)"),
                   paste0("P(B", fore.yr + 1, "<B10%)"))
  if(is.null(xlim)){
    xlim <- c(0, max(catches, na.rm = TRUE))
  }
  # make empty plot
  matplot(catches,
          prob.dat,
          xlim = xlim,
          ylim = c(0, 1),
          las = 1,
          yaxs = "i",
          xaxt = "n",
          ylab = "Probability",
          xlab = paste0("Catch in ", fore.yr, " (1,000 t)"),
          type = "n")
  # add x-axis
  axis(1,
       at = catches,
       cex.axis = 0.9,
       las = 2)
  # add plot with points (now on top of axes)
  matplot(catches,
          prob.dat,
          add = TRUE,
          type = "b",
          lty = 2,
          pch = pch,
          col = colors)
  # restore clipping to plot region
  par(xpd = FALSE)
  # add line at 50% and horizontal axis
  abline(h = 0.5,
         lty = 2,
         lwd = 1,
         col = "grey")

  # add legend
  legend(legend.loc,
         legend.text,
         col = colors,
         lty = 1,
         lwd = 2,
         pch = pch,
         cex = legend.cex,
         bty = "n")
}
