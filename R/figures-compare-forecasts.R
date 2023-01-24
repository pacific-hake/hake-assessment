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

