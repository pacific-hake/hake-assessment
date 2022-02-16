
##' Trying a histogram to replace the overly smoothed density plot of HCR catch
##' ##'
##' @param model
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' @ontrun{
##' @
##' @}
make.forecast.catch.posterior.hist <- function(model,         ## model is the model for which mcmc runs with different forecasts
                                                              ## has been made
                                               fore.yr,       ## Forecast year to make the plot with
                                               xmax = 4000,   ## max x value (could be more dynamic)
                                               do.plot = TRUE, ## If FALSE, no
                                               ## plot will be drawn, but the
                                               ## return values will be returned
                                               num.bins.in.cred = 30
                                               ){
  if(do.plot){
    oldpar <- par()
  }
  dat <- model$mcmc
  dat <- eval(parse(text = paste0("dat$ForeCatch_", fore.yr))) / 1000.0
  med.catch <- median(dat)

  cred.interval <- quantile(dat, probs = c(0.025, 0.975))
  bin.width <- diff(cred.interval)/num.bins.in.cred

  min.bin.break = cred.interval[1] - ceiling(cred.interval[1]/bin.width) * bin.width
    # First bin will start <0 and sometimes not be empty, if min(dat) is within
    #  first bin). May want to fix properly if fig looks funny.
  max.bin.break = cred.interval[2] +
    ceiling((max(dat) - cred.interval[2])/bin.width) * bin.width
  bin.breaks = seq(min.bin.break,
                    max.bin.break,
                   by = bin.width)
  par(mfrow = c(2,1))
  hist(dat,
       breaks = bin.breaks)

  hist(dat[dat<cred.interval[1]],
       breaks = bin.breaks,
       add = TRUE,
       col = "red")
  hist(dat[dat>cred.interval[2]],
       breaks = bin.breaks,
       add = TRUE,
       col = "red")

  hpdi <- HDInterval::hdi(dat)
  bin.breaks.hpdi =
#DO A FUNCTION FOR THE BIN BREAKS (Above) if needed more
#  HERE

br <- base.model$mcmc[["Bratio_2022"]]

> > quantile(br, probs = c(0.025, 0.975))
     2.5%     97.5%
0.3105285 1.3473200
> HDInterval::hdi(br)
   lower    upper
0.261039 1.233090
attr(,"credMass")
[1] 0.95
  >


  > br <- base.model$mcmc[["Bratio_2022"]]
> > quantile(br, probs = c(0.025, 0.975))
     2.5%     97.5%
0.3105285 1.3473200
> HDInterval::hdi(br)
   lower    upper
0.261039 1.233090
attr(,"credMass")
[1] 0.95

  R_2010 <- base.model$mcmc[["Recr_2010"]]
> (list "" '(("x" . "") ("dec.points" . "0")) '("x" "dec.points" "dll" "lib" "env" "nativeRoutines"))
> f(quantile(R_2010, probs = c(0.025, 0.975)))
        2.5%        97.5%
"11,200,560" "32,817,810"
> f(HDInterval::hdi(R_2010))
       lower        upper
"10,122,800" "29,072,100"
>

  This doesn't change so much because it's presumably more symmetric:
R2014overR2010 <- base.model$mcmc[["Recr_2014"]] / base.model$mcmc[["Recr_2010"]]
> quantile(R2014overR2010, probs = c(0.025, 0.975))
     2.5%     97.5%
0.4504541 0.6674952
> HDInterval::hdi(R2014overR2010)
    lower     upper
0.4415496 0.6541714
attr(,"credMass")
[1] 0.95

hist(R2014overR2010, breaks = 50)

R2020overR2010 <- base.model$mcmc[["Recr_2020"]] / base.model$mcmc[["Recr_2010"]]
sum(R2020overR2010 > 1)/length(R2020overR2010)
(list "" '(("x" . "")) '("x" "..."))
 0.04897959


> R2022overR2010 <- base.model$mcmc[["Recr_2022"]] / base.model$mcmc[["Recr_2010"]]
> sum(R2022overR2010 > 1)/length(R2022overR2010)
[1] 0.02865473

check 2017 fishing intensity interval. - still overlaps 1 (1.072 vs 1.057 for
                                                           HDI) - since
                                                           it's a ratio I think it's less likely to change much since more symmetric of a distribution.
#  dens <- density(dat, from = min(dat))
#  dens.orig <- dens
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
