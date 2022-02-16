##' See HCR-histogram.R for running it for now.
##'
##' Calculate the 95% credible interval of a vector of MCMC samples of a
##'  quantity in two different ways, and optionally plot histogram
##'
##' Calculate the 95% credible interval of the vector as either the highest
##'  posterior density interval (hpdi)
##'  or the usual, and simpler, interval between the 2.5% and 97.5%
##'  quantiles. The hpdi is narrower (or equal for a symmetric distribution)
##'  and has several advantages, particularly as a decision tool
##'  (e.g. https://www.sciencedirect.com/topics/mathematics/highest-density-interval).
##'  Optionally return a histogram showing the samples within and outside the
##'  credible interval (by calculating bin breaks to exactly to include the
##'  bounds of the interval); this is preferable to showing a density type plot
##'  as that requires some smoothing.
##'
##' @param vec a vector of MCMC samples of a quantity
##' @param credible.mass a scalar [0,1] specifying the mass within the credible
##'   interval (gets passed to `HDIinterval::hdi()` as `credMass`)
##' @param num.bins.in.cred number of bins to show within the credible interval
##'   for the histogram.
##' @param interval.type "hpdi" for the highest posterior density interval
##'   (calculated using `HDIinterval::hdi()`, or "equal" for equal tailed, as
##'   traditionally done in the hake assessment).
##' @param do.plot plot the histogram and return the results, else just return
##'   the results.
##' @param ... Further arguments passed to `hist()`.
##' @return **
##' @export
##' @author Andrew Edwards
##' @examples
##' @dontrun{
##' @
##' @}
make.posterior.intervals <- function(vec,
                                     credible.mass = 0.95,
                                     num.bins.in.cred = 30,
                                     interval.type = "hpdi",
                                     do.plot = TRUE,
                                     ...){
  stopifnot(interval.type %in% c("hpdi", "equal"))

  if(interval.type == "hpdi"){
    cred.interval <- HDInterval::hdi(vec,
                                     credMass = credible.mass)
    main.title = "Highest density interval"
  } else {
    cred.interval <- quantile(vec,
                              probs = c((1 - credible.mass)/2,
                                        1 - (1 - credible.mass)/2))
    main.title = "Equal-tailed interval"
    #names(cred.interval) = c("lower",
    #                         "upper")  # to match hpdi names
  }

  bin.width <- diff(cred.interval)/num.bins.in.cred

  min.bin.break = cred.interval[1] - ceiling(cred.interval[1]/bin.width) * bin.width
    # First bin will start <0 and sometimes not be empty, if min(vec) is within
    #  first bin). May want to fix properly if fig looks funny.
  max.bin.break = cred.interval[2] +
    ceiling((max(vec) - cred.interval[2])/bin.width) * bin.width
  bin.breaks = seq(min.bin.break,
                   max.bin.break,
                   by = bin.width)
  hist(vec,
       breaks = bin.breaks,
       main = main.title)

  hist(vec[vec<cred.interval[1]],
       breaks = bin.breaks,
       add = TRUE,
       col = "red")
  hist(vec[vec>cred.interval[2]],
       breaks = bin.breaks,
       add = TRUE,
       col = "red")

  # TODO return
}
