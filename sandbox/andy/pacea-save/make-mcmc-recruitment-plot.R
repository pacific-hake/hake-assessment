# Saving this from R/figures-mcmc-diagnostics.R to have the base plot version
# handy (Chris is switching it to ggplot version).

## Adapting from make.mcmc.depletion.plot, and values for make.recruitment.plot
## Plot of MCMC samples of recruitment, to help for Issue #836.
##
## make.mcmc.recruitment.plot(base_model, start_yr = start_yr, equil.yr = unfished_eq_yr)
## make.mcmc.recruitment.plot(base_model, start_yr = 2006, equil.yr = unfished_eq_yr, samples = 100)
## make.mcmc.recruitment.plot(base_model, start_yr = 2006, equil.yr = unfished_eq_yr, samples = 100, rescale = TRUE)
## make.mcmc.recruitment.plot(base_model, start_yr = 1966, equil.yr = unfished_eq_yr, samples = NULL, rescale = TRUE, traceplot = FALSE)
make.mcmc.recruitment.plot <- function(model,         ## model is a model with an mcmc run which has the output of the
                                                    ##  r4ss package's function
                                                    ##  SSgetMCMC and has the extra.mcmc member
                                       equil.yr,
                                       start_yr,      ## Year to start the plot
                                       end_yr = 2023, ## Year to end the plot
                                       plot_R0 = TRUE,## Show R0 (only tested on
                                                      ## 2023 fig)
                                       plot_rescale_yr = TRUE,  ## Line at 1.0
                                       plot_rescale_yr_col = "darkgreen",
                                       y.max = NULL,  ## maximum value for the y-axis
                                       samples = 1000,## how many lines to show
                                       rescale = FALSE, ## whether to rescale by
                                                        ## a certain year's recruitment
                                       rescale.yr = 2010,  # year to rescale by
                                         # if rescaling, not fully functional
                                         # yet though
                                       traceplot = TRUE,   # do a traceplot of
                                         # MCMC samples else a median and 95%
                                       # interval plot
                                       highlight = FALSE,  # whether to
                                         # just highlight some colour-code
                                         # samples instead of many same colour
                                         # ones (superimposing is too busy)
                                       highlight.num = 5,   # how many to
                                         # highlight, then highlight them
                                         # equally spaced out (in order of 2010
                                         # recruitment), plus the 2nd and
                                         # penultimate samples.
                                       highlight.cols = c("darkgreen",
                                                          "red",
                                                          "pink",
                                                          "black",
                                                          "orange",
                                                          "gold",
                                                          "brown",
                                                          "darkgrey",
                                                          "violet",
                                                          "black",
                                                          "lightgrey"),
                                       highlight.lwd = 2,
                                       auto.xaxis = TRUE,  # label normally, else
                                       # use bigticks
                                       col.intervals.not.rescale = "blue", # intervals col
                                             # when not rescaling
                                       col.intervals.rescale = "red", # col when
                                       # rescaling
                                       trace.col.not.rescale = rgb(0, 0, 1,
                                                                   0.2), # paleblue
                                       trace.col.rescale = rgb(1, 0, 0, 0.2)
                                       ){
  oldpar <- par()

  par(las = 1, mar = c(5, 4, 1, 1) + 0.1, cex.axis = 0.9)

  dat <- as_tibble(model$mcmc) %>%
    select(c("Recr_Virgin",
             paste0("Recr_", start_yr):paste0("Recr_", end_yr))) / 1e6  # convert
                                        # from thousands to billions

  if(rescale){
    dat <- dat / dat$"Recr_2010"      # generalise ***
    if(is.null(y.max)){
      y.max <- 1.2                    # so will miss some super high ones, but
                                      # easier to see others
    }

    dat_R0 <- select(dat, "Recr_Virgin")  # Just R0
    dat <- select(dat, -"Recr_Virgin")

    col.intervals <- col.intervals.rescale
    trace.col  <- trace.col.rescale
    yLab <- paste0("Age-0 recruits relative to those in ", rescale.yr)
  } else {
    yLab <- "Age-0 recruits (billions)"
    col.intervals <- col.intervals.not.rescale
    trace.col  <- trace.col.not.rescale
  }

  # subsamble to help the lines be more visible
  nsamp <- nrow(dat)    # total samples
  if(!is.null(samples)){
    subsample <- floor(seq(1, nsamp, length.out=samples)) # subset (floor to get
                                                          # integers)
    dat_sub <- dat[subsample, ]
  } else {                                                # no subsampling
    dat_sub <- dat
  }

  if(is.null(y.max)){
    y.max <- max(max(dat_sub) * 0.8)  # so will miss some super high ones, but
                                      # easier to see others
  }

  plot(0,
       type = 'n',
       xlim = c(start_yr-0.5, end_yr+0.5),
       ylim = c(0, y.max),
       axes = FALSE,
       xlab = "Year",
       ylab = yLab)

  abline(h = 0, col = "lightgrey")
  # paleblue <- rgb(0, 0, 1, 0.2)

  if(traceplot){
    if(!highlight){
      matplot(x = start_yr:end_yr,
            y = t(dat_sub),
            col = trace.col,
            type = 'l',
            add=TRUE,
            lty = 1)
    }
    if(highlight){
      highlight.rows <- c(2,
                          round(1:(highlight.num-1) * (samples/(highlight.num -
                                                                1))))
      highlight.rows[length(highlight.rows)] <-
        highlight.rows[length(highlight.rows)] - 1 # so penultimate
      dat_sub_highlight <- as_tibble(dat_sub) %>%
        arrange(Recr_2010)
      dat_sub_highlight <- dat_sub_highlight[highlight.rows, ]
      matplot(x = start_yr:end_yr,
              y = t(dat_sub_highlight),
              col = highlight.cols,
              type = 'l',
              add=TRUE,
              lty = 1,
              lwd = highlight.lwd)
    }
  } else {        # medians and intervals
    if(plot_R0){
      R0_0.025 <- apply(dat_R0, 2, quantile, probs = 0.025)
      R0_med <- apply(dat_R0, 2, median)
      R0_0.975 <- apply(dat_R0, 2, quantile, probs = 0.975)
      rect(start_yr - 5,
           R0_0.025,
           end_yr + 5,
           R0_0.975,
           col = "lightgrey",
           border = NA)
      abline(h = R0_0.025, lty = 3)
      abline(h = R0_med, lty = 2)
      abline(h = R0_0.975, lty = 3)
    }

    # Plot horizontal line at 1:
    if(plot_rescale_yr){
      abline(h = 1,
             col = plot_rescale_yr_col,
             lty = 2)
    }

    lower <- apply(dat_sub, 2, quantile, probs = 0.025)
    medians <- apply(dat_sub, 2, median)
    upper <- apply(dat_sub, 2, quantile, probs = 0.975)


    yrs <- start_yr:end_yr
    points(yrs, medians, pch = 20)
    segments(x0 = yrs,
             y0 = lower,
             x1 = yrs,
             y1 = upper,
             col = col.intervals)
  }

  box()
  if(auto.xaxis){
    axis(1, at = start_yr:end_yr) #cex.axis = 0.8, tcl = -0.6)
    } else {
    axis(1, at = big_ticks)
    axis(1,
         at = small_ticks,
         lab = rep("",length(small_ticks)), tcl = -0.3)
    }
  axis(2)  #, at = 0:5, las = 1)
  par <- oldpar
}
