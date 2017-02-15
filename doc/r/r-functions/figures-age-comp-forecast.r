make.age.comp.forecast.plot <- function(model){
  # make plot of estimated distribution of forecast catch at age
  # in first forecast year

  # create objects for # expected proportion selected by numbers and by weight
  natsel.prop <- model$extra.mcmc$natsel.prop
  natselwt.prop <- model$extra.mcmc$natselwt.prop

  # define multi-figure layout and margins
  par(mfrow=c(1,2), oma=c(0.2,2,0,1), mar=c(4,3,3,0), cex.main=1)

  # calculate max y-value for vertical axis
  ymax <- 1.05*max(apply(natsel.prop, 2, quantile, probs=0.975),
                   apply(natselwt.prop, 2, quantile, probs=0.975))

  # first plot with proportion of numbers
  plot(0:20, apply(natsel.prop, 2, median), type='h', lend=3, col='grey', lwd=15,
       xlab="Age", ylab="Proportion", main="Proportion by numbers",
       ylim=c(0, ymax), yaxs='i', axes=FALSE, xlim=c(.5, 15.5), xaxs='i')
  # add axes
  axis(1, at=1:20, lab=rep(NA,20))
  axis(1, at=seq(2,20,2), lab=seq(2,20,2))
  axis(2, at=seq(0,1,.1), las=1)
  box()
  # add points showing median estimates
  points(0:20, apply(natsel.prop, 2, median), pch=18, cex=1.3)

  # deal with warnings and make arrows
  old_warn <- options()$warn      # previous setting for warnings
  options(warn=-1)                # turn off "zero-length arrow" warning
  # add 95% intervals
  arrows(x0=0:20, x1=0:20,
         y0=apply(natsel.prop, 2, quantile, probs=0.025),
         y1=apply(natsel.prop, 2, quantile, probs=0.975),
         code=3, angle=90, length=0.05)
  # add 50% intervals
  arrows(x0=0:20, x1=0:20,
         y0=apply(natsel.prop, 2, quantile, probs=0.25),
         y1=apply(natsel.prop, 2, quantile, probs=0.75),
         code=0, angle=90, length=0, lwd=3)
  options(warn=old_warn)  #restore warnings to previous setting

  # second plot with proportion of weight
  plot(0:20, apply(natselwt.prop, 2, median), type='h', lend=3, col='grey', lwd=15,
       xlab="Age", ylab="",
       main="Proportion by weight",
       ylim=c(0, ymax), yaxs='i', axes=FALSE, xlim=c(.5, 15.5), xaxs='i')
  # add axes
  axis(1, at=1:20, lab=rep(NA,20))
  axis(1, at=seq(2,20,2), lab=seq(2,20,2))
  axis(2, at=seq(0,1,.1), las=1)
  box()
  # add points showing median estimates
  points(0:20, apply(natselwt.prop, 2, median), pch=18, cex=1.3)

  # deal with warnings and make arrows
  old_warn <- options()$warn      # previous setting for warnings
  options(warn=-1)                # turn off "zero-length arrow" warning
  # add 95% intervals
  arrows(x0=0:20, x1=0:20,
         y0=apply(natselwt.prop, 2, quantile, probs=0.025),
         y1=apply(natselwt.prop, 2, quantile, probs=0.975),
         code=3, angle=90, length=0.05)
  # add 50% intervals
  arrows(x0=0:20, x1=0:20,
         y0=apply(natselwt.prop, 2, quantile, probs=0.25),
         y1=apply(natselwt.prop, 2, quantile, probs=0.75),
         code=0, angle=90, length=0, lwd=3)
  options(warn=old_warn)  #restore warnings to previous setting

  # add labels in outer margins applying to both panels
  mtext(paste("Expected proportion in", model$endyr+1, "catch"),
        side=2, line=0, outer=TRUE)
  ## mtext(paste("Expected proportion of each cohort in the",model$endyr+1,"catch"),
  ##       side=3, line=0, outer=TRUE, font=2, cex=1.2)
}

# IGT 2017/01/30:
# make EPS file as a short-term measure for 2017 doc rather than
# make this figure dynamically
if(FALSE){
  setwd('r')
  cairo_ps(filename =  "../main-figures/main_age_comp_forecast.eps",
           width = 6.5, height = 4.5, pointsize = 10)
  # function call runs get.forecast.age.info to make RData file if needed
  make.age.comp.forecast.plot(base.model, make.missing.RData.file=TRUE)
  dev.off()
  setwd('..')

  # calculations that could also be made dynamic:
  round(100*quantile(forecast.age.info$natsel.prop$X3, c(0.025, 0.5, 0.975)))
  round(100*quantile(forecast.age.info$natsel.prop$X7, c(0.025, 0.5, 0.975)))
  round(100*quantile(forecast.age.info$natselwt.prop$X3, c(0.025, 0.5, 0.975)))
  round(100*quantile(forecast.age.info$natselwt.prop$X7, c(0.025, 0.5, 0.975)))
  ##  2.5%   50% 97.5% 
  ##    14    52    87 
  ##  2.5%   50% 97.5% 
  ##     7    27    52 
  ##  2.5%   50% 97.5% 
  ##    10    39    79 
  ##  2.5%   50% 97.5% 
  ##    12    36    59 

}
