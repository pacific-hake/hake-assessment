
# estimating distribution of forecast catch at age in first forecast year
# this file contains two functions:
#  * get.forecast.age.info (gathers info required for plot)
#  * make.age.comp.forecast.plot (creates the plot)

get.forecast.age.info <- function(model,
                                  output.file = "age-comp-forecast-info.RData",
                                  nsamples = 999,
                                  verbose = TRUE){

  # get info on expected numbers at age, selectivity, and weight-at-age,
  # from first year of forecast and save as RData file

  extra.mcmc.dir <- file.path(model$path, "extra-mcmc")

  # empty tables
  sel.table <- NULL
  selwt.table <- NULL
  natage.table <- NULL

  # unique text to identify rows

  # something like "2017_1Asel"
  sel.text1 <- model$ageselex$label[model$ageselex$year==model$endyr+1 &
                                      model$ageselex$factor=="Asel"]
  # something like "2017_1_sel*wt"
  sel.text2 <- model$ageselex$label[model$ageselex$year==model$endyr+1 &
                                      model$ageselex$factor=="sel*wt"]
  natage.text <- "Z_AT_AGE_Annual_2 With_fishery"

  # loop over all report files to read them, find the location of lines of interest,
  # and then read those lines as tables
  # NOTE: THIS TAKES A WHILE AND SHOULD BE COMBINED WITH THE "extra-mcmc" STUFF
  cat("Gathering info on forecast comps from", nsamples, "report files in\n",
      file.path(extra.mcmc.dir,"reports"),
      "\nThis may take a while\n")
  for(irep in 1:nsamples){
    if(irep %% 100 == 0){
      cat("irep:", irep, "out of", nsamples, "\n")
    }
    rep <- file.path(extra.mcmc.dir, paste0("reports/Report_", irep, ".sso"))

    # need to reach each report file in full to find position of
    # various quantities because the length of "SPR/YPR_Profile"
    # may differ among files
    repLines <- readLines(rep)
    sel.line1 <- grep(sel.text1, repLines)
    sel.line2 <- grep(sel.text2, repLines, fixed=TRUE)

    # read individual rows of selectivity info
    sel.row1 <- read.table(file=rep, skip=sel.line1-1, nrow=1)
    sel.row2 <- read.table(file=rep, skip=sel.line2-1, nrow=1)

    # read numbers at age table based on start and end lines and length of table
    natage.line.start <- grep("NUMBERS_AT_AGE_Annual_2 With_fishery", repLines)
    natage.line.end <- grep("Z_AT_AGE_Annual_2 With_fishery", repLines)-3
    natage.N.lines <- natage.line.end - natage.line.start
    natage.allrows <- read.table(file=rep, skip=natage.line.start,
                                 nrow=natage.N.lines, header=TRUE)
    # subset all rows to select first forecast year
    natage.row <- natage.allrows[natage.allrows$Year==model$endyr+1,]

    # add rows to tables of values for each MCMC sample
    sel.table <- rbind(sel.table, sel.row1)
    selwt.table <- rbind(selwt.table, sel.row2)
    natage.table <- rbind(natage.table, natage.row)
  }

  # remove initial columns (containing stuff like Gender and Year)
  natage.table.slim <- natage.table[,-(1:3)]
  sel.table.slim <- sel.table[,-(1:7)]
  selwt.table.slim <- selwt.table[,-(1:7)]

  # selected biomass by age is product of numbers*selectivity*weight at each age
  natselwt <- natage.table.slim*selwt.table.slim
  # selected numbers by age is product of numbers*selectivity at each age
  natsel <- natage.table.slim*sel.table.slim

  # check that things look reasonable
  ## matplot(0:20, t(sel.table.slim), type='l', col=rgb(0,0,0,.02), lwd=3, lty=1)
  ## matplot(0:20, t(selwt.table.slim), type='l', col=rgb(0,0,0,.02), lwd=3, lty=1)
  ## matplot(0:20, t(natage.table.slim), type='l', col=rgb(0,0,0,.02), lwd=3, lty=1, ylim=c(0,1e8))

  # define new objects to store proportions by age
  natsel.prop <- natsel
  natselwt.prop <- natselwt

  # create tables of proportions by dividing by sum of each row
  for(irow in 1:nsamples){
    natsel.prop[irow,] <- natsel[irow,]/sum(natsel[irow,])
    natselwt.prop[irow,] <- natselwt[irow,]/sum(natselwt[irow,])
  }

  # make a list and save it to the chosen output file
  forecast.age.info <- list(natsel.prop = natsel.prop,
                            natselwt.prop = natselwt.prop)
  cat("saving results to",file.path(extra.mcmc.dir, output.file),"\n")
  save(forecast.age.info, file = file.path(extra.mcmc.dir, output.file))
}

##################################################################################

make.age.comp.forecast.plot <- function(model,
                                        make.missing.RData.file=FALSE,
                                        dir = "extra-mcmc",
                                        info.file = "age-comp-forecast-info.RData"){
  # make plot of estimated distribution of forecast catch at age
  # in first forecast year

  # dir is directory relative to model$path where RData is located
  # info.file is the name of the RData file created by get.forecast.age.info
  RData.file <- file.path(model$path, dir, info.file)
  if(file.exists(RData.file)) {
    # load output from get.forecast.age.info
    load(RData.file)
  } else {
    cat0("Can't find file: ", RData.file, "\n",
        " which is required by make.age.comp.forecast.plot\n")
    if(make.missing.RData.file){
      cat0(" Running get.forecast.age.info to build RData file\n",
          " containing info on forecast age comp...\n")
      get.forecast.age.info(model)
      load(RData.file)
    } else {
      cat0(" make.missing.RData.file is set to FALSE so not making\n",
          " plot of age comp for first forecast year.\n")
      return()
    }
  }
  
  # create objects for # expected proportion selected by numbers and by weight
  natsel.prop <- forecast.age.info$natsel.prop
  natselwt.prop <- forecast.age.info$natselwt.prop

  # define multi-figure layout and margins
  par(mfrow=c(1,2), oma=c(0.2,2,1.2,1), mar=c(4,3,3,0), cex.main=1)

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
       xlab="Age", ylab="Proportion", main="Proportion by weight",
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
  mtext("Proportion", side=2, line=0, outer=TRUE)
  mtext(paste("Expected proportion of each cohort in the",model$endyr+1,"catch"),
        side=3, line=0, outer=TRUE, font=2, cex=1.2)
  #dev.off()
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
