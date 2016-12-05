# estimating distribution of forecast catch at age in first forecast year
# quickly assembed at JMC meeting 3/18/16
# this file is messy and not generalized but it shouldn't be too hard to generalize it
dir <- "c:/github/hake-assessment/models/55_2016base/partest/reports/"

# empty tables
sel.table <- NULL
selwt.table <- NULL
natage.table <- NULL

# unique text to identify rows
sel.text1 <- "2016_1Asel"
sel.text2 <- "2016_1_sel*wt"
natage.text <- "Z_AT_AGE_Annual_2 With_fishery"

# loop over all report files to read them, find the location of lines of interest,
# and then read those lines as tables
# NOTE: THIS TAKES A WHILE AND SHOULD BE COMBINED WITH THE "PARTEST" STUFF
for(irep in 1:999){
  if(irep %% 100 == 0){
    cat("irep:", irep, "\n")
  }
  rep <- file.path(dir, paste0("Report_", irep, ".sso"))
  # need to reach each report file because length of "SPR/YPR_Profile"
  # may differ among files
  repLines <- readLines(rep)
  sel.line1 <- grep(sel.text1, repLines)
  sel.line2 <- grep(sel.text2, repLines, fixed=TRUE)
  # numbers at age for 2016 are 4 lines prior to Z_AT_AGE header
  natage.line <- grep(natage.text, repLines) - 4 # manually adjust -4 value

  # read individual rows as tables
  sel.row1 <- read.table(file=rep, skip=sel.line1-1, nrow=1)
  sel.row2 <- read.table(file=rep, skip=sel.line2-1, nrow=1)
  natage.row <- read.table(file=rep, skip=natage.line-1, nrow=1)

  # add rows to tables
  sel.table <- rbind(sel.table, sel.row1)
  selwt.table <- rbind(selwt.table, sel.row2)
  natage.table <- rbind(natage.table, natage.row)
}

# remove initial columns
natage.table.slim <- natage.table[,-(1:3)]
sel.table.slim <- sel.table[,-(1:7)]
selwt.table.slim <- selwt.table[,-(1:7)]

# selected biomass by age
natselwt <- natage.table.slim*selwt.table.slim
# selected numbers by age
natsel <- natage.table.slim*sel.table.slim

# check that things look reasonable
## matplot(0:20, t(sel.table.slim), type='l', col=rgb(0,0,0,.02), lwd=3, lty=1)
## matplot(0:20, t(selwt.table.slim), type='l', col=rgb(0,0,0,.02), lwd=3, lty=1)
## matplot(0:20, t(natage.table.slim), type='l', col=rgb(0,0,0,.02), lwd=3, lty=1, ylim=c(0,1e8))

# convert to proportions
natsel.prop <- natsel
natselwt.prop <- natselwt

for(irow in 1:999){
  natsel.prop[irow,] <- natsel[irow,]/sum(natsel[irow,])
  natselwt.prop[irow,] <- natselwt[irow,]/sum(natselwt[irow,])
}
  
## matplot(0:20, t(natsel.prop), type='l', col=rgb(0,0,0,.02), lwd=3, lty=1, ylim=c(0,1))
## matplot(0:20, t(natselwt.prop), type='l', col=rgb(0,0,0,.02), lwd=3, lty=1, ylim=c(0,1))

# png('c:/github/hake-assessment/beamer/JMC/Figures/expected
postscript(file.path("c:/github/hake-assessment/beamer/JMC/Figures/",
                     "expected_proportions_at_age.eps"),
           horizontal = FALSE, onefile = FALSE,
           paper = "special", height = 6, width = 10)

par(mfrow=c(1,2), oma=c(0.1,2,1,1), mar=c(4,3,3,0), cex.main=1)
# first plot with proportion of numbers
plot(0:20, apply(natsel.prop, 2, median), type='h', lend=3, col='grey', lwd=15,
     xlab="Age", ylab="Proportion", main="Proportion by numbers",
     ylim=c(0,.7), yaxs='i', axes=FALSE, xlim=c(.5, 20.5), xaxs='i')
axis(1, at=1:20, lab=rep(NA,20))
axis(1, at=seq(2,20,2), lab=seq(2,20,2))
axis(2, las=1)
box()
points(0:20, apply(natsel.prop, 2, median), pch=18, cex=1.3)
arrows(x0=0:20, x1=0:20,
       y0=apply(natsel.prop, 2, quantile, probs=0.025),
       y1=apply(natsel.prop, 2, quantile, probs=0.975),
       code=3, angle=90, length=0.05)
arrows(x0=0:20, x1=0:20,
       y0=apply(natsel.prop, 2, quantile, probs=0.25),
       y1=apply(natsel.prop, 2, quantile, probs=0.75),
       code=0, angle=90, length=0, lwd=3)

# second plot with proportion of weight
plot(0:20, apply(natselwt.prop, 2, median), type='h', lend=3, col='grey', lwd=15,
     xlab="Age", ylab="Proportion", main="Proportion by weight",
     ylim=c(0,.7), yaxs='i', axes=FALSE, xlim=c(.5, 20.5), xaxs='i')
axis(1, at=1:20, lab=rep(NA,20))
axis(1, at=seq(2,20,2), lab=seq(2,20,2))
axis(2, las=1)
box()
points(0:20, apply(natselwt.prop, 2, median), pch=18, cex=1.3)
arrows(x0=0:20, x1=0:20,
       y0=apply(natselwt.prop, 2, quantile, probs=0.025),
       y1=apply(natselwt.prop, 2, quantile, probs=0.975),
       code=3, angle=90, length=0.05)
arrows(x0=0:20, x1=0:20,
       y0=apply(natselwt.prop, 2, quantile, probs=0.25),
       y1=apply(natselwt.prop, 2, quantile, probs=0.75),
       code=0, angle=90, length=0, lwd=3)

mtext("Proportion", side=2, line=0, outer=TRUE)
mtext("Expected proportion in 2016 catch", side=3, line=0, outer=TRUE, font=2, cex=1.2)
dev.off()
