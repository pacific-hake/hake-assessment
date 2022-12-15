
make.selectivity.illustration <- function(
    mod.old,     # model with parameterization used in 2018 base model
    mod.alt,     # model with alternative parameterization
    yr1 = 1990,  # baseline year
    yr2 = 1991){ # year to illustrate with deviations
  # code to make figures illustrating parameterization of selectivity
  # using equations 39-40 of the 2017 hake assessment

  ## mod.old <- base.model
  ## mod.alt <- sens.models.4[[which(sens_model_dir_names_4=="2018.40.10_semiPara_tvSelect_sig0.695")]]

  ###########################################################
  # define two years, one for baseline and other with deviations
  yr1 <- 1990
  yr2 <- 1991
  ages <- 0:20 # range of ages for which selectivity is defined
  a_min <- 1   # minimum age with selectivity (1 for fishery, 2 for survey)

  # note: calculation of selectivity below is similar to what's done in
  #       randWalkSelex.fn which is in doc/r/r-functions/utilities.r
  #       but for the illustration the intermediate calculations are needed

  # get parameters for baseline fishery selectivity in status-quo approach
  selpar_base <- mod.old$SelAgeAdj[mod.old$SelAgeAdj$Yr==yr1 &
                                     mod.old$SelAgeAdj$Fleet==1,
                                   grep("Par", names(mod.old$SelAgeAdj))]
  # get parameters for baseline fishery selectivity in alternative approach
  selpar_base_alt <- mod.alt$SelAgeAdj[mod.alt$SelAgeAdj$Yr==yr1 &
                                         mod.alt$SelAgeAdj$Fleet==1,
                                       grep("Par", names(mod.alt$SelAgeAdj))]
  # get parameters for adjusted selectivity (yr2) in status-quo approach
  selpar_adj  <- mod.old$SelAgeAdj[mod.old$SelAgeAdj$Yr==yr2 &
                                     mod.old$SelAgeAdj$Fleet==1,
                                   grep("Par", names(mod.old$SelAgeAdj))]

  # placeholder for sum of parameters up to each age
  S_prime_base     <- rep(0, length(ages))
  S_prime_base_alt <- rep(0, length(ages))
  S_prime_adj      <- rep(0, length(ages))

  # for all ages above a_min calculate the sum
  for(a in ages[ages >= a_min]){
    S_prime_base[ages == a]     <- sum(selpar_base[ages %in% a_min:a])
    S_prime_base_alt[ages == a] <- sum(selpar_base_alt[ages %in% a_min:a])
    S_prime_adj[ages == a]      <- sum(selpar_adj[ages %in% a_min:a])
  }

  # scale relative to the max and exponentiate
  S_base <- exp(S_prime_base - max(S_prime_base))
  S_base_alt <- exp(S_prime_base_alt - max(S_prime_base_alt))
  S_adj <- exp(S_prime_adj - max(S_prime_adj))

  # set selectivity = 0 for all ages below a_min
  S_base[ages < a_min] <- 0
  S_base_alt[ages < a_min] <- 0
  S_adj[ages < a_min] <- 0

  # different way to get selectivity from model output to double check the results above
  S_base_B <- mod.old$ageselex[mod.old$ageselex$Factor=="Asel" &
                                 mod.old$ageselex$Fleet==1 &
                                   mod.old$ageselex$Yr==yr1, paste(0:20)]
  S_base_alt_B <- mod.alt$ageselex[mod.alt$ageselex$Factor=="Asel" &
                                     mod.alt$ageselex$Fleet==1 &
                                       mod.alt$ageselex$Yr==yr1, paste(0:20)]
  S_adj_B <- mod.old$ageselex[mod.old$ageselex$Factor=="Asel" &
                                mod.old$ageselex$Fleet==1 &
                                  mod.old$ageselex$Yr==yr2, paste(0:20)]
  S_adj_alt_B <- mod.alt$ageselex[mod.alt$ageselex$Factor=="Asel" &
                                    mod.alt$ageselex$Fleet==1 &
                                      mod.alt$ageselex$Yr==yr2, paste(0:20)]

  # selectivity deviations for semi-parametric selectivity
  dev_string <- paste0("Fishery_ARDEV_y", yr2)
  dev_labels <- grep(dev_string, mod.alt$parameters$Label, value=TRUE)
  dev_ages <- as.numeric(substring(dev_labels, first=nchar(dev_string)+3))
  dev_vals <- mod.alt$parameters$Value[mod.alt$parameters$Label %in% dev_labels]

  sigmasel <- mod.alt$parameters$Value[mod.alt$parameters$Label=="sigmasel_Fishery(1)_AGE(1)"]

  # compare baseline selectivity for status-quo model calculated both ways
  rbind(S_base[1:10], S_base_B[1:10])
  ##      0           1          2          3          4          5 6 7 8 9
  ## 1    0 0.011446663 0.12362321 0.36905965 0.52258272 0.66890047 1 1 1 1
  ## 1779 0 0.011446800 0.12362400 0.36906000 0.52258300 0.66890100 1 1 1 1


  ####################################################################################
  ### make plot showing both approaches

  ## function to label panels from Sean Anderson
  ## http://seananderson.ca/2013/10/21/panel-letters.html
  add_label <- function(label, xfrac=0.02, yfrac=0.07,
                        pos = 4, ...) {
    u <- par("usr")
    x <- u[1] + xfrac * (u[2] - u[1])
    y <- u[4] - yfrac * (u[4] - u[3])
    text(x, y, label, pos = pos, ...)
  }

  # subset of ages to show
  sub <- ages >= a_min

  xlim <- c(0,8)
  # define y-limits that can apply to both columns
  ylim1 <- c(-1,3.5)
  ylim2 <- c(0,1.1*max(S_prime_base[sub], S_prime_adj[sub],
                       S_prime_base_alt[sub], S_prime_adj[sub]))
  ylim3 <- c(0,1.05*max(1, as.numeric(S_adj_alt_B)))

  par(mfcol=c(3,2), mar=c(2,4.5,0,0), oma=c(2,0,3,1), cex=1, las=1)

  # first plot showing parameters by age
  plot(ages[sub], selpar_base[sub], type='l', lwd=4, axes=FALSE,
       ylim=ylim1, xlim=xlim, xlab="",
       ylab=expression(paste("Parameter value,   ", italic(p[a]))))
  add_label("(a)")
  axis(1, at=0:8); axis(2); box()
  lines(ages[sub], selpar_adj[sub], type='l', lwd=2, col=4)
  for(a in ages[sub]){
    lines(x = rep(a, 2),
          y = c(selpar_base[ages==a], selpar_adj[ages==a]),
          col=2, lwd=2)
  }
  abline(h=0, col='grey', lty=3)
  legend('topright', col=c(1,2,4), lty=c(1,1,1), lwd=c(4,2,2), bty='n',
         legend=c("baseline",
             paste("deviations"),
             paste("adjusted values")))
  mtext("Base model parameterization", side=3, line=1,
        outer=TRUE, adj=.2)

  # second plot showing cumulative sum of parameters
  plot(ages[sub], S_prime_base[sub], type='l', lwd=4, yaxs='i', axes=FALSE,
       xlim=xlim, ylim=ylim2,
       ylab=expression(paste("Cumulative total,   ", italic("S'"[a]))))
  add_label("(b)")
  axis(1, at=0:8); axis(2); box()
  lines(ages[sub], S_prime_adj[sub], type='l', lwd=2, col=4)

  # third plot showing resulting selectivity
  plot(ages, S_base, type='l', lwd=4, xlim=xlim, ylim=ylim3, yaxs='i', axes=FALSE,
       ylab=expression(paste("Resulting selectivity,   ", italic(S[a]))))
  add_label("(c)")
  axis(1, at=0:8); axis(2); box()
  lines(ages, S_adj, type='l', lwd=2, col=4)
  abline(h=1, col='grey', lty=3)

  ### plots for semi-parametric selectivity parameterization
  # first plot showing parameters by age
  plot(ages[sub], selpar_base_alt[sub], type='l', lwd=4,
       ylim=ylim1, xlim=xlim, xlab="", ylab="", axes=FALSE)
  add_label("(d)")
  axis(1, at=0:8); axis(2); box()
  abline(h=0, col='grey', lty=3)
  mtext("Semi-parametric parameterization", side=3, line=1,
        outer=TRUE, adj=.99)

  # second plot showing cumulative sum of parameters
  plot(ages[sub], S_prime_base_alt[sub], type='l', lwd=4, yaxs='i', axes=FALSE,
       xlim=xlim, xlab="", ylab="",
       ylim=ylim2)
  add_label("(e)")
  axis(1, at=0:8); axis(2); box()

  # third plot showing resulting selectivity
  plot(ages, S_base_alt_B, type='l', lwd=4, xlim=xlim, yaxs='i',  axes=FALSE,
       ylim=ylim3, xlab="", ylab="")
  add_label("(f)")
  axis(1, at=0:8); axis(2); box()
  lines(ages, S_adj_alt_B, type='l', lwd=2, col=4)
  abline(h=c(0, 1), col='grey', lty=3)
  # vertical lines showing deviations
  sub2 <- ages %in% 1:5
  for(a in ages[sub2]){
    lines(x = rep(a, 2),
          y = c(S_base_alt_B[ages==a],
              S_base_alt_B[ages==a]*exp(sigmasel*dev_vals[dev_ages==a])),
          col=2, lwd=2)
  }

  mtext('Age', side=1, line=0, outer=TRUE)
}

  ## # calculate time-varying selectivity in yr2 from value in yr1 and dev value
  ## S_base_alt_B[ages==3]*exp(sigmasel*dev_vals[dev_ages==3])
  ## # reported value for time-varying selectivity in yr2
  ## S_adj_alt_B[ages==3]
