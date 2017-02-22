make.parameters.estimated.summary.table <- function(model,                ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                                    start.rec.dev.yr,     ## First year of estimated recruitment devs
                                                    end.rec.dev.yr,       ## Last year of estimated recruitment devs
                                                    digits = 3,           ## number of decimal points for the estimates
                                                    xcaption = "default", ## Caption to use
                                                    xlabel   = "default", ## Latex label to use
                                                    font.size = 9,        ## Size of the font for the table
                                                    space.size = 10,      ## Size of the spaces for the table
                                                    return.xtable = TRUE  ## If FALSE, will return an R table,
                                                                          ##  the values of which can be referenced in knitr code chunks.
                                                    ){
  ## Returns an xtable in the proper format for the parameters estimated summary

  ## column indexes for the values as found in the control file
  lo <- 1
  hi <- 2
  init <- 3
  p.mean <- 4 ## prior mean
  p.type <- 5 ## prior type
  p.sd <- 6   ## prior sd
  phase <- 7
  start.yr.sel <- 10
  end.yr.sel <- 11
  sel.dev.sd <- 12

  prior.type <- c("-1" = "Uniform", "2" = "Beta", "3" = "Lognormal")

  fetch.and.split <- function(ctl, x){
    ## Fetch the line x from the vector ctl and split it up, removing spaces.
    ## Also remove any leading spaces..
    ## Return the vector of values
    j <- ctl[x]
    ## Remove inter-number spaces
    j <- strsplit(j," +")[[1]]
    ## Remove leading spaces
    j <- j[j != ""]
    return(j)
  }

  fetch.prior.info <- function(vals,
                               digits = 2){
    ## Looks at the prior type p.type and phase, and if uniform will return "Uniform"
    ## If not uniform, it will parse the vals and build a string defining the prior info.
    ## If Fixed, it will return the initial value
    ## If Lognormal, it will parse the vals and build a string defining the prior info, with the exp function applied..
    if(vals[p.type] < 0 & vals[phase] > 0){
      ## Uniform prior on estimated parameter
      return("Uniform")
    }
    if(vals[p.type] < 0 & vals[phase] < 0){
      ## Fixed parameter
      return(vals[init])
    }
    if(prior.type[vals[p.type]] == "Lognormal"){
      return(paste0(prior.type[vals[p.type]], "(",
                    f(exp(as.numeric(vals[p.mean])), digits), ",",
                    f(exp(as.numeric(vals[p.sd])), digits), ")"))
    }
    return(paste0(prior.type[vals[p.type]], "(",
                  f(as.numeric(vals[p.mean]), digits), ",",
                  f(as.numeric(vals[p.sd]), digits), ")"))
  }

  ctl <- model$ctl
  ## Remove all lines that start with a comment
  ctl <- ctl[-grep("^#.*", ctl)]

  ## R0 is at line 39 of comment-stripped dataframe. Get it's values which can be indexed by the variables defined above
  r0 <- fetch.and.split(ctl, 39)
  r0.vals <- c("Log(\\emph{R}\\subscr{0})", 1, paste0("(", r0[lo], ",", r0[hi], ")"), prior.type[r0[p.type]])

  ## Steepness is at line 40 of comment-stripped dataframe
  h <- fetch.and.split(ctl, 40)
  h.vals <- c("Steepness (\\emph{h})", 1, paste0("(", h[lo], ",", h[hi], ")"), fetch.prior.info(h, digits))

  ## Recruitment variability (sigma_r) is at line 41 of comment-stripped dataframe
  sig.r <- fetch.and.split(ctl, 41)
  sig.r.vals <- c("Recruitment variability (\\emph{$\\sigma_r$})",
                  if(sig.r[p.type] < 0 & sig.r[phase] > 0) 1 else "--",
                  if(sig.r[p.type] < 0 & sig.r[phase] > 0) paste0("(", sig.r[lo], ",", sig.r[hi], ")") else "NA",
                  fetch.prior.info(sig.r, digits))

  ## Recruitment devs, lower and upper bound found on lines 64 and 65 of comment-stripped dataframe
  ## The number of them comes from the arguments to this function (for now)
  rec.dev.lb <- fetch.and.split(ctl, 64)[1]
  rec.dev.ub <- fetch.and.split(ctl, 65)[1]
  rec.dev.vals <- c(paste0("Log recruitment deviations: ", start.rec.dev.yr, "--", end.rec.dev.yr),
                    end.rec.dev.yr - start.rec.dev.yr + 1,
                    paste0("(", rec.dev.lb, ",", rec.dev.ub, ")"),
                    "Lognormal(0,\\emph{$\\sigma_r$})")

  ## Natural mortality is at line 20 of comment-stripped dataframe
  m <- fetch.and.split(ctl, 20)
  m.vals <- c("Natural mortality (\\emph{M})",
              if(prior.type[m[p.type]] == "Fixed") "--" else 1,
              paste0("(", m[lo], ",", m[hi], ")"),
              fetch.prior.info(m, digits))

  ## Warning!! - Hard-coded for the hake assessment
  q.vals <- c("Catchability (\\emph{q})", 1, "NA", "Analytic solution")

  ## Survey additional value for SE is at line 78 of comment-stripped dataframe
  se <- fetch.and.split(ctl, 78)
  se.vals <- c("Additional value for survey log(SE)",
               if(prior.type[se[p.type]] == "Fixed") 1 else "--",
               paste0("(", se[lo], ",", se[hi], ")"),
               fetch.prior.info(se, digits))

  ## Number of survey selectivities is on line 83 of comment-stripped dataframe
  num.sel <- fetch.and.split(ctl, 83)
  grep.num.sel <- grep("#", num.sel)
  if(length(grep.num.sel) > 0){
    num.sel <- num.sel[1:(grep.num.sel - 1)]
  }
  num.sel <- as.numeric(num.sel[length(num.sel)])
  ## num.sel is the number of selectivity entries in the file for survey
  ## Age-0 starts on line 107 of comment-stripped dataframe
  line.num <- 107
  ages.estimated <- NULL
  for(i in line.num:(line.num + num.sel)){
    age.sel <- fetch.and.split(ctl, i)
    if(age.sel[phase] > 0){
      ## This age plus one is being estimated
      ages.estimated <- c(ages.estimated, i - line.num)
      ## Use the last line to get the values
      est.sel <- age.sel
    }
  }
  age.sel.vals <- c(paste0("Non-parametric age-based selectivity: ages ", min(ages.estimated), "--", max(ages.estimated)),
                    length(ages.estimated),
                    paste0("(", est.sel[lo], ",", est.sel[hi], ")"),
                    fetch.prior.info(est.sel, digits))

  ## Number of fishery selectivities is on line 82 of comment-stripped dataframe
  num.sel <- fetch.and.split(ctl, 82)
  grep.num.sel <- grep("#", num.sel)
  if(length(grep.num.sel) > 0){
    num.sel <- num.sel[1:(grep.num.sel - 1)]
  }
  num.sel <- as.numeric(num.sel[length(num.sel)])
  ## num.sel is the number of selectivity entries in the file for survey
  ## Age-0 starts on line 85 of comment-stripped dataframe
  line.num <- 85
  ages.estimated <- NULL
  for(i in line.num:(line.num + num.sel)){
    age.sel <- fetch.and.split(ctl, i)
    if(age.sel[phase] > 0){
      ## This age plus one is being estimated
      ages.estimated <- c(ages.estimated, i - line.num)
      ## Use the last line to get the values
      est.sel <- age.sel
    }
  }
  f.age.sel.vals <- c(paste0("Non-parametric age-based selectivity: ages ", min(ages.estimated), "--", max(ages.estimated)),
                      length(ages.estimated),
                      paste0("(", est.sel[lo], ",", est.sel[hi], ")"),
                      fetch.prior.info(est.sel, digits))

  ## Selectivity deviations for fishery. Uses last line to get values, assumes all are the same
  f.age.sel.dev.vals <- c(paste0("Selectivity deviations (", est.sel[start.yr.sel], "--",
                                 est.sel[end.yr.sel], ", ages ", min(ages.estimated), "--", max(ages.estimated), ")"),
                          length(ages.estimated) * length(est.sel[start.yr.sel]:est.sel[end.yr.sel]),
                          "NA",
                          paste0("Normal(0,", est.sel[sel.dev.sd], ")"))

  tab <- rbind(r0.vals, h.vals, sig.r.vals, rec.dev.vals, m.vals, q.vals, se.vals, age.sel.vals, f.age.sel.vals, f.age.sel.dev.vals)

  if(!return.xtable){
    return(tab)
  }

  ## Make first row empty to make the Stock Dynamics header appear below the horizontal line
  tab <- rbind(c("","","",""), tab)

  colnames(tab) <- c("\\textbf{Parameter}",
                     "\\specialcell{\\textbf{Number}\\\\\\textbf{estimated}}",
                     "\\specialcell{\\textbf{Bounds}\\\\\\textbf{(low,high)}}",
                     paste0("\\specialcell{\\textbf{Prior (Mean, SD)}\\\\\\textbf{single value = fixed}}"))

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- 1
  addtorow$pos[[2]] <- 6
  addtorow$pos[[3]] <- 6
  addtorow$pos[[4]] <- 9
  addtorow$command <- c("\\textbf{\\underline{Stock dynamics}} \\\\",
                        "\\\\ \\textbf{\\underline{Catchability and selectivity (double normal)}} \\\\",
                        "\\emph{Acoustic survey} \\\\",
                        "\\emph{Fishery} \\\\")

  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(tab, caption=xcaption, label=xlabel, align=get.align(ncol(tab), just="c")),
               caption.placement = "top", include.rownames=FALSE, sanitize.text.function=function(x){x},
               size=size.string, add.to.row=addtorow, table.placement="H"))

}

make.short.parameter.estimates.sens.table <- function(models,               ## A list of models which contain the MLE output from SS_output
                                                      model.names,          ## A vector of names of the same length as the number of models in the models list
                                                      posterior.regex,      ## a vector of the posterior names to search for (partial names will be matched)
                                                      end.yr,
                                                      digits = 3,           ## number of decimal points for the estimates
                                                      xcaption = "default", ## Caption to use
                                                      xlabel   = "default", ## Latex label to use
                                                      font.size = 9,        ## Size of the font for the table
                                                      space.size = 10){     ## Size of the spaces for the table
  ## Returns an xtable in the proper format for the MLE parameter estimates for all the models,
  ##  one for each column

  tab <- NULL
  for(model in models){
    j <- model$par
    p.names <- rownames(j)
    mle.grep <- unique(grep(paste(posterior.regex, collapse="|"), p.names))
    mle.par <- j[mle.grep,]$Value
    mle.par[2] <- exp(mle.par[2]) / 1000 ## To make R millions

    ## Add 2008 recruitment
    rec <- model$recruit[model$recruit$year == 2008,]$pred_recr
    rec <- rec / 1000
    mle.par <- c(mle.par, rec)

    ## Add 2010 recruitment
    rec <- model$recruit[model$recruit$year == 2010,]$pred_recr
    rec <- rec / 1000
    mle.par <- c(mle.par, rec)

    ## Add 2014 recruitment
    rec <- model$recruit[model$recruit$year == 2014,]$pred_recr
    rec <- rec / 1000
    mle.par <- c(mle.par, rec)

    ## Add B0
    b0 <- model$SBzero
    b0 <- b0 / 1000 ## To make B0 in the thousands
    mle.par <- c(mle.par, b0)

    ## Add depletion for 2009
    d <- 100 * model$derived_quants[paste("Bratio",2009,sep="_"),"Value"]
    mle.par <- c(mle.par, d)

    ## Add depletion for end.yr
    d <- 100 * model$derived_quants[paste("Bratio",end.yr,sep="_"),"Value"]
    mle.par <- c(mle.par, d)

    ## Add fishing intensity for last year
    fi <- model$derived_quants[paste("SPRratio",end.yr-1,sep="_"),"Value"]
    fi <- fi * 100
    mle.par <- c(mle.par, fi)

    ## Add Female spawning biomass B_f40%
    ## Always divide SSB by 2 in single sex model, unless you grab model$SBzero
    ##  divide by 1000 to be consistent with showing biomass in thousands of tons
    b <-  model$derived_quants["SSB_SPRtgt","Value"] / 2 / 1000
    mle.par <- c(mle.par, b)

    ## Add SPR MSY-proxy
    mle.par <- c(mle.par, 40)

    ## Add Exploitation fraction corresponding to SPR
    f <- model$derived_quants["Fstd_SPRtgt","Value"]
    f <- f * 100
    mle.par <- c(mle.par, f)

    ## Add Yield at Bf_40%
    y <- model$derived_quants["TotYield_SPRtgt","Value"] / 1000
    mle.par <- c(mle.par, y)

    if(is.null(tab)){
      tab <- as.data.frame(mle.par)
    }else{
      tab <- cbind(tab, mle.par)
    }
  }

  ## Format the tables rows depending on what they are
  ## Decimal values
  tab[c(1,3,4),] <- f(tab[c(1,3,4),], 3)
  ## Large numbers with no decimal points but probably commas
  tab[c(2,5,6,7,8,12,15),] <- f(apply(tab[c(2,5,6,7,8,12,15),], c(1,2), as.numeric))
  ## Percentages
  tab[c(9,10,11,14),] <- paste0(f(apply(tab[c(9,10,11,14),], c(1,2), as.numeric), 1), "\\%")
  ## SPR Percentages row (some may be NA). This is really ugly but works
  tab[13, !is.na(tab[13,])] <- paste0(f(as.numeric(tab[13, !is.na(tab[13,])]), 1), "\\%")

  ## Make first row empty to make the Parameter header appear below the horizontal line
  tab <- rbind(rep("", length(models)), tab)

  ## Replace NAs with dashes
  tab[is.na(tab)] <- "\\textbf{--}"

  ## Set the first column to be the names
  tab <- cbind(c("",  ## Necessary because of the rbind(rep("", length(models)), tab) call above
                 "Natural mortality (\\emph{M})",
                 "\\emph{R}\\subscr{0} (millions)",
                 "Steepness (\\emph{h})",
                 "Additional acoustic survey SD",
                 "2008 recruitment (millions)",
                 "2010 recruitment (millions)",
                 "2014 recruitment (millions)",
                 "\\emph{B}\\subscr{0} (thousand t)",
                 "2009 relative spawning biomass",
                 paste0(end.yr, " relative spawning biomass"),
                 paste0(end.yr - 1, " rel. fishing intensity: (1-SPR)/(1-SPR\\subscr{40\\%})"),
                 "Female spawning biomass (\\emph{$B_{F_{40_{\\%}}}$}; thousand t)",
                 "SPR\\subscr{MSY-proxy}",
                 "Exploitation fraction corresponding to SPR",
                 "Yield at \\emph{$B_{F_{40_{\\%}}}$} (thousand t)"),
               tab)
  ## Need to split up the headers (model names) by words and let them stack on top of each other
  model.names.str <- paste0("\\specialcell{", gsub(" ", "\\\\\\\\", model.names), "}")
  colnames(tab) <- c("", model.names.str)

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- 1
  addtorow$pos[[2]] <- 5
  addtorow$pos[[3]] <- 11
  addtorow$command <- c("\\textbf{\\underline{Parameters}} \\\\",
                        "\\\\ \\textbf{\\underline{Derived Quantities}} \\\\",
                        "\\\\ \\textbf{\\underline{Reference Points based on $\\Fforty$}} \\\\")

  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(tab, caption=xcaption, label=xlabel, align=get.align(ncol(tab), just="c")),
               caption.placement = "top", include.rownames=FALSE, sanitize.text.function=function(x){x},
               size=size.string, add.to.row=addtorow, table.placement="H"))

}

make.short.parameter.estimates.table <- function(model,                ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                                 last.yr.model,        ## last year's base model for comparison
                                                 posterior.regex,      ## a vector of the posterior names to search for (partial names will be matched)
                                                 end.yr,               ## required for spawning biomass, so you get the right year's value
                                                 digits = 3,           ## number of decimal points for the estimates
                                                 xcaption = "default", ## Caption to use
                                                 xlabel   = "default", ## Latex label to use
                                                 font.size = 9,        ## Size of the font for the table
                                                 space.size = 10       ## Size of the spaces for the table
                                                 ){
  ## Returns an xtable in the proper format for the parameter estimates
  ## For MLE vs median vs median of the last year

  ## This year's model MLE
  j <- model$par
  p.names <- rownames(j)
  mle.grep <- unique(grep(paste(posterior.regex, collapse="|"), p.names))
  ## mle.names <- j[mle.grep,]$Label

  mle.par <- j[mle.grep,]$Value
  mle.par[2] <- exp(mle.par[2]) / 1000 ### To make R millions

  ## Add Q for MLE
  mle.q <- round(model$cpue$Calc_Q[1],3)
  mle.par <- c(mle.par, mle.q)

  ## Add 2008 recruitment
  rec <- model$recruit[model$recruit$year == 2008,]$pred_recr
  rec <- rec / 1000
  mle.par <- c(mle.par, rec)

  ## Add 2010 recruitment
  rec <- model$recruit[model$recruit$year == 2010,]$pred_recr
  rec <- rec / 1000
  mle.par <- c(mle.par, rec)

  ## Add 2014 recruitment
  rec <- model$recruit[model$recruit$year == 2014,]$pred_recr
  rec <- rec / 1000
  mle.par <- c(mle.par, rec)

  ## Add B0
  b0 <- model$SBzero ## Note that this is divided by 2 in a single sex model
  b0 <- b0 / 1000    ## To make B0 in the thousands
  mle.par <- c(mle.par, b0)

  ## Add depletion for 2009
  d <- 100 * model$derived_quants[paste("Bratio",2009,sep="_"),"Value"]
  mle.par <- c(mle.par, d)

  ## Add depletion for end.yr
  d <- 100 * model$derived_quants[paste("Bratio",end.yr,sep="_"),"Value"]
  mle.par <- c(mle.par, d)

  ## Add fishing intensity for last year
  fi <- model$derived_quants[paste("SPRratio",end.yr-1,sep="_"),"Value"]
  fi <- fi * 100.0
  mle.par <- c(mle.par, fi)

  ## Add Female spawning biomass B_f40%
  ## Always divide SSB by 2 in single sex model, unless you grab model$SBzero
  ##  divide by 1000 to be consistent with showing biomass in thousands of tons
  b <- model$derived_quants["SSB_SPRtgt","Value"] / 2 / 1000
  mle.par <- c(mle.par, b)

  ## Add SPR MSY-proxy
  mle.par <- c(mle.par, 40)

  ## Add Exploitation fraction corresponding to SPR
  f <- model$derived_quants["Fstd_SPRtgt","Value"]
  f <- 100 * f #make a percentage
  mle.par <- c(mle.par, f)

  ## Add Yield at Bf_40%
  y <- model$derived_quants["TotYield_SPRtgt","Value"]/1000
  mle.par <- c(mle.par, y)

  calc.mcmc <- function(x,
                        q.choice = 1 ## q == 1 is this year, q == 2 is last year
                        ){
    mcmc.grep <- unique(grep(paste(posterior.regex, collapse="|"), names(x$mcmc)))
    mcmc.names <- names(x$mcmc)[mcmc.grep]
    mcmc.par <- x$mcmc[,mcmc.grep]
    mcmc.meds <- apply(mcmc.par, 2, median)
    mcmc.meds[2] <- exp(mcmc.meds[2]) / 1000 # To make R0 in the millions
    names(mcmc.meds) <- NULL

    ## Add Q - *Warning* - hardwired values for 2016 assessment
    if(q.choice == 1){
      q <- round(median(base.model$extra.mcmc$Q_vector), 3)
    }else{
      q <- round(median(last.yr.base.model$extra.mcmc$Q_vector), 3)
    }
    mcmc.meds <- c(mcmc.meds, q)

    ## Add 2008 recruitment
    rec <- median(x$mcmc$Recr_2008)
    rec <- rec / 1000
    mcmc.meds <- c(mcmc.meds, rec)

    ## Add 2010 recruitment
    rec <- median(x$mcmc$Recr_2010)
    rec <- rec / 1000
    mcmc.meds <- c(mcmc.meds, rec)

    ## Add 2014 recruitment
    rec <- median(x$mcmc$Recr_2014)
    rec <- rec / 1000
    mcmc.meds <- c(mcmc.meds, rec)

    ## Add B0
    b0 <- median(x$mcmc$SPB_Initial / 2) ## divide by 2 for females
    b0 <- b0 / 1000 ## To make B0 in the thousands
    mcmc.meds <- c(mcmc.meds, b0)

    ## Add depletion for 2009
    d <- median(x$mcmc$Bratio_2009)
    d <- d * 100  ## To make a percentage
    mcmc.meds <- c(mcmc.meds, d)

    ## Add depletion for 2015
    d <- median(x$mcmc$Bratio_2016)
    d <- d * 100  ## To make a percentage
    mcmc.meds <- c(mcmc.meds, d)

    ## Add fishing intensity for end.yr - 1
    d <- median(x$mcmc[,paste("SPRratio", end.yr - 1, sep="_")])
    d <- d * 100  ## To make a percentage
    mcmc.meds <- c(mcmc.meds, d)

    ## Add Female spawning biomass B_f40%
    b <- median(x$mcmc[,"SSB_SPRtgt"]) / 2 /1000
    mcmc.meds <- c(mcmc.meds, b)

    ## Add SPR MSY-proxy
    mcmc.meds <- c(mcmc.meds, 40)

    ## Add Exploitation fraction corresponding to SPR
    f <- median(x$mcmc[,"Fstd_SPRtgt"])
    f <- 100 * f
    mcmc.meds <- c(mcmc.meds, f)

    ## Add Yield at Bf_40%
    y <- median(x$mcmc[,"TotYield_SPRtgt"]) / 1000
    mcmc.meds <- c(mcmc.meds, y)
    return(mcmc.meds)
  }

  ## This year's model MCMC
  mcmc.meds <- calc.mcmc(model, q = 1)

  ## Last year's model MCMC
  last.yr.mcmc.meds <- calc.mcmc(last.yr.model, q = 2)
  ## *Warning* THIS IS A HACK TO NOT REPORT THE SPR FOR 2015 FROM LAST ASSESSMENT SINCE ACTUAL CATCH WAS NOT KNOWN
  last.yr.mcmc.meds[11] <- NA
  tab <- as.data.frame(cbind(mle.par, mcmc.meds, last.yr.mcmc.meds))
  colnames(tab) <- NULL

  ## Format the tables rows depending on what they are.
  ## Decimal values
  tab[c(1,3,4,5),] <- f(tab[c(1,3,4,5),], 3)
  ## Large numbers with no decimal points but probably commas
  tab[c(2,6,7,8,9,13,16),] <- f(apply(tab[c(2,6,7,8,9,13,16),], c(1,2), as.numeric))
  ## Percentages on non-NA elements
  paste.perc <- function(vec){
    ## Paste percentages on to all elements of vec that are not NA
    vec[!is.na(vec)] <- paste0(f(as.numeric(vec[!is.na(vec)]), 1), "\\%")
    return(vec)
  }
  tab[10,] <- paste.perc(tab[10,])
  tab[11,] <- paste.perc(tab[11,])
  tab[12,] <- paste.perc(tab[12,])
  tab[15,] <- paste.perc(tab[15,])
  ## SPR Percentages row (some may be NA). This is really ugly but works
  tab[14, !is.na(tab[14,])] <- paste0(f(apply(tab[14, !is.na(tab[14,])], 1, as.numeric), 1), "\\%")

  ## Make first row empty to make the Parameter header appear below the horizontal line
  tab <- rbind(c("","",""), tab)

  ## Replace NAs with dashes
  tab[is.na(tab)] <- "\\textbf{--}"

  ## Set the first column to be the names
  tab <- cbind(c("",  ## Necessary because of the rbind(c("","",""), tab) call above
                 "Natural mortality (\\emph{M})",
                 "Unfished recruitment (\\emph{R}\\subscr{0}, millions)",
                 "Steepness (\\emph{h})",
                 "Additional acoustic survey SD",
                 "Catchability (\\emph{q})",
                 "2008 recruitment  (millions)",
                 "2010 recruitment  (millions)",
                 "2014 recruitment  (millions)",
                 "Unfished female spawning biomass (\\emph{B}\\subscr{0}, thousand~t)",
                 "2009 relative spawning biomass",
                 paste0(end.yr, " relative spawning biomass"),
                 paste0(end.yr - 1, " relative fishing intensity: (1-SPR)/(1-SPR)\\subscr{40\\%})"),
                 "Female spawning biomass at \\emph{F}\\subscr{SPR=40\\%} (\\emph{B}\\subscr{SPR=40\\%}, thousand t)",
                 "SPR at \\emph{F}\\subscr{SPR=40\\%}", # "SPR\\subscr{MSY-proxy}",
                 "Exploitation fraction corresponding to SPR",
                 "Yield at \\emph{B}\\subscr{SPR=40\\%} (thousand~t)"),
               tab)
  colnames(tab) <- c("",
                     "\\textbf{MLE}",
                     "\\specialcell{\\textbf{Posterior}\\\\\\textbf{median}}",
                     paste0("\\specialcell{\\textbf{Posterior}\\\\\\textbf{median from}\\\\\\textbf{",end.yr - 1," base}\\\\\\textbf{model}}"))

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- 1
  addtorow$pos[[2]] <- 6
  addtorow$pos[[3]] <- 14
  addtorow$command <- c("\\textbf{\\underline{Parameters}} \\\\",
                        "\\\\ \\textbf{\\underline{Derived Quantities}} \\\\",
                        "\\\\ \\textbf{\\underline{Reference Points (equilibrium) based on \\emph{F}\\subscr{SPR=40\\%}}} \\\\")
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(tab, caption=xcaption, label=xlabel, align=get.align(ncol(tab), just="c")),
               caption.placement = "top", include.rownames=FALSE, sanitize.text.function=function(x){x},
               size=size.string, add.to.row=addtorow, table.placement="H"))
}

make.long.parameter.estimates.table <- function(model,                ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                                posterior.regex,      ## a vector of the posterior names to search for (partial names will be matched)
                                                digits = 4,           ## number of decimal points for the estimates
                                                xcaption = "default", ## Caption to use
                                                xlabel   = "default", ## Latex label to use
                                                font.size = 9,        ## Size of the font for the table
                                                space.size = 10       ## Size of the spaces for the table
                                                ){
  ## Returns an xtable in the proper format for the posterior medians of the parameter estimates

  mc <- model$mcmc
  mc.names <- names(mc)

  ## Start with the key posteriors using the regex
  mcmc.grep <- unique(grep(paste(posterior.regex, collapse="|"), mc.names))
  mcmc.names <- mc.names[mcmc.grep]
  mcmc.par <- mc[,mcmc.grep]
  mcmc.meds <- as.data.frame(apply(mcmc.par, 2, median))
  df <- cbind(mcmc.names, mcmc.meds)
  names(df) <- c("param","p.med")
  rownames(df) <- NULL

  calc.meds <- function(df, x){
    ## x is a data frame of posteriors for some parameters
    ## This function will take the medians of these,
    ##  and bind them with the data frame df, and return the result
    ## Assumes df has column names param and p.med
    d <- as.data.frame(apply(x, 2, median))
    d <- cbind(rownames(d), d)
    rownames(d) <- NULL
    names(d) <- c("param","p.med")
    df <- rbind(df, d)
    return(df)
  }
  ## Add all Early_InitAge parameters
  ei <- mc[,grep("Early_InitAge_[0-9]+", mc.names)]
  df <- calc.meds(df, ei)

  ## Add all Recruitment deviation parameters
  rec <- mc[,union(grep(".*_RecrDev_[0-9]+", mc.names),
                  grep("ForeRecr_[0-9]+", mc.names))]
  df <- calc.meds(df, rec)

  ## Add all AgeSel
  a.sel <- mc[,grep("AgeSel_.*", mc.names)]
  df <- calc.meds(df, a.sel)

  ## Format the values
  df[,2] <- f(df[,2], digits)

  ## Make the underscores in the names have a preceeding \ so latex will like it
  param.names <- levels(df[,1])[df[,1]]
  df[,1] <- gsub("\\_", "\\\\_", param.names)

  ## Latex column names
  names(df) <- c("\\textbf{Parameter}", "\\textbf{Posterior median}")

  addtorow          <- list()
  addtorow$pos      <- list()
  addtorow$pos[[1]] <- c(0)
  addtorow$command  <- c(paste("\\hline \n",
                               "\\endhead \n",
                               "\\hline \n",
                               "{\\footnotesize Continued on next page} \n",
                               "\\endfoot \n",
                               "\\endlastfoot \n",sep=""))
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(df, caption=xcaption, label=xlabel, align=get.align(ncol(df)), digits=digits),
               caption.placement = "top", table.placement="H", tabular.environment="longtable", floating=FALSE,
               include.rownames=FALSE, sanitize.text.function=function(x){x},
               size=size.string, add.to.row = addtorow, hline.after=c(-1)))
}
