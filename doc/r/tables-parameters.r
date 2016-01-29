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

  calc.ro <- function(x){
    ## Changes from logspace and multiplies by two because it's female only
    return(exp(x) * 2)
  }

  ## This year's model MLE
  j <- model$par
  p.names <- rownames(j)
  mle.grep <- unique(grep(paste(posterior.regex, collapse="|"), p.names))
  ## mle.names <- j[mle.grep,]$Label
  mle.par <- j[mle.grep,]$Value
  mle.par[2] <- mle.par[2] * 1000 ### To make R millions

  ## Add 2008 recruitment
  rec <- model$recruit[model$recruit$year == 2008,]$pred_recr
  rec <- rec / 1000 ## To make R in the millions
  mle.par <- c(mle.par, rec)

  ## Add 2010 recruitment
  rec <- model$recruit[model$recruit$year == 2010,]$pred_recr
  rec <- rec / 1000 ## To make R in the millions
  mle.par <- c(mle.par, rec)

  ## Add B0
  b0 <- model$SBzero
  b0 <- b0 / 1000 ## To make B0 in the thousands
  mle.par <- c(mle.par, b0)

  ## Add depletion for 2009
  d <- model$timeseries[model$timeseries$Yr == 2009,]$SpawnBio
  d <- d / (b0 * 1000)
  d <- d * 100  ## To make a percentage
  mle.par <- c(mle.par, d)

  ## Add depletion for end.yr
  d <- model$timeseries[model$timeseries$Yr == end.yr,]$SpawnBio
  d <- d / (b0 * 1000)
  d <- d * 100  ## To make a percentage
  mle.par <- c(mle.par, d)

  ## Add fishing intensity for end.yr
  spr <- model$sprseries[model$sprseries$Year == end.yr - 1,]$SPR
  spr40 <- model$sprseries[model$sprseries$Year == end.yr - 1,]$SPR_std
  fi <- (1 - spr) / (1 - spr40)
  mle.par <- c(mle.par, fi)

  ## Add Female spawning biomass B_f40%
  ## b <- models[[1]]$timeseries[model$timeseries$Yr == end.yr,]
  mle.par <- c(mle.par, NA)

  ## Add SPR MSY-proxy
  mle.par <- c(mle.par, 40)

  ## Add Exploitation fraction corresponding to SPR
  mle.par <- c(mle.par, NA)

  ## Add Yield at Bf_40%
  mle.par <- c(mle.par, NA)

  calc.mcmc <- function(x){
    mcmc.grep <- unique(grep(paste(posterior.regex, collapse="|"), names(x$mcmc)))
    mcmc.names <- names(x$mcmc)[mcmc.grep]
    mcmc.par <- x$mcmc[,mcmc.grep]
    mcmc.meds <- apply(mcmc.par, 2, median)
    mcmc.meds[2] <- mcmc.meds[2] * 1000 # To make R0 in the millions
    names(mcmc.meds) <- NULL

    ## Add 2008 recruitment
    rec <- median(x$mcmc$Recr_2008)
    rec <- rec / 1000 ## To make R in the millions
    mcmc.meds <- c(mcmc.meds, rec)

    ## Add 2010 recruitment
    rec <- median(x$mcmc$Recr_2010)
    rec <- rec / 1000 ## To make R in the millions
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
    d <- median(x$mcmc$Bratio_2015)
    d <- d * 100  ## To make a percentage
    mcmc.meds <- c(mcmc.meds, d)

    ## Add fishing intensity for end.yr - 1
    d <- median(eval(parse(text = paste0("models[[1]]$mcmc$SPRratio_", end.yr - 1))))
    d <- d * 100  ## To make a percentage
    mcmc.meds <- c(mcmc.meds, d)

    ## Add Female spawning biomass B_f40%
    mcmc.meds <- c(mcmc.meds, NA)

    ## Add SPR MSY-proxy
    mcmc.meds <- c(mcmc.meds, NA)

    ## Add Exploitation fraction corresponding to SPR
    mcmc.meds <- c(mcmc.meds, NA)

    ## Add Yield at Bf_40%
    mcmc.meds <- c(mcmc.meds, NA)
    return(mcmc.meds)
  }

  ## This year's model MCMC
  mcmc.meds <- calc.mcmc(model)

  ## Last year's model MCMC
  last.yr.mcmc.meds <- calc.mcmc(last.yr.model)
  tab <- as.data.frame(cbind(mle.par, mcmc.meds, last.yr.mcmc.meds))
  colnames(tab) <- NULL

  ## Format the tables rows depending on what they are
  ## Decimal values
  tab[c(1,3,4),] <- fmt0(tab[c(1,3,4),], 3)
  ## Large numbers with no decimal points but probably commas
  tab[c(2,5,6,7),] <- fmt0(apply(tab[c(2,5,6,7),], 1, as.numeric))
  ## Percentages
  tab[c(8,9,10),] <- paste0(fmt0(apply(tab[c(8,9,10),], 1, as.numeric), 1), "\\%")
  ## SPR Percentages row (some may be NA). This is really ugly but works
  tab[12, !is.na(tab[12,])] <- paste0(fmt0(apply(tab[12, !is.na(tab[12,])], 1, as.numeric), 1), "\\%")

  ## Replace NAs with dashes
  tab[is.na(tab)] <- "--"

  ## Set the first column to be the names
  tab <- cbind(c("Natural Mortality (\\emph{M})",
                 "\\emph{R}\\subscr{0} (millions)",
                 "Steepness (\\emph{h})",
                 "Additional acoustic survey SD",
                 "2008 recruitment",
                 "2010 recruitment",
                 "\\emph{B}\\subscr{0} (thousand t)",
                 "2009 Relative Spawning Biomass",
                 paste0(end.yr, " Relative Spawning Biomass"),
                 paste0(end.yr - 1, " Fishing intensity: (1-SPR)/(1-SPR\\subscr{40\\%})"),
                 "Female Spawning Biomass (\\emph{$B_{F_{40_{\\%}}}$})",
                 "SPR\\subscr{MSY-proxy}",
                 "Exploitation Fraction corresponding to SPR",
                 "Yield at \\emph{$B_{F_{40_{\\%}}}$} (thousand t)"),
               tab)
  colnames(tab) <- c("",
                     "\\textbf{MLE}",
                     "\\specialcell{\\textbf{Posterior}\\\\\\textbf{median}}",
                     paste0("\\specialcell{\\textbf{Posterior}\\\\\\textbf{median from}\\\\\\textbf{",end.yr - 1," base}\\\\\\textbf{model}}"))

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- 0
  addtorow$pos[[2]] <- 4
  addtorow$pos[[3]] <- 10
  addtorow$command <- c("\\hline \\\\ \\textbf{\\underline{Parameters}} \\\\",
                        "\\hline \\\\ \\textbf{\\underline{Derived Quantities}} \\\\",
                        "\\hline \\\\ \\textbf{\\underline{Reference Points based on \\emph{F}\\subscr{40\\%}}} \\\\")

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
  df[,2] <- fmt0(df[,2], digits)

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
