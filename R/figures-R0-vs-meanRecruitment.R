make.R0.vs.meanRecruitment.fig <- function(plot=TRUE){
  # figure to compare R0 to mean recruitment
  # not at all generalized, just making a figure for 2018 SRG requests presentation
  
  # look at mean recruitment vs. R0
  getR0 <- function(model){
    # R0 in units of billions of fish
    round(exp(model$parameters["SR_LN(R0)","Value"])/1e6, 3)
  }
  getMeanRecr <- function(model){
    # Mean recruitment over the years 1966 to 2017 (or final data year)
    round(mean(model$timeseries$Recruit_0[model$timeseries$Era=="TIME"])/1e6, 3)
  }

  mod.sig1.0 <- sens.models.1[[which(sens_model_names_1=="Sigma R 1.0")]]
  mod.sig1.8 <- sens.models.1[[which(sens_model_names_1=="Sigma R 1.8")]]
      
  # make table
  sigma.table <- data.frame(sigmaR = c(1.0, 1.4, 1.8), R0 = NA, MeanRecr1966to2017=NA)

  # add R0 values
  sigma.table$R0[sigma.table$sigmaR==1.0] <- getR0(mod.sig1.0)
  sigma.table$R0[sigma.table$sigmaR==1.4] <- getR0(base_model)
  sigma.table$R0[sigma.table$sigmaR==1.8] <- getR0(mod.sig1.8)

  # add Mean Recruitments
  sigma.table$MeanRecr1966to2017[sigma.table$sigmaR==1.0] <- getMeanRecr(mod.sig1.0)
  sigma.table$MeanRecr1966to2017[sigma.table$sigmaR==1.4] <- getMeanRecr(base_model)
  sigma.table$MeanRecr1966to2017[sigma.table$sigmaR==1.8] <- getMeanRecr(mod.sig1.8)

  # make plot
  if(plot){
    par(mar=c(4,4,.1,.1))
    plot(sigma.table$sigmaR, sigma.table$R0,
         type='o', cex=2, pch=16, lwd=3, col=4, ylim=c(0,4), yaxs='i', las=1,
         xlab="SigmaR", ylab="Number of recruits (billions)", axes=FALSE)
    axis(1, at=c(1,1.4,1.8), labels=c("1.0","1.4 (base)","1.8"))
    axis(2, las=1)
    box()
    lines(sigma.table$sigmaR, sigma.table$MeanRecr1966to2017,
          type='o', cex=2, pch=16, lwd=3, col=2)
    text(labels="Mean recruitment (1966-2017)", 1, 2.5, col=2, adj=0)
    text(labels="Equilibrium recruitment (R0)", 1, 1.4, col=4, adj=0)
  }

  # return table of values
  return(invisible(sigma.table))
}
