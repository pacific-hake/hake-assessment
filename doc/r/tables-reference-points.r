make.reference.points.table <- function(model,                ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                        xcaption = "default", ## Caption to use
                                        xlabel   = "default", ## Latex label to use
                                        font.size = 9,        ## Size of the font for the table
                                        space.size = 10       ## Size of the spaces for the table
                                        ){
  ## Returns an xtable in the proper format for the executive summary reference points

  unfish.fem.bio <- round(quantile(model$mcmc$SSB_Unfished,prob=c(0.025,0.5,0.975))/2e6,3) * 1000
  unfish.recr <- round(quantile(model$mcmc$Recr_Virgin,prob=c(0.025,0.5,0.975))/1e6,3) * 1000


  f.spawn.bio.bf40 <- round(quantile(model$mcmc$SSB_SPRtgt,prob=c(0.025,0.5,0.975))/2e6,3) * 1000
  exp.frac.spr <- round(100*quantile(model$mcmc$Fstd_SPRtgt,prob=c(0.025,0.5,0.975)),1)
  yield.bf40 <- round(quantile(model$mcmc$TotYield_SPRtgt,prob=c(0.025,0.5,0.975))/1e6,3) * 1000

  fem.spawn.bio.b40 <- round(quantile(model$mcmc$SSB_Btgt,prob=c(0.025,0.5,0.975))/2e6,3) * 1000
  spr.b40 <- round(100*quantile(model$mcmc$SPR_Btgt,prob=c(0.025,0.5,0.975)),1)
  exp.frac.b40 <- round(100*quantile(model$mcmc$Fstd_Btgt,prob=c(0.025,0.5,0.975)),1)
  yield.b40 <- round(quantile(model$mcmc$TotYield_Btgt,prob=c(0.025,0.5,0.975))/1e6,3) * 1000

  fem.spawn.bio.bmsy <- round(quantile(model$mcmc$SSB_MSY,prob=c(0.025,0.5,0.975))/2e6,3) * 1000
  spr.msy <- round(100*quantile(model$mcmc$SPR_MSY,prob=c(0.025,0.5,0.975)),1)
  exp.frac.sprmsy <- round(100*quantile(model$mcmc$Fstd_MSY,prob=c(0.025,0.5,0.975)),1)
  msy <- round(quantile(model$mcmc$TotYield_MSY,prob=c(0.025,0.5,0.975))/1e6,3) * 1000

  tab <- rbind(unfish.fem.bio, unfish.recr,
               f.spawn.bio.bf40, exp.frac.spr, yield.bf40,
               fem.spawn.bio.b40, spr.b40, exp.frac.b40, yield.b40,
               fem.spawn.bio.bmsy, spr.msy, exp.frac.sprmsy, msy)
  descr <- c("Unfished female B (B\\subscr{0}, thousand t)",
             "Unfished recruitment (R\\subscr{0}, millions)",
             "Female spawning biomass (B\\subscr{F40\\%} thousand t)",
             "Exploitation fraction corresponding to SPR",
             "Yield at B\\subscr{F40\\%} (thousand t)",
             "Female spawning biomass (B\\subscr{40\\%}thousand t)",
             "SPR\\subscr{B40\\%}",
             "Exploitation fraction resulting in B\\subscr{40\\%}",
             "Yield at B\\subscr{40\\%} (thousand t)",
             "Female spawning biomass (B\\subscr{MSY} thousand t)",
             "SPR\\subscr{MSY}",
             "Exploitation fraction corresponding to SPR\\subscr{MSY}",
             "MSY (thousand t)")
  tab <- cbind(descr, tab)

  colnames(tab) <- c("Quantity",
                     "\\specialcell{\\textbf{2.5\\supscr{th}}\\\\\\textbf{percentile}}",
                     "\\specialcell{\\textbf{Median}}",
                     "\\specialcell{\\textbf{97.5\\supscr{th}}\\\\\\textbf{percentile}}")
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- 2
  addtorow$pos[[2]] <- 7
  addtorow$pos[[3]] <- 11
  addtorow$command <- c("Reference points (equilibrium) based on F\\subscr{40\\%} \\\\",
                        "Reference points (equilibrium) based on B\\subscr{40\\%} \\\\",
                        "Reference points (equilibrium) based on estimated MSY \\\\")
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(tab, caption=xcaption, label=xlabel, align=get.align(ncol(tab))),
               caption.placement = "top", include.rownames=FALSE, sanitize.text.function=function(x){x},
               size=size.string, add.to.row=addtorow))
}
