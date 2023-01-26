plot_spr_illustration <- function(model,
                                  yrs = NULL,
                                  show.fished = TRUE,
                                  show.legend = TRUE){
  # graphic for appendix to illustrate how SPR is calculated
  col1 <- 'grey20'
  col2 <- 'grey70'

  # ages to show in the plot
  ages <- 0:20
  xlim <- c(0,20)
  x.legend <- 6 # x-value associated with left side of legend
  x.legend2 <- 12 # x-value associated with left side of legend

  cols <- as.character(ages)

  if(is.null(yrs)){
    yrs <- unique(model$wtatage$Yr)
  }

  # average maturity * fecundity
  matfec.vec <- model$wtatage %>%
    filter(Fleet == -2,
           Yr %in% yrs) %>%
    select(cols) %>%
    summarize_all(mean)
  # average weight at age
  wt.vec.avg <- model$wtatage %>%
    filter(Fleet == -1,
           Yr %in% yrs) %>%
    select(cols) %>%
    summarize_all(mean)

  # numbers at age in equilibrium
  N_at_age.equil <- as.numeric(model$natage[model$natage$Era=="VIRG" &
                                              model$natage$"Beg/Mid"=="B",
                                            cols])

  # M and Z at age for final year
  M_at_age.endyr <- as.numeric(model$Natural_Mortality[model$Natural_Mortality$Yr == model$endyr,
                                              cols])
  F_at_age.endyr <- as.numeric(model$fatage[model$fatage$Yr == model$endyr,
                                              cols])

  Z_at_age.endyr <- F_at_age.endyr + M_at_age.endyr

  # calculate numbers at age with and without fishing based on 1 at age 0
  N_at_age.M <- rep(1, 21)
  N_at_age.Z <- rep(1, 21)
  for(iage in 1:20){
    N_at_age.M[iage+1] <- N_at_age.M[iage]*exp(-M_at_age.endyr[iage])
    N_at_age.Z[iage+1] <- N_at_age.Z[iage]*exp(-Z_at_age.endyr[iage])
  }

  # set up 3-panel plot and adjust margins
  par(mfrow = c(3, 1),
      mar = c(1, 4, 3, 1),
      oma = c(3, 0, 0, 0),
      cex = 1)

  # make plot of numbers per recruit
  plot(0,
       type = 'n',
       xlim = xlim,
       ylim = c(0, 1.1),
       xaxs = 'r',
       yaxs  ='i',
       xlab = "",
       ylab = "",
       axes = FALSE)
  points(ages,
         N_at_age.M,
         type = 'h',
         lwd = 30,
         lend = 1,
         col = col2)
  if(show.fished){
    points(ages,
           N_at_age.Z,
           type = 'h',
           lwd = 30,
           lend = 1,
           col = col1)
  }
  axis(1, at = -1:22)
  axis(2, las = 1)
  mtext("Numbers per recruit",
        font = 2,
        cex = 1,
        at = 0,
        adj = 0)
  if(show.legend){
    legend(x = x.legend,
           y = 1,
           fill = c(col2, col1),
           border = NA,
           legend = c("No fishing","With fishing"),
           bty = 'n')
  }

  # make plot of biomass per recruit
  plot(0,
       type='n',
       xlim = xlim,
       ylim = c(0, 0.215),
       xaxs = 'r',
       yaxs = 'i',
       xlab = "",
       ylab = "",
       axes = FALSE)
  points(ages,
         N_at_age.M * wt.vec.avg,
         type = 'h',
         lwd = 30,
         lend = 1,
         col = col2)
  if(show.fished){
    points(ages,
           N_at_age.Z * wt.vec.avg,
           type = 'h',
           lwd = 30,
           lend = 1,
           col = col1)
  }
  axis(1, at = -1:22)
  axis(2, las = 1)
  mtext("Biomass per recruit",
        font = 2,
        cex = 1,
        at = 0,
        adj = 0)

  # make plot of female spawning biomass per recruit
  plot(0,
       type = 'n',
       xlim = xlim,
       ylim = c(0, 0.12),
       xaxs = 'r',
       yaxs = 'i',
       xlab = "",
       ylab = "",
       axes = FALSE)
  points(ages,
         0.5 * N_at_age.M * matfec.vec,
         type = 'h',
         lwd = 30,
         lend = 1,
         col = col2)
  if(show.fished){
    points(ages,
           0.5 * N_at_age.Z * matfec.vec,
           type = 'h',
           lwd = 30,
           lend = 1,
           col = col1)
  }
  axis(1, at = -1:22)
  axis(2, at = pretty(c(0, 0.1)), las = 1)
  mtext("Female spawning biomass per recruit",
        font = 2,
        cex = 1,
        at = 0,
        adj = 0)

  # add label for x-axis (applied to all plots)
  mtext("Age",
        side = 1,
        outer = TRUE,
        line = 1)

  # calculate spawning potential with and without fishing and SPR
  spawn.potential.M <- sum(0.5 * N_at_age.M * matfec.vec)
  spawn.potential.Z <- sum(0.5 * N_at_age.Z * matfec.vec)
  sum.M <- sum(spawn.potential.M)
  sum.Z <- sum(spawn.potential.Z)
  SPR <- sum(spawn.potential.Z) / sum(spawn.potential.M)

  # format numbers for legend
  sum.M.txt <- format(sum.M,
                      digits = 2,
                      nsmall = 2)
  sum.Z.txt <- format(sum.Z,
                      digits = 2,
                      nsmall = 2)
  SPR.txt <- format(SPR,
                    digits = 2,
                    nsmall = 2)
  intensity.txt <- format((1 - SPR) / (1 - 0.4),
                          digits = 2,
                          nsmall = 2)

  if(show.legend){
    legend(x = x.legend,
           y = 1.05 * par()$usr[4],
           fill = c(col2, col1, NA),
           border = NA,
           legend = c(paste0("Total = ",
                             sum.M.txt),
                      paste0("Total = ",
                             sum.Z.txt)),
           bty = 'n')
    text(x = x.legend2,
         y = 0.90 * par()$usr[4],
         paste0("SPR = ",
                sum.Z.txt,
                "/",
                sum.M.txt,
                " = ",
                SPR.txt),
         pos = 4)
    text(x = x.legend2,
         y = 0.55 * par()$usr[4],
         paste0("Rel. Fishing Intensity =\n",
                "     (1 - SPR)/(1 - 0.40) =\n",
                "          ",
                intensity.txt),
         pos = 4)
  }
}
