
make.mcmc.sr.variability.plot <- function(model,
                                          start_yr,
                                          end_yr,
                                          xlim = c(0, 1.5),
                                          ylim = c(0, 7),
                                          probs = c(0.025, 0.5, 0.975)){
  oldpar <- par()
  on.exit(par(oldpar))

  yrs <- start_yr:end_yr

  beverton.holt <- function(h, b = seq(0, 1.5, 0.005)){
    (4.0 * h * b) / ((1.0 - h) + (5.0 * h - 1.0) * b)
  }

  r.mat <- NULL
  for(iter in model$mcmc$Iter){
    b <- seq(0, 1.5 , 0.005)
    h <- model$mcmc$SR_BH_steep[model$mcmc$Iter == iter]
    r <- beverton.holt(h, b)
    r.mat <- cbind(r.mat, r)
  }
  r.quants <- apply(r.mat, 1, quantile, probs = probs)
  adj <- exp(-0.5 * model$sigma_R_in ^ 2) # bias adjustment

  layout(mat = matrix(2:1, nrow = 1), widths = c(5, 1))

  # Get quantities of interest and calculate lognormal distribution
  # over a range of values
  ymax <- 7
  r.vec <- seq(0, ymax, length=1000)
  sigma_R <- model$sigma_R_in
  meanlog <- (sigma_R ^ 2) / 2
  dlnorm.vec <- dlnorm(x = r.vec, meanlog = meanlog , sdlog = sigma_R)

  ## Note: to check that this parameterization seems right,
  ## commands like the following were used
  #  sigma_R <- 1.4
  #  meanlog <- (-sigma_R ^ 2) / 2
  #  z <- rlnorm(n = 1e6, meanlog = meanlog, sdlog = sigma_R)
  #  mean(z); median(z); sd(log(z))
  #  [1] 1.001241  # should be close to 1
  #  [1] 0.3750798 # should be close to 0.38
  #  [1] 1.400605  # should be close to 1.4

  # Density plot on right-hand side
  par(mar = c(4, 1.5, 1, 1) + 0.1)
  plot(0,
       type = 'n',
       xlim = c(0, max(dlnorm.vec)),
       ylim = c(0, ymax),
       xaxs = 'i',
       yaxs = 'i',
       axes = FALSE,
       xlab = "",
       ylab = "")
  polygon(y = c(0, r.vec, max(r.vec)),
          x = c(0, dlnorm.vec, 0),
          border = NA,
          col = rgb(0, 0, 0, 0.2))

  # horizontal lines for mean and median
  lines(c(0, dlnorm(x = 1, meanlog = meanlog, sdlog = sigma_R)),
        y = c(1, 1), lty = 1, col = 1)
  lines(c(0, dlnorm(x = adj, meanlog = meanlog, sdlog = sigma_R)),
        y = c(adj, adj), lty = 1, col = 2)
  axis(2, at = c(0, adj, 1, nrow(model$mcmc)), lab = rep("", 4), las = 1)
  mtext("Expected distribution of absolute recruitments", side = 4, line = -2, adj = 0.8)

  # Main plot
  par(mar = c(4, 4, 1, 1) + 0.1, mgp = c(2.3, 1, 0))

  plot(0,
       type = "n",
       xlim = xlim,
       ylim = ylim,
       xaxs = "i",
       yaxs = "i",
       axes = FALSE,
       xlab = "Relative spawning biomass",
       ylab = expression(paste("Recruitment relative to unfished equilibrium", ~~(italic(R)[0]))))
  axis(1, at = seq(0, 2, 0.2))
  axis(2, at = seq(0, par()$usr[4], 1), las = 1)
  axis(2, at = adj, label = round(adj, 2), cex.axis = 0.8, las = 1)
  abline(h = 1, v = 1, lty = 2, col = 1)
  abline(h = adj, lty = 2, col = 2)

  addpoly(b, r.quants[1,], r.quants[3,])
  addpoly(b, adj * r.quants[1,], adj * r.quants[3,], shade.col = rgb(1, 0, 0, 0.1), color = 2)
  lines(b, r.quants[2,], lwd = 3)
  lines(b, adj * r.quants[2,], lwd = 3, col = 2)

  r.ratio <- model$mcmc[names(model$mcmc) %in% c("Recr_Virgin", paste0("Recr_", yrs))]
  mn.r.virg <- median(r.ratio[, 1])
  b.ratio <- model$mcmc[names(model$mcmc) %in% c("SSB_Virgin", paste0("SSB_", yrs))]

  # standardize relative to equilibrium
  b.ratio <- b.ratio / b.ratio[, 1]
  r.ratio <- r.ratio / r.ratio[, 1]
  colvec <- rev(rich.colors.short(length(yrs) + 10, alpha = 0.8))[-(1:10)]

  for(iyr in 1:length(yrs)){
    y <- yrs[iyr]
    Bs <- b.ratio[,iyr + 1] # + 1 to account for equilibrium in first column
    Rs <- r.ratio[,iyr + 1]
    Bq <- quantile(Bs, prob = probs)
    Rq <- quantile(Rs, prob = probs)
    arrows(Bq[2], Rq[1], Bq[2], Rq[3], col = colvec[iyr], code = 3, angle = 90, length = 0.02)
    arrows(Bq[1], Rq[2], Bq[3], Rq[2], col = colvec[iyr], code = 3, angle = 90, length = 0.02)
    points(Bq[2], Rq[2], pch = 21, bg = colvec[iyr], col = 1)
    if(Rq[2] > 1.5 | y > 2007) text(y, x = Bq[2], y = Rq[2], cex = 0.5, adj = c(-.3,-.3))
  }
  box()
  text(1.01, 0.15 + 1, "Mean recruitment", cex = 0.8, pos = 4)
  text(1.01, 0.15 + 1 * adj,"Median recruitment", col = 2, cex = 0.8, pos = 4)
  points(1, 1, pch = 16, cex = 2)
  points(1, adj, pch = 16, cex = 2, col = 2)

  mtext("Recruitment (billions)", side = 4, line = 1, adj = 0.25)
  axis(4, at = 1, label = round(mn.r.virg / 1e6, 1), las = 1, cex.axis = 0.8)
  axis(4, at = adj, label = round(adj * mn.r.virg / 1e6, 1), las = 1, cex.axis = 0.8)
  axis(4, at = 0, lab = " 0", las = 1, cex.axis = 0.8)
}
