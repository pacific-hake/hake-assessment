maturity.ogive.figure <- function(model, useyears = 1975:(assess.yr - 1)){

  # maturity.samples is created by data-tables.r
  # which reads
  # maturity.samples.file <- "hake-maturity-data.csv"
  mat1 <- maturity.samples
  Amax <- 15 # plus group used in calculations below


  # subset for North and South of Point Conception (34.44 degrees N)
  mat1.N <- mat1[mat1$N_or_S_of_34.44 == "N",]
  mat1.S <- mat1[mat1$N_or_S_of_34.44 == "S",]

  # vector of ages and another to store maturity values
  # (starting at 0 as required by wtatage although no ovary samples for age 0 fish)
  age.vec <- 0:20
  mat.N.vec <- NA * age.vec
  mat.S.vec <- NA * age.vec
  nsamp.N.vec <- NA * age.vec
  nsamp.S.vec <- NA * age.vec

  # loop over ages
  for(a in 1:Amax){
    # subset for a given age
    mat1.N.age <- mat1.N[!is.na(mat1.N$Age) & mat1.N$Age == a,]
    mat1.S.age <- mat1.S[!is.na(mat1.S$Age) & mat1.S$Age == a,]
    # subset plus group for age Amax
    if(a==Amax){
      mat1.N.age <- mat1.N[!is.na(mat1.N$Age) & mat1.N$Age >= a,]
      mat1.S.age <- mat1.S[!is.na(mat1.S$Age) & mat1.S$Age >= a,]
    }

    # sample size
    nsamp.N <- nrow(mat1.N.age)
    nsamp.S <- nrow(mat1.S.age)

    # calculate y-values (fraction mature)
    y.N <- mean(mat1.N.age$Functional_maturity, na.rm=TRUE)
    y.S <- mean(mat1.S.age$Functional_maturity, na.rm=TRUE)

    # store maturities
    mat.N.vec[age.vec == a] <- y.N
    mat.S.vec[age.vec == a] <- y.S
    nsamp.N.vec[age.vec == a] <- nsamp.N
    nsamp.S.vec[age.vec == a] <- nsamp.S
    # apply plus-group values to all ages above Amax
    if(a==Amax){
      mat.N.vec[age.vec >= a] <- y.N
      nsamp.N.vec[age.vec >= a] <- nsamp.N
    }
  }

  nsamp.N.vec2 <- rep(0, 21) # vector similar to nsamp.N.vec but going to 21 instead of 15
  for(a in 0:20){
    nsamp.N.vec2[a + 1] <- sum(!is.na(mat1.N$Age) & mat1.N$Age == a)
  }

  avg.wt <- apply(model$wtatage[model$wtatage$Fleet == 1 &
                                model$wtatage$Yr %in% useyears,
                                grep("^\\d", colnames(model$wtatage))],
                  2,
                  mean)
  fec.vec.new <- apply(model$wtatage[model$wtatage$Fleet == -2 &
                                     model$wtatage$Yr %in% useyears,
                                     grep("^\\d", colnames(model$wtatage))],
                       2,
                       mean)

  # define colors
  col.N <- rgb(0.3, 0.3, 1, 0.8)
  col.S <- rgb(1, 0, 0, 0.5)

  # create empty plot with axes and grid lines
  par(mar = c(2, 4, 1, 1), mfrow = c(2, 1), oma = c(2, 0, 0, 0))
  plot(0, xlim=c(1, 20),
       ylim = c(0, 1.1),
       type = "n",
       xlab = "",
       ylab = "Fraction mature",
       axes = FALSE)
  lines(0:20, mat.N.vec, lwd = 2, col = col.N)
  lines(0:20, mat.S.vec, lwd = 2, col = col.S)
  axis(1, at = 1:14)
  axis(1, at = 15, label = "15+")
  axis(2, las = 1)
  abline(h = seq(0, 1, 0.2), col = "grey")

  # loop over ages
  for(a in 1:Amax){
    # add points to plot
    nsamp.N <- nsamp.N.vec[age.vec == a]
    nsamp.S <- nsamp.S.vec[age.vec == a]
    y.N <- mat.N.vec[age.vec == a]
    y.S <- mat.S.vec[age.vec == a]

    points(a, y.N, cex = 0.3 * sqrt(nsamp.N), col = 1, bg = col.N, pch = 21)
    points(a, y.S, cex = 0.3 * sqrt(nsamp.S), col = 1, bg = col.S, pch = 21)
    text(a, y.N, cex = 0.8, labels = nsamp.N, pos = if(nsamp.N < 60) 1 else NULL)
    text(a, y.S, cex = 0.8, labels = nsamp.S, pos = if (nsamp.S < 60) 3 else NULL,
         col = rgb(1, 0, 0, 0.5))
  }

  # add legend
  legend("bottomright", legend=c("South of 34.44\u00B0", "North of 34.44\u00B0"),
         #title="Size/number indicates sample size")
         bg = "white", box.col = "grey", col = 1, pt.bg = c(col.S, col.N), pch = 21, pt.cex = 2)
  box()

  # second plot
  plot(0, type = "l", lwd = 3, xlim = c(1, 20),
       ylim = c(0, max(c(avg.wt, fec.vec.new)) * 1.05), #yaxs='i',
       xlab = "", ylab = "Weight or fecundity (kg)", axes = FALSE)
  axis(1, at = 1:20)
  axis(2, las = 1)
  abline(h = seq(0, 1, 0.2), col = "grey")
  lines(1:20, avg.wt[-1], lwd = 2, col = 3)
  lines(1:20, fec.vec.new[-1], lwd = 4, col = rgb(0.8, 0, 0.8, 0.8))
  legend("bottomright", col = c(3, rgb(.8,0,0.8),1), lwd = c(2, 4, 4),
         bg = "white", box.col = "grey",
         legend = c("Mean weight at age",
                  paste0("Mean fecundity (maturity at age x weight at age)")))
  box()
  mtext(side = 1, line = 0, outer = TRUE, "Age")
}
