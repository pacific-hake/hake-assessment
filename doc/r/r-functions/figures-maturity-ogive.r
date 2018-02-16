maturity.ogive.figure <- function(){

  # maturity.samples is created by data-tables.r
  # which reads
  # maturity.samples.file <- "hake-maturity-data.csv"
  mat1 <- maturity.samples
  
  Amax <- 15 # plus group used in calculations below


  # subset for North and South of Point Conception (34.44 degrees N)
  mat1.N <- mat1[mat1$N_or_S_of_34.44 == "N",]
  mat1.S <- mat1[mat1$N_or_S_of_34.44 == "S",]
  ## mat1.N <- mat1[mat1$Latitude >= 34.44,]
  ## mat1.S <- mat1[mat1$Latitude < 34.44,]

  # vector of ages and another to store maturity values
  # (starting at 0 as required by wtatage although no ovary samples for age 0 fish)
  age.vec <- 0:20
  mat.N.vec <- NA*age.vec
  mat.S.vec <- NA*age.vec
  nsamp.N.vec <- NA*age.vec
  nsamp.S.vec <- NA*age.vec

  # loop over ages
  for(a in 1:Amax){
    # subset for a given age
    mat1.N.age <- mat1.N[!is.na(mat1.N$Age) & mat1.N$Age==a,]
    mat1.S.age <- mat1.S[!is.na(mat1.S$Age) & mat1.S$Age==a,]
    # subset plus group for age Amax
    if(a==Amax){
      mat1.N.age <- mat1.N[!is.na(mat1.N$Age) & mat1.N$Age >= a,]
      mat1.S.age <- mat1.S[!is.na(mat1.S$Age) & mat1.S$Age >= a,]
    }

    # sample size
    nsamp.N <- nrow(mat1.N.age)
    nsamp.S <- nrow(mat1.S.age)

    # calculate y-values (fraction mature)
    ## y.N <- mean(mat1.N.age$Biological_maturity, na.rm=TRUE)
    ## y.S <- mean(mat1.S.age$Biological_maturity, na.rm=TRUE)
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

  nsamp.N.vec2 <- rep(0, 21) # vector similar to nsamp.N.vec but going to 20 intead of 15 
  for(a in 0:20){
    nsamp.N.vec2[a+1] <- sum(!is.na(mat1.N$Age) & mat1.N$Age==a)
  }

  # values from wtatage.ss file for fleet = -1 for year = 1966
  if(FALSE){
    avg.wt <- as.numeric(base.model$wtatage[base.model$wtatage$Yr==1966 &
                                              base.model$wtatage$Fleet==-1,
                                            paste(0:20)])
    # compute matrix of fecundity based on table of weights
    # start in 1974 as that is the last year of the average weight at age
    wtatage.lines <- base.model$wtatage[base.model$wtatage$Fleet==-1 &
                                          base.model$wtatage$Yr >= 1974, ]
    wtatage.lines.new <- wtatage.lines
    for(irow in 1:nrow(wtatage.lines.new)){
      wtatage.lines.new[irow, paste(0:20)] <-
        round(mat.N.vec*wtatage.lines.new[irow, paste(0:20)], 4)
    }
    # change to fleet -2 = maturity*fecundity
    wtatage.lines.new$Fleet <- -2
    # change average to represent early year (where negative value makes it
    # apply to all subsequent years that aren't overriden with new values)
    wtatage.lines.new$Yr[wtatage.lines.new$Yr==1974] <- -1940
    # add comments
    wtatage.lines.new$comment[wtatage.lines.new$Yr==-1940] <- "#maturity * avg. wt."
    wtatage.lines.new$comment[wtatage.lines.new$Yr>=1975] <- "#maturity * annual wt."
  }

  # fecundity * maturity
  # values from top of wtatage.ss file for fleet = -2
  # origin was 2011 model that included growth
  fec.vec.old <- c(0.0000,0.0000,0.1003,0.2535,0.3992,
                   0.5180,0.6131,0.6895,0.7511,0.8007,
                   0.8406,0.8724,0.8979,0.9181,0.9342,
                   0.9469,0.9569,0.9649,0.9711,0.9761,0.9830)
  # wtatage averaged across years
  
  # values from 2018 model that aren't dependent on having model loaded
  avg.wt <- c(0.0169,0.0916,0.2489,0.3790,0.4841,0.5329,
              0.5813,0.6471,0.7184,0.7875,0.8594,0.9307,
              0.9695,1.0658,1.0091,1.0336,1.0336,1.0336,
              1.0336,1.0336,1.0336)
  fec.vec.new <- avg.wt * mat.N.vec

  ### write stuff to CSV file if needed
  csv <- FALSE
  csv.info <- data.frame(age=0:20,
                         N.ovaries = nsamp.N.vec2,
                         old.fecundity=round(fec.vec.old, 3),
                         new.fecundity=round(fec.vec.new, 3),
                         maturity = round(mat.N.vec, 3),
                         avg.wt = round(avg.wt, 3))
  if(csv){                       
    write.csv(csv.info,
              file.path(dir, 'maturity-table.csv'),
              row.names=FALSE)
  }

  # define colors
  col.N <- rgb(.3,.3,1,.8)
  col.S <- rgb(1,0,0,.5)
  
  # create empty plot with axes and grid lines
  par(mar=c(2,4,1,1), mfrow=c(2,1), oma=c(2,0,0,0))
  plot(0, xlim=c(1,20), ylim=c(0,1.1), type='n', xlab="", ylab="Fraction mature",
       axes=FALSE)
  lines(0:20, mat.N.vec, lwd=2, col=col.N)
  lines(0:20, mat.S.vec, lwd=2, col=col.S)
  axis(1, at=1:14)
  axis(1, at=15, label="15+")
  axis(2, las=1)
  abline(h=seq(0,1,.2), col='grey')

  # loop over ages
  for(a in 1:Amax){
    # add points to plot
    nsamp.N <- nsamp.N.vec[age.vec == a]
    nsamp.S <- nsamp.S.vec[age.vec == a]
    y.N <- mat.N.vec[age.vec == a]
    y.S <- mat.S.vec[age.vec == a]
    
    points(a, y.N, cex=0.3*sqrt(nsamp.N), col=1, bg=col.N, pch=21)
    points(a, y.S, cex=0.3*sqrt(nsamp.S), col=1, bg=col.S, pch=21)
    text(a, y.N, cex=.8, labels=nsamp.N, pos=if (nsamp.N < 60) 1 else NULL)
    text(a, y.S, cex=.8, labels=nsamp.S, pos=if (nsamp.S < 60) 3 else NULL,
         col=rgb(1,0,0,.5))
  }  

  # add legend
  legend('bottomright', legend=c("South of 34.44°", "North of 34.44°"),
         #title="Size/number indicates sample size")
         bg='white', box.col='grey', col=1, pt.bg=c(col.S, col.N), pch=21, pt.cex=2)
  box()

  # second plot
  plot(0, type='l', lwd=3, xlim=c(1, 20), ylim=c(0,1.1), #yaxs='i',
       xlab="", ylab="Weight (kg) or fecundity, ", axes=FALSE)
  axis(1, at=1:20)
  axis(2, las=1)
  abline(h=seq(0,1,.2), col='grey')
  lines(1:20, avg.wt[-1], lwd=2, col=3)
  lines(1:20, fec.vec.old[-1], lwd=4, col=1)
  lines(1:20, fec.vec.new[-1], lwd=4, col=rgb(.8,0,0.8,.8))
  legend('bottomright', col=c(3,rgb(.8,0,0.8),1), lwd=c(2,4,4),
         bg='white', box.col='grey',
         legend=c("Mean weight at age",
             "New fecundity (maturity at age x mean weight at age)",
             "Old fecundity (converted from length-based estimates)"))
  box()
  mtext(side=1, line=0, outer=TRUE, "Age")
}
