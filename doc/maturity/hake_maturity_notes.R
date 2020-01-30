dir <- 'c:/SS/hake/maturity/'
mat1 <- read.csv(file.path(dir, '2012_2016_FEAT_WCGBT_ASHOP_Hakematurity_non-confidential.csv'))

# open PNG file
png(file.path(dir, 'maturity_at_age.png'), width=7, height=5, units='in', res=300)

# create empty plot with axes and grid lines
par(mar=c(4,4,1,1))
plot(0, xlim=c(1,15), ylim=c(0,1.1), type='n', xlab="Age", ylab="Fraction mature",
     axes=FALSE)
axis(1, at=1:15)
axis(2, las=1)
abline(h=seq(0,1,.2), col='grey')

# subset for North and South of Point Conception (34.5 degrees N)
mat1.N <- mat1[mat1$Latitude >= 34.5,]
mat1.S <- mat1[mat1$Latitude < 34.5,]

# vector of ages and another to store maturity values
# (starting at 0 as required by wtatage although no ovary samples for age 0 fish)
age.vec <- 0:15
mat.vec <- 0*age.vec
nsamp.vec <- 0*age.vec

# loop over ages
for(a in 1:15){
  # subset for a given age
  mat1.N.age <- mat1.N[!is.na(mat1.N$Age) & mat1.N$Age==a,]
  mat1.S.age <- mat1.S[!is.na(mat1.S$Age) & mat1.S$Age==a,]
  # subset plus group for age 15
  if(a==15){
    mat1.N.age <- mat1.N[!is.na(mat1.N$Age) & mat1.N$Age >= a,]
    mat1.S.age <- mat1.S[!is.na(mat1.S$Age) & mat1.S$Age >= a,]
  }

  # sample size
  nsamp.N <- nrow(mat1.N.age)
  nsamp.S <- nrow(mat1.S.age)

  # calculate y-values (fraction mature)
  y.N <- mean(mat1.N.age$Biological_maturity, na.rm=TRUE)
  y.S <- mean(mat1.S.age$Biological_maturity, na.rm=TRUE)

  # define colors
  col.N <- 'grey'
  col.S <- rgb(1,0,0,.5)
  
  # add points to plot
  points(a, y.N, cex=0.3*sqrt(nsamp.N), col=1, bg=col.N, pch=21)
  points(a, y.S, cex=0.3*sqrt(nsamp.S), col=1, bg=col.S, pch=21)
  text(a, y.N, cex=.8, labels=nsamp.N, pos=if (nsamp.N < 60) 1 else NULL)
  text(a, y.S, cex=.8, labels=nsamp.S, pos=if (nsamp.S < 60) 3 else NULL,
       col=rgb(1,0,0,.5))

  # store maturities
  mat.vec[age.vec == a] <- y.N
  nsamp.vec[age.vec == a] <- nsamp.N
}

# add legend
legend('bottomright', legend=c("South of 34.5°", "North of 34.5°"),
       #title="Size/number indicates sample size")
       col=1, pt.bg=c(col.S, col.N), pch=21, pt.cex=2)
# close png file
dev.off()


if(FALSE){
  # read 2017 base model to get wtatage values
  h0 <- SS_output('c:/github/hake-assessment/models/45_BasePreSRG_v4')

  # fecundity * maturity
  # values from top of wtatage.ss file for fleet = -2
  # origin was 2011 model that included growth
  fec.vec.old <- c(0.0000,0.0000,0.1003,0.2535,0.3992,
                    0.5180,0.6131,0.6895,0.7511,0.8007,
                    0.8406,0.8724,0.8979,0.9181,0.9342,
                   0.9469,0.9569,0.9649,0.9711,0.9761,0.9830)
  # wtatage averaged across years
  # values from top of wtatage.ss file for fleet = -1 for year = -1940
  avg.wt.old <- c(0.0169,0.0848,0.2445,0.3698,0.4772,
                  0.5288,0.5853,0.6624,0.7212,0.7840,
                  0.8524,0.9291,0.9760,1.0603,1.0126,
                  1.0391,1.0391,1.0391,1.0391,1.0391,1.0391)

  mat.vec <- c(mat.vec, rep(1, 5))
  fec.vec.new <- avg.wt.old * mat.vec

  png(file.path(dir, 'fecundity_comparison.png'), width=7, height=5, units='in', res=300)
  plot(0:20, fec.vec.old, type='l', lwd=3, ylim=c(0,1.1), yaxs='i')
  lines(0:20, fec.vec.new, lwd=3, col=2)
  legend('bottomright', col=c(1,2), lwd=3,
         legend=c("Status-quo maturity x fecundity",
             "New maturity-at-age x mean weight-at-age")) 
  dev.off()
}
