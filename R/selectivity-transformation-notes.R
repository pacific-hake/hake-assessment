# code to testing transformation of time-varying selectivity,
# included in 3.24 model used in 2017, but not in proposed 3.30 mondels for 2018

# testing logistic transformation
trans.fn <- function(x, x.low = -5, x.high = 9){
  -.5*log((x.high - x.low + 2e-7)/(x - x.low + 1e-7) - 1);
}

backtrans.fn <- function(x.tilde, x.low = -5, x.high = 9){
  x.low + (x.high - x.low) / (1 + exp( -2*x.tilde ) ) 
}


# vector of parameter values and their transformed versions
x.vec <- seq(-5, 9, .01)
x.vec.tilde <- trans.fn(x.vec)
# example values
p <-  2.5
eps <- 0.7

png('selectivity_transformation_illustration.png')
# plot standard vs. transformed values
plot(x = x.vec, y = x.vec.tilde, type='l', col=2, ylim=c(-5,5), xaxs='i', lwd=3,
     xlab="Parameter space", ylab="Transformed parameter space", axes=FALSE)
points(x = p, y = trans.fn(p), pch=16, cex=1.5)
points(x = backtrans.fn(trans.fn(p) + eps), y = trans.fn(p) + eps, pch=16, cex=1.5)
arrows(x0 = p, x1 = p, y0 = trans.fn(p), y1 = trans.fn(p) + eps,
       length=0.15, lwd=2)
arrows(x0 = p, x1 = backtrans.fn(trans.fn(p) + eps),
       y0 = trans.fn(p) + eps, y1 = trans.fn(p) + eps,
       length=0.15, lwd=2)
par(mgp = c(3,1.5,0)) # add more space for axis labels
axis(side=1, at=c(-5, p, backtrans.fn(trans.fn(p) + eps), 9),
     labels=c("Lower\nbound", "Base\nvalue",
         "Value with\ndeviation", "Upper\nbound"))
par(mgp = c(3,1,0)) # restore default spacing
axis(2, las=1)
text(x = p, y = trans.fn(p) + eps/2, "epsilon", pos=2)
box()
dev.off()

### check values use last.yr.base_model
p
## [1] 1.56975
eps
## [1] 0.023822
trans.fn(p)
## [1] -0.06154186
trans.fn(p) + eps
## [1] -0.03771986
backtrans.fn(trans.fn(p) + eps)
## [1] 1.736086
last.yr.base_model$SelAgeAdj$Par4[last.yr.base_model$SelAgeAdj$FleetSvy==1 & last.yr.base_model$SelAgeAdj$Yr==2001]
## [1] 1.73608 
# SelAgeAdj is the adjusted parameter value used in the model which matches the
# calculation above


### NOTE: approach used in linear model below has been replaced by
###       taking the derivative of the transformation function,
###       which results in a 7:1 ratio or a new phi of 1.40.

# Linear model fit to range of values indicates slope of about 0.1485
# This indicates that removing the transformation would be best approximated
# by changing the old phi = 0.20 to phi = 0.20/0.1485 ~ 1.35
# which is similar to, but not exactly the same as the phi = 1.50
# chosen by trial and error in the converted models
lm1 <- lm(x.vec.tilde[x.vec < 5 & x.vec > -1] ~ x.vec[x.vec < 5 & x.vec > -1])
lm1

## Call:
## lm(formula = x.vec.tilde[x.vec < 5 & x.vec > -1] ~ x.vec[x.vec < 
##     5 & x.vec > -1])

## Coefficients:
##                   (Intercept)  x.vec[x.vec < 5 & x.vec > -1]  
##                       -0.2971                         0.1485  
