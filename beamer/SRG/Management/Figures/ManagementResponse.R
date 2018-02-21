## setwd("C:/GitHub/hake-assessment/beamer/SRG/Management")
## setwd("D:/Documents/GitHub/hake-assessment/beamer/SRG/Management")

## assuming working directory is "/hake-assessment/doc/r"
x <- read.csv("../../data/catch-targets-biomass.csv") # manual update to file in 2017
dir.SRG.management <- "../../beamer/SRG/Management/"

# location is bottom unless points overlap too much
x$loc <- "bottom"
x$loc[x$Year %in% c(2004, 2006, 2012, 2016)] <- "top"

# vector of colors
colvec <- rev(rich.colors.short(n = nrow(x)))
# make text darker
scale <- .9
for(i in 1:length(colvec)){
  tmp <- col2rgb(colvec[i])/255
  colvec[i] <- rgb(tmp[1]*scale,tmp[2]*scale,tmp[3]*scale)
}


doPNG <- FALSE
doEPS <- TRUE
wd=5; ht=4
if(doPNG) png(file.path(dir.SRG.management, "Figures/ManagementResponse.png"),
              height=ht,width=wd,res=300,units="in")
if(doEPS) cairo_ps(filename = file.path(dir.SRG.management,
                       "Figures/ManagementResponse.eps"),
                   width = wd, height = ht, pointsize = 10)
#if(!doPNG & !doEPS) windows(height=ht,width=wd)
par(mfrow=c(1,1),mar=c(3.5,4.6,1,1))
# empty plot
plot(0, type='n', las=1, ylab="", xlab="", xlim=c(0,1000), ylim=c(0,800))
# add points for realized catch
points(x$AssessTac/1000, x$Realized/1000, pch=22, bg=colvec, col="black", cex=1.1)
# add points for TAC
points(x$AssessTac/1000, x$TAC/1000, pch=16, col="black", cex=1.1)


sub1 <- which(x$loc=="bottom")
sub2 <- which(x$loc=="top")
# add text below most of the points
text(x=x$AssessTac[sub1]/1000, y=x$Realized[sub1]/1000,
     label=substring(x$Year,3)[sub1], pch=0, col=colvec[sub1], 
     cex=0.7, srt=0, adj=c(0.5, 2.0))
# add text above points for the subset that overlaps too much
text(x=x$AssessTac[sub2]/1000, y=x$Realized[sub2]/1000, 
     label=substring(x$Year,3)[sub2], pch=0, col=colvec[sub2], 
     cex=0.7, srt=0, adj=c(1.5, 0.5))

segments(x$AssessTac/1000,x$TAC/1000,x$AssessTac/1000,x$Realized/1000+10)
abline(a=0,b=1,col=gray(0.5))
title(xlab="TAC predicted from the assessment (thousand t)",mgp=c(2.1,1,0),cex.lab=1.1)
title(ylab="Thousand t",mgp=c(3.1,1,0),cex.lab=1.1)
legend("topleft",legend=c("TAC implemented by management","Realized catch"),pch=c(16,0),bty="n",cex=1.2)
if(doPNG | doEPS) dev.off()





## par(mar=c(5,4,1,1)+0.1)

## plot(x$AssessTac/1000,x$TAC/1000,
## 	 xlab="TAC predicted from the assessment (x1000 t)",
## 	 ylab="TAC implemented by management (x1000 t)",
## 	 las=1, ylim=c(0,1000), xlim=c(0,1000), pch=16)
## abline(a=0,b=1)





## x <- read.csv("../../data/catch-targets-biomass.csv")
## ht<-5;wd<-10
## if(doPNG) {png("Figures/2_TacCatchManagement.png",height=ht,width=wd,pointsize=10,units="in",res=300)}
## if(!doPNG) {windows(height=ht,width=wd)}
## par(mfrow=c(1,2),mar=c(3.5,4.5,1,1))
## plot(x$AssessTac/1000, x$TAC/1000, las=1, pch=16, col="black", cex=1.2,
##         ylab="", xlab="", xlim=c(0,1000), ylim=c(0,1000))
## abline(a=0,b=1,col=gray(0.5))
## title(xlab="TAC predicted from the assessment (thousand t)",mgp=c(2.1,1,0),cex.lab=1.1)
## title(ylab="TAC implemented by management (thousand t)",mgp=c(3.1,1,0),cex.lab=1.1)
## legend("topleft",legend=NA,title="a)",bty="n",cex=1.2)
## #title(main="Management Response 2004-2014")
## plot(x$AssessTac/1000, x$Realized/1000, las=1, pch=16, col="black", cex=1.2,
##         ylab="", xlab="", xlim=c(0,1000), ylim=c(0,1000))
## abline(a=0,b=1,col=gray(0.5))
## title(xlab="TAC predicted from the assessment (thousand t)",mgp=c(2.1,1,0),cex.lab=1.1)
## title(ylab="Realized catch (thousand t)",mgp=c(3.1,1,0),cex.lab=1.1)
## legend("topleft",legend=NA,title="b)",bty="n",cex=1.2)
## if(doPNG){dev.off()}
