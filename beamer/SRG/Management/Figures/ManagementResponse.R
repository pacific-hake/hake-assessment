setwd("C:/GitHub/hake-assessment/beamer/SRG/Management")
setwd("D:/Documents/GitHub/hake-assessment/beamer/SRG/Management")
x <- read.csv("Figures/CatchTargetsBiomass.csv")


doPNG <- FALSE
doEPS <- TRUE
wd=5; ht=5
if(doPNG) png("Figures/ManagementResponse.png",height=ht,width=wd,res=300,units="in")
if(doEPS) cairo_ps(filename =  "Figures/ManagementResponse.eps", width = wd, height = ht, pointsize = 10)
if(!doPNG & !doEPS) windows(height=ht,width=wd)
par(mfrow=c(1,1),mar=c(3.5,4.5,1,1))
plot(x$AssessTac/1000, x$TAC/1000, las=1, pch=16, col="black", cex=1.1,
        ylab="", xlab="", xlim=c(0,1000), ylim=c(0,1000))
points(x$AssessTac/1000, x$Realized/1000, pch=0, col="black", cex=1.1)
segments(x$AssessTac/1000,x$TAC/1000,x$AssessTac/1000,x$Realized/1000+10)
abline(a=0,b=1,col=gray(0.5))
title(xlab="TAC predicted from the assessment (thousand mt)",mgp=c(2.1,1,0),cex.lab=1.1)
title(ylab="Thousand mt",mgp=c(3.1,1,0),cex.lab=1.1)
legend("topleft",legend=c("TAC implemented by management","Realized catch"),pch=c(16,0),bty="n",cex=1.2)
if(doPNG | doEPS) dev.off()





par(mar=c(5,4,1,1)+0.1)

plot(x$AssessTac/1000,x$TAC/1000,
	 xlab="TAC predicted from the assessment (x1000 mt)",
	 ylab="TAC implemented by management (x1000 mt)",
	 las=1, ylim=c(0,1000), xlim=c(0,1000), pch=16)
abline(a=0,b=1)





x <- read.csv("CatchTargetsBiomass.csv")
ht<-5;wd<-10
if(doPNG) {png("Figures/2_TacCatchManagement.png",height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(height=ht,width=wd)}
par(mfrow=c(1,2),mar=c(3.5,4.5,1,1))
plot(x$AssessTac/1000, x$TAC/1000, las=1, pch=16, col="black", cex=1.2,
        ylab="", xlab="", xlim=c(0,1000), ylim=c(0,1000))
abline(a=0,b=1,col=gray(0.5))
title(xlab="TAC predicted from the assessment (thousand mt)",mgp=c(2.1,1,0),cex.lab=1.1)
title(ylab="TAC implemented by management (thousand mt)",mgp=c(3.1,1,0),cex.lab=1.1)
legend("topleft",legend=NA,title="a)",bty="n",cex=1.2)
#title(main="Management Response 2004-2014")
plot(x$AssessTac/1000, x$Realized/1000, las=1, pch=16, col="black", cex=1.2,
        ylab="", xlab="", xlim=c(0,1000), ylim=c(0,1000))
abline(a=0,b=1,col=gray(0.5))
title(xlab="TAC predicted from the assessment (thousand mt)",mgp=c(2.1,1,0),cex.lab=1.1)
title(ylab="Realized catch (thousand mt)",mgp=c(3.1,1,0),cex.lab=1.1)
legend("topleft",legend=NA,title="b)",bty="n",cex=1.2)
if(doPNG){dev.off()}
