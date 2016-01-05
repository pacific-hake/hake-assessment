### will create tables for 2015 hake assessment
#devtools::install_github("r4ss/r4ss")
library(r4ss)

codeDir <- "~/GitHub/hakeAssess/hake2015/Rcode"
setwd("C:/NOAA2015/Hake")
source(file.path(codeDir,"HakeTableFunctions.R"))
source(file.path(codeDir,"makeMetricsTable.r"))

SSdir <- "Models"
base <- SS_output(dir=file.path(SSdir,"2015hake_basePreSRG"),covar=F)
mcmc <- SSgetMCMC(dir=file.path(SSdir,"2015hake_basePreSRG_mcmc"),writecsv=F)
base$mcmc <- data.frame(mcmc$model1)

stop("Do not source beyond this point\n")

###Executive Summary Tables
yrs <- 2005:2015

tableDir <- "WriteUp/Tables"

spb.mle <- DerivedQuantsTables.ex(models=list(base),
                       variable="SPB_", years=yrs, mcmc=F,
                       probs=c(0.025,0.975), scalar=1e6, csvFileName=file.path(tableDir,"spb.mle"))
spb.mcmc <- DerivedQuantsTables.ex(models=list(base),
                       variable="SPB_", years=yrs, mcmc=T,
                       probs=c(0.025,0.975), scalar=1e6, csvFileName=file.path(tableDir,"spb.mcmc"))
bratio.mcmc <- DerivedQuantsTables.ex(models=list(base),
                       variable="Bratio_", years=yrs, mcmc=T,
                       probs=c(0.025,0.975), scalar=1, csvFileName=file.path(tableDir,"bratio.mcmc"))
recr.mcmc <- DerivedQuantsTables.ex(models=list(base),
                       variable="Recr_", years=yrs, mcmc=T,
                       probs=c(0.025,0.975), scalar=1e6, csvFileName=file.path(tableDir,"recr.mcmc"))
spr.mcmc <- DerivedQuantsTables.ex(models=list(base),
                       variable="SPRratio_", years=yrs, mcmc=T,
                       probs=c(0.025,0.975), scalar=1, csvFileName=file.path(tableDir,"spr.mcmc"))
f.mcmc <- DerivedQuantsTables.ex(models=list(base),
                       variable="F_", years=yrs, mcmc=T,
                       probs=c(0.025,0.975), scalar=1, csvFileName=file.path(tableDir,"f.mcmc"))
dev.mcmc <- DerivedQuantsTables.ex(models=list(base),
                       variable="RecrDev_", years=yrs[-length(yrs)], mcmc=T,
                       probs=c(0.025,0.975), scalar=1, csvFileName=file.path(tableDir,"dev.mcmc"))
totalb.mle <- base$timeseries$Bio_all[base$timeseries$Yr%in%(yrs)]/1e6
expb.mle <- NA
#x <- data.frame(totalb.mle,expb.mle,spb.mle[,2],spb.mcmc[,2],bratio.mcmc[,2],recr.mcmc[,2],dev.mcmc[,2],spr.mcmc[,2],f.mcmc[,2])
#names(x) <- c("totalb.mle","expb.mle","spb.mle","spb.mcmc","bratio.mcmc","recr.mcmc","dev.mcmc","spr.mcmc","f.mcmc")
#write.csv(x,file=paste(tableDir,"ExecSummTimeSeriesMid.csv",sep="/"))


##############################################################################
#Reference points

round(quantile(base$mcmc$SSB_Unfished,prob=c(0.025,0.5,0.975))/2e6,3)
round(quantile(base$mcmc$Recr_Virgin,prob=c(0.025,0.5,0.975))/1e6,3)

round(quantile(base$mcmc$SSB_Btgt,prob=c(0.025,0.5,0.975))/2e6,3)
round(100*quantile(base$mcmc$SPR_Btgt,prob=c(0.025,0.5,0.975)),1)
round(100*quantile(base$mcmc$Fstd_Btgt,prob=c(0.025,0.5,0.975)),1)
round(quantile(base$mcmc$TotYield_Btgt,prob=c(0.025,0.5,0.975))/1e6,3)

round(quantile(base$mcmc$SSB_SPRtgt,prob=c(0.025,0.5,0.975))/2e6,3)
round(100*quantile(base$mcmc$Fstd_SPRtgt,prob=c(0.025,0.5,0.975)),1)
round(quantile(base$mcmc$TotYield_SPRtgt,prob=c(0.025,0.5,0.975))/1e6,3)

round(quantile(base$mcmc$SSB_MSY,prob=c(0.025,0.5,0.975))/2e6,3)
round(100*quantile(base$mcmc$SPR_MSY,prob=c(0.025,0.5,0.975)),1)
round(100*quantile(base$mcmc$Fstd_MSY,prob=c(0.025,0.5,0.975)),1)
round(quantile(base$mcmc$TotYield_MSY,prob=c(0.025,0.5,0.975))/1e6,3)




####################################################################################################
# Metrics
SSdir <- "Models/2015hake_basePreSRG_metrics"
modelsPath   <- file.path(SSdir)
models       <- list.dirs(modelsPath)[-1]
mcmc         <- SSgetMCMC(models,writecsv=F)
metricsTable <- HakeMetricsTableRisk(mcmc,models,year=2016)
write.csv(metricsTable,"Writeup/Tables/metrics2015.csv")

#SPR of 100%
median(mcmc$model6$SPRratio_2015)
#median B2015 = median B2016
median(mcmc$model2$SPB_2015)
median(mcmc$model2$SPB_2016)

#Second year metrics
SSdir <- "Models/2015hake_basePreSRG_metrics2"
modelsPath   <- file.path(SSdir)
models       <- list.dirs(modelsPath)[-1]
mcmc         <- SSgetMCMC(models,writecsv=F,)
metricsTable <- HakeMetricsTableRisk(mcmc,models,year=2017)
write.csv(metricsTable,"Writeup/Tables/metrics2016.csv")

#SPR of 100%
median(mcmc$model6$SPRratio_2016)
#median B2015 = median B2016
median(mcmc$model2$SPB_2016)
median(mcmc$model2$SPB_2017)


#Decision tables
#Create decision table runs (copy base into folders and insert fixed catch)
if(F) {
  SSdir <- "Models"
  baseName <- "2015hake_basePreSRG_mcmc"
  baseFolder <- file.path(SSdir,baseName)
  decFolder <- paste(baseFolder,"decisionTable",sep="_")
  dir.create(decFolder)

  decNames <- c("0","medBsame","300","428","stableCatch","SPR100","defaultHR")
  catchLevels <- list(c(0.01,0.01,0.01), c(180000,80000,0.01), rep(300000,3), rep(428000,3),
                      rep(710000,3), c(730000,650000,600000), c(804576,682782,0))

  for(ii in 1:length(decNames)) {
    i <- decNames[ii]
    newFolder <- file.path(decFolder,paste(ii,baseName,i,sep="_"))
    dir.create(newFolder)
    file.copy(file.path(baseFolder,list.files(baseFolder)),newFolder)

    #insert fixed catches into forecast file
    foreFile <- file.path(newFolder,"forecast.ss")
    fore <- SS_readforecast(foreFile,Nfleets=1,Nareas=1,verbose=F)
    fore$Ncatch <- 3
    fore$ForeCatch <- data.frame(Year=2015:2017, Seas=1, Fleet=1, Catch_or_F=catchLevels[[ii]])
    SS_writeforecast(fore, dir = newFolder, overwrite = TRUE, verbose = F)
    #could potentially call ss3 -mceval from here, but for now do it manually!!!
  }
}

#Read in results and put together table
SSdir <- "Models"
baseName <- "2015hake_basePreSRG_mcmc"
baseFolder <- file.path(SSdir,baseName)
decFolder <- paste(baseFolder,"decisionTable",sep="_")
models       <- list.dirs(decFolder)[-1]
tmp <- SSgetMCMC(dir=models,writecsv=F)
#SPR of 100%
median(tmp$model6$SPRratio_2017)
#default harvest catch
median(tmp$model7$ForeCatch_2017)


HakeDecisionTablesQuantiles.ex(tmp,years=2015:2017,outVar="Bratio_",quantiles=c(0.05,0.25,0.5,0.75,0.95),scalar=1,csvFileName="WriteUp/Tables/BratioDecisionTable.csv")
HakeDecisionTablesQuantiles.ex(tmp,years=2015:2017,outVar="SPRratio_",quantiles=c(0.05,0.25,0.5,0.75,0.95),scalar=1,csvFileName="WriteUp/Tables/SPRratioDecisionTable.csv")

#example of how to create a decision and metrics tables sorting on a specific parameter
# out <- HakeDecisionTablesSort.ex(mcmc,years=2014:2015,sortVar="Recr_2010",outVar="Bratio_",percentages=c(0.1,0.8,0.1),scalar=1,csvFileName=NULL)
# write.csv(out$metrics,file="Writeup/Tables/metrics2015Lower10.csv")
# SSdir <- "C:/NOAA2014/Hake/Models/2014hake_21_metrics"
# modelsPath   <- file.path(SSdir)
# models       <- list.dirs(modelsPath)[-1]
# mcmc         <- SSgetMCMC(models,writecsv=F)
# out <- HakeDecisionTablesSort.ex(mcmc,years=2014:2015,sortVar="Recr_2010",outVar="Bratio_",percentages=c(0.1,0.8,0.1),scalar=1,csvFileName=NULL)
# write.csv(out$metrics,file="Writeup/Tables/metrics2015Lower10.csv")




##############################################################
### Main body tables

## median posterior population estimates from the base model
yrs <- 1966:2015

spb.mcmc <- DerivedQuants.ex(models=list(base),variable="SPB_",mcmc=c(T),scalar=1e3,years=yrs,probs=0.5)
bratio.mcmc <- c(NA,DerivedQuants.ex(models=list(base),variable="Bratio_",mcmc=c(T),scalar=1e-2,years=yrs[-1],probs=0.5))
bratio.mcmc[1] <- median(base$mcmc$SPB_1966/base$mcmc$SPB_Virgin)/1e-2
recr.mcmc <- DerivedQuants.ex(models=list(base),variable="Recr_",mcmc=c(T),scalar=1e3,years=yrs,probs=0.5)
spr.mcmc <- DerivedQuants.ex(models=list(base),variable="SPRratio_",mcmc=c(T),scalar=1e-2,years=yrs,probs=0.5)
f.mcmc <- DerivedQuants.ex(models=list(base),variable="F_",mcmc=c(T),scalar=1e-2,years=yrs,probs=0.5)
x <- data.frame(spb.mcmc,bratio.mcmc,recr.mcmc,spr.mcmc,f.mcmc)
names(x) <- c("spb.mcmc","bratio.mcmc","recr.mcmc","spr.mcmc","f.mcmc")
write.csv(x,file=paste(tableDir,"MainTimeSeries.csv",sep="/"))


spb.mcmc <- DerivedQuants.ex(models=list(base),variable="SPB_",mcmc=c(T),scalar=1e3,years=yrs,probs=0.025)
bratio.mcmc <- c(NA,DerivedQuants.ex(models=list(base),variable="Bratio_",mcmc=c(T),scalar=1e-2,years=yrs[-1],probs=0.025))
bratio.mcmc[1] <- quantile(base$mcmc$SPB_1966/base$mcmc$SPB_Virgin,prob=0.025)/1e-2
recr.mcmc <- DerivedQuants.ex(models=list(base),variable="Recr_",mcmc=c(T),scalar=1e3,years=yrs,probs=0.025)
spr.mcmc <- DerivedQuants.ex(models=list(base),variable="SPRratio_",mcmc=c(T),scalar=1e-2,years=yrs,probs=0.025)
f.mcmc <- DerivedQuants.ex(models=list(base),variable="F_",mcmc=c(T),scalar=1e-2,years=yrs,probs=0.025)
xLo <- data.frame(spb.mcmc,bratio.mcmc,recr.mcmc,spr.mcmc,f.mcmc)
names(xLo) <- c("spb.mcmc","bratio.mcmc","recr.mcmc","spr.mcmc","f.mcmc")

spb.mcmc <- DerivedQuants.ex(models=list(base),variable="SPB_",mcmc=c(T),scalar=1e3,years=yrs,probs=0.975)
bratio.mcmc <- c(NA,DerivedQuants.ex(models=list(base),variable="Bratio_",mcmc=c(T),scalar=1e-2,years=yrs[-1],probs=0.975))
bratio.mcmc[1] <- quantile(base$mcmc$SPB_1966/base$mcmc$SPB_Virgin,prob=0.975)/1e-2
recr.mcmc <- DerivedQuants.ex(models=list(base),variable="Recr_",mcmc=c(T),scalar=1e3,years=yrs,probs=0.975)
spr.mcmc <- DerivedQuants.ex(models=list(base),variable="SPRratio_",mcmc=c(T),scalar=1e-2,years=yrs,probs=0.975)
f.mcmc <- DerivedQuants.ex(models=list(base),variable="F_",mcmc=c(T),scalar=1e-2,years=yrs,probs=0.975)
xHi <- data.frame(spb.mcmc,bratio.mcmc,recr.mcmc,spr.mcmc,f.mcmc)
names(xHi) <- c("spb.mcmc","bratio.mcmc","recr.mcmc","spr.mcmc","f.mcmc")

x <- data.frame(spb=paste(round(xLo[,1],0),"-",round(xHi[,1],0),sep=""),
                bratio=paste(round(xLo[,2],0),"-",round(xHi[,2],0),"%",sep=""),
                recr=paste(round(xLo[,3],0),"-",round(xHi[,3],0),sep=""),
                spr=paste(round(xLo[,4],0),"-",round(xHi[,4],0),"%",sep=""),
                f=paste(round(xLo[,5],0),"-",round(xHi[,5],0),"%",sep=""))

write.csv(x,file=paste(tableDir,"MainTimeSeriesCI.csv",sep="/"))

ind <- base$natage$Time%in%(1966:2015)
tmp <- apply(base$natage[as.character(15:20)],1,sum)
write.csv(cbind(base$natage[ind,as.character(0:14)],tmp[ind])/1e3,file=paste(tableDir,"MLEnumAtAge.csv",sep="/"))
########################################################################################################################



####################################
#MLE vs MCMC
mymodels <- list(base)
models <- SSsummarize(mymodels)
models$mcmc <- vector(mode="list",length=length(mymodels))  #create the mcmc list of model dataframes
models$mcmc <- list(base$mcmc)

nombres <- c("Recr_Virgin","SR_BH_steep","NatM_p_1_Fem_GP_1","Q","Q_extraSD_2_Acoustic_Survey",
    "Recr_2008","Recr_2010","SPB_Virgin","Bratio_2009","Bratio_2015","SPRratio_2014",
    "SSB_SPRtgt","Fstd_SPRtgt","TotYield_SPRtgt")

#tableDir <- "WriteUp/Tables"
modelnames <- c("MLE")
mle <- SStableComparisons(models,modelnames=modelnames,
    names=nombres,
    #csv=TRUE,csvdir=tableDir,csvfile="mleTable.csv",mcmc=F
)
modelnames <- c("MCMC")
mcmc <- SStableComparisons(models,modelnames=modelnames,
    names=nombres,mcmc=T
    #csv=TRUE,csvdir=tableDir,csvfile="mcmcTable.csv"
)

tmpNames <- mcmc[,1]
mle <- mle[-c(1:4),-1]
mcmc <- mcmc[,-1]
out <- data.frame(mle,mcmc)
rownames(out) <- tmpNames
write.csv(out,file="WriteUp/Tables/mleVsMcmcTable.csv")
##########################################################


###################################################################################
# Appendix of parameter estimates

yrs <- 1966:2015

apply(base$mcmc[,3:207],2,median)
write.csv(as.matrix(apply(base$mcmc[,3:207],2,median)),file="WriteUp/Tables/params.csv",)















































































































dev.mcmc <- c(DerivedQuants.ex(models=list(base),variable="Main_RecrDev_",mcmc=c(T),scalar=1,years=2003:2009,probs=0.025),DerivedQuants.ex(models=list(base),variable="Late_RecrDev_",mcmc=c(T),scalar=1,years=2010:2013,probs=0.025),NA)
totalb.mle <- base$timeseries$Bio_all[base$timeseries$Yr%in%(yrs)]/1e6
expb.mle <- NA
spb.mle <- DerivedQuants.ex(models=list(base),variable="SPB_",mcmc=c(F),scalar=1e6,years=yrs,probs=0.025)
spb.mcmc <- DerivedQuants.ex(models=list(base),variable="SPB_",mcmc=c(T),scalar=1e6,years=yrs,probs=0.025)
bratio.mcmc <- c(NA,DerivedQuants.ex(models=list(base),variable="Bratio_",mcmc=c(T),scalar=1,years=yrs[-1],probs=0.025))
recr.mcmc <- DerivedQuants.ex(models=list(base),variable="Recr_",mcmc=c(T),scalar=1e6,years=yrs,probs=0.025)
spr.mcmc <- DerivedQuants.ex(models=list(base),variable="SPRratio_",mcmc=c(T),scalar=1,years=yrs,probs=0.025)
f.mcmc <- DerivedQuants.ex(models=list(base),variable="F_",mcmc=c(T),scalar=1,years=yrs,probs=0.025)
x <- data.frame(totalb.mle,expb.mle,spb.mle,spb.mcmc,bratio.mcmc,recr.mcmc,dev.mcmc,spr.mcmc,f.mcmc)
names(x) <- c("totalb.mle","expb.mle","spb.mle","spb.mcmc","bratio.mcmc","recr.mcmc","dev.mcmc","spr.mcmc","f.mcmc")
write.csv(x,file=paste(tableDir,"ExecSummTimeSeriesLow.csv",sep="/"))

dev.mcmc <- c(DerivedQuants.ex(models=list(base),variable="Main_RecrDev_",mcmc=c(T),scalar=1,years=2003:2009,probs=0.975),DerivedQuants.ex(models=list(base),variable="Late_RecrDev_",mcmc=c(T),scalar=1,years=2010:2013,probs=0.975),NA)
totalb.mle <- base$timeseries$Bio_all[base$timeseries$Yr%in%(yrs)]/1e6
expb.mle <- NA
spb.mle <- DerivedQuants.ex(models=list(base),variable="SPB_",mcmc=c(F),scalar=1e6,years=yrs,probs=0.975)
spb.mcmc <- DerivedQuants.ex(models=list(base),variable="SPB_",mcmc=c(T),scalar=1e6,years=yrs,probs=0.975)
bratio.mcmc <- c(NA,DerivedQuants.ex(models=list(base),variable="Bratio_",mcmc=c(T),scalar=1,years=yrs[-1],probs=0.975))
recr.mcmc <- DerivedQuants.ex(models=list(base),variable="Recr_",mcmc=c(T),scalar=1e6,years=yrs,probs=0.975)
spr.mcmc <- DerivedQuants.ex(models=list(base),variable="SPRratio_",mcmc=c(T),scalar=1,years=yrs,probs=0.975)
f.mcmc <- DerivedQuants.ex(models=list(base),variable="F_",mcmc=c(T),scalar=1,years=yrs,probs=0.975)
x <- data.frame(totalb.mle,expb.mle,spb.mle,spb.mcmc,bratio.mcmc,recr.mcmc,dev.mcmc,spr.mcmc,f.mcmc)
names(x) <- c("totalb.mle","expb.mle","spb.mle","spb.mcmc","bratio.mcmc","recr.mcmc","dev.mcmc","spr.mcmc","f.mcmc")
write.csv(x,file=paste(tableDir,"ExecSummTimeSeriesHigh.csv",sep="/"))


recr.mcmc <- DerivedQuants.ex(models=list(base),variable="Main_RecrDev_",mcmc=c(T),scalar=1,years=1999:2009,probs=0.025)
recr.mcmc <- DerivedQuants.ex(models=list(base),variable="Main_RecrDev_",mcmc=c(T),scalar=1,years=1999:2009,probs=0.5)
recr.mcmc <- DerivedQuants.ex(models=list(base),variable="Main_RecrDev_",mcmc=c(T),scalar=1,years=1999:2009,probs=0.975)
recr.mcmc <- DerivedQuants.ex(models=list(base),variable="Late_RecrDev_",mcmc=c(T),scalar=1,years=2010:2012,probs=0.025)
recr.mcmc <- DerivedQuants.ex(models=list(base),variable="Late_RecrDev_",mcmc=c(T),scalar=1,years=2010:2012,probs=0.5)
recr.mcmc <- DerivedQuants.ex(models=list(base),variable="Late_RecrDev_",mcmc=c(T),scalar=1,years=2010:2012,probs=0.975)









rbind(DerivedQuants.ex(models=list(base),variable="Recr_",mcmc=c(F),scalar=1e6,years="Virgin",probs=c(0.025,0.5,0.975)),
      #DerivedQuants.ex(models=list(base),variable="SR_BH_",mcmc=c(F),scalar=1e6,years="steep",probs=c(0.025,0.5,0.975)),
      #,
      #,
      DerivedQuants.ex(models=list(base),variable="Recr_",mcmc=c(F),scalar=1e6,years=c(2008,2010),probs=c(0.025,0.5,0.975)),
      DerivedQuants.ex(models=list(base),variable="SPB_",mcmc=c(F),scalar=1e6,years="Virgin",probs=c(0.025,0.5,0.975)),
      DerivedQuants.ex(models=list(base),variable="Bratio_",mcmc=c(F),scalar=1e6,years="2013",probs=c(0.025,0.5,0.975)),

t(apply(base$mcmc[,names],2,quantile,prob=c(0.025,0.5,0.975)))

tableDir <- "C:\\NOAA2014\\Hake\\WriteUp\\Tables"

hake2013 <- SS_output(dir=paste(SSdir,"2013hake_19",sep="/"),covar=T,verbose=F)
hake2014.06 <- SS_output(dir=paste(SSdir,"2014hake_06_final2013Data",sep="/"),covar=T,verbose=F)
hake2014.07 <- SS_output(dir=paste(SSdir,"2014hake_07_fishery2013Data",sep="/"),covar=T,verbose=F)
hake2014.08 <- SS_output(dir=paste(SSdir,"2014hake_08_survey2013Data",sep="/"),covar=T,verbose=F)
#add final 2013 fishery catch and data and final 2013 acoust data
mymodels <- list(hake2013,hake2014.06,hake2014.07,hake2014.08)
modelnames <- c("hake2013","hake2014_06_all2013Data","hake2014_07_fishery2013Data","hake2014_02_survey2013Data")
mysummary <- SSsummarize(mymodels)
SStableComparisons(mysummary,modelnames=modelnames,
    names=c("SPB_Virgin","SPB_2012","SPB_2013",
    "Bratio_2012","Bratio_2013","Bratio_2014","Recr_2008","Recr_2010"),
    csv=TRUE,
    csvdir=tableDir,
    csvfile="bridgeTable.csv",mcmc=F
)


hake27 <- SS_output(dir=file.path(SSdir,"2014hake_27_newMat"),covar=T)
hake28 <- SS_output(dir=file.path(SSdir,"2014hake_28_no2012survey"),covar=T)
hake29 <- SS_output(dir=file.path(SSdir,"2014hake_29_highTVfrom21"),covar=T)
hake30 <- SS_output(dir=file.path(SSdir,"2014hake_30_TVselex1975start_from21"),covar=T)
mymodels <- list(base,hake27,hake28,hake29,hake30)
modelnames <- c("Base","New maturity","No 2012 Survey","High TV sel","TV Sel from 1975")
mysummary <- SSsummarize(mymodels)
SStableComparisons(mysummary,modelnames=modelnames,
    names=c("Recr_Virgin","SR_BH_steep","NatM_p_1_Fem_GP_1","Q_extraSD_2_Acoustic_Survey",
    "Recr_2008","Recr_2010","SPB_Virgin","Bratio_2014","SPRratio_2013",
    #"SSB_Btgt","SPR_Btgt","Fstd_Btgt","TotYield_Btgt",
    "SSB_SPRtgt","Fstd_SPRtgt","TotYield_SPRtgt"),
    #"SSB_MSY","SPR_MSY","Fstd_MSY","TotYield_MSY"),
    csv=TRUE,
    csvdir=tableDir,
    csvfile="sens.MLE.Table.csv",mcmc=F
)



hake25 <- SS_output(dir=file.path(SSdir,"2014hake_25_noTVfrom21"),covar=T)
mcmc <- SSgetMCMC(dir=file.path(SSdir,"2014hake_25_noTVfrom21_MCMC"),writecsv=F)
hake25$mcmc <- data.frame(mcmc$model1)
hake31 <- SS_output(dir=file.path(SSdir,"2014hake_31_maxSelex10"),covar=T)
mcmc <- SSgetMCMC(dir=file.path(SSdir,"2014hake_31_maxSelex10_MCMC"),writecsv=F)
hake31$mcmc <- data.frame(mcmc$model1)
mymodels <- list(base,hake31,hake25)
modelnames <- c("Base","Selex age-10","No TV selex")
mysummary <- SSsummarize(mymodels)
mysummary$mcmc <- list(base$mcmc,hake31$mcmc,hake25$mcmc)
SStableComparisons(mysummary,modelnames=modelnames,
    names=c("Recr_Virgin","SR_BH_steep","NatM_p_1_Fem_GP_1","Q_extraSD_2_Acoustic_Survey",
    "Recr_2008","Recr_2010","SPB_Virgin","Bratio_2014","SPRratio_2013",
    #"SSB_Btgt","SPR_Btgt","Fstd_Btgt","TotYield_Btgt",
    "SSB_SPRtgt","Fstd_SPRtgt","TotYield_SPRtgt"),
    #"SSB_MSY","SPR_MSY","Fstd_MSY","TotYield_MSY"),
    csv=TRUE,
    csvdir=tableDir,
    csvfile="sens.31.25.Table.csv",mcmc=T
)



pdf("WriteUp/Sensitivities/hake31.pdf")
SSplotComparisons(mysummary, legendlabels=modelnames,endyr=2014,new=F,minbthresh = 0,
                densitynames = c("SPB_Virgin", "R0","NatM_p_1_Fem_GP_1","SR_BH_steep","SPB_2013","Bratio_2013","Recr_2008","Recr_2009","Recr_2010","Recr_2011","ForeCatch_2014"))
#plot and compare selex
columns <- 1:9
# setup colors, points, and line types
if(length(mymodels) > 3) col <- rc(length(mymodels)+1,alpha=0.6)[-1]
if(length(mymodels) < 3) col <- rc(length(mymodels),alpha=0.6)
if(length(mymodels) == 3) col <- c(rgb(0,0,1,0.6),rgb(1,0,0,0.6),rgb(0,205/255,0,0.6))
tmp <- mysummary$agesel
#commSelex <- tmp[tmp$factor=="Asel" & tmp$year==2013 & tmp$fleet==1,as.character(0:20)]
acSelex <- tmp[tmp$factor=="Asel" & tmp$year==2013 & tmp$fleet==2,as.character(0:20)]
par(mfrow=c(1,1))
#plot(as.numeric(names(commSelex))[columns],commSelex[1,columns],type="b",col=col[1],xlab="Age",ylab="Selectivity",lwd=3,pch=20,main="Commercial")
#for(i in 2:nrow(commSelex)) {
#    lines(as.numeric(names(commSelex))[columns],commSelex[i,columns],col=col[i],lwd=3,type="b",pch=20)
#}
plot(as.numeric(names(acSelex))[columns],acSelex[1,columns],type="b",col=col[1],xlab="Age",ylab="Selectivity",lwd=3,pch=20,main="Survey")
for(i in 2:nrow(acSelex)) {
    lines(as.numeric(names(acSelex))[columns],acSelex[i,columns],col=col[i],lwd=3,type="b",pch=20)
}
legend("bottomright",modelnames,col=col,pch=20,lty=1)
dev.off()


###Main Doc Medians










#retro table
retro01 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro01"),covar=F)
retro02 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro02"),covar=F)
retro03 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro03"),covar=F)
retro04 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro04"),covar=F)
retro05 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro05"),covar=F)
retro06 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro06"),covar=F)
retro07 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro07"),covar=F)
retro08 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro08"),covar=F)
retro09 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro09"),covar=F)
retro10 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro10"),covar=F)
retro11 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro11"),covar=F)
retro12 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro12"),covar=F)
retro13 <- SS_output(dir=file.path(SSdir,"2014hake_21_retros/2014hake_21_retro13"),covar=F)

retroDir <- file.path(SSdir,"2014hake_21_retros")
mcmc <- SSgetMCMC(dir=file.path(retroDir,dir(retroDir)),writecsv=F)
mcmc <- list(base$mcmc,mcmc[[1]],mcmc[[2]],mcmc[[3]],mcmc[[4]],mcmc[[5]])

mymodels <- list(base,retro01,retro02,retro03,retro04,retro05)
modelnames <- c("Base","Retro -1","Retro -2","Retro -3","Retro -4","Retro -5")
mysummary <- SSsummarize(mymodels)
names(mcmc) <- modelnames
mysummary$mcmc <- mcmc
SStableComparisons(mysummary,modelnames=modelnames,
    names=c("R0","SR_BH_steep","NatM_p_1_Fem_GP_1","Q_extraSD_2_Acoustic_Survey",
    "Recr_2008","Recr_2010","SPB_Virgin","Bratio_2014","SPRratio_2013",
    "SSB_SPRtgt","Fstd_SPRtgt","TotYield_SPRtgt"),
    csv=TRUE,
    csvdir="Writeup/Tables",
    csvfile="retroTable.csv",
    mcmc=T
)


retroDir <- file.path(SSdir,"2014hake_21_retros")
mcmc <- SSgetMCMC(dir=rev(file.path(retroDir,dir(retroDir))),writecsv=F)
mcmc[[length(mcmc)+1]] <- base$mcmc

n <- length(mcmc)
recdevs <- NULL
  for(imodel in (1:n)){
    tmp <- unique(c(grep("_RecrDev_",names(mcmc[[imodel]])),
                    #grep("_InitAge_",names(mcmc[[imodel]])),
                    grep("ForeRecr_",names(mcmc[[imodel]]))))
    if(length(tmp) > 0) { #there are some mcmc values to use
      mcmc.tmp <- mcmc[[imodel]][,tmp] # subset of columns from MCMC for this model
      mcmclabs <- names(mcmc.tmp)
      med   <- apply(mcmc.tmp,2,quantile,prob=0.5)   #hard-wired probability
      recdevs <- cbind(recdevs,med)
    }
  }
recdevs <- cbind(as.numeric(unlist(lapply(strsplit(rownames(recdevs),"_"),function(x){x[length(x)]}))),recdevs)
colnames(recdevs)[1] <- "Yr"


yrs <- (2014-n):2013
cohorts <- 1999:2012
tmp <- (min(yrs)-max(cohorts)):(max(yrs)-min(cohorts))
ageMat <- matrix(NA,nrow=length(cohorts),ncol=length(tmp),dimnames=list(cohorts,tmp))
for(coh in cohorts) {
    ages <- yrs-coh
    ageMat[as.character(coh),as.character(ages)] <- recdevs[recdevs[,"Yr"]==coh,(1:n)+1]
}

#Retros on 25
tmp <- SSgetMCMC(dir=file.path(SSdir,"2014hake_25_noTVfrom21_MCMC"),writecsv=F)
retroDir <- file.path(SSdir,"2014hake_25_retros")
mcmc25 <- SSgetMCMC(dir=rev(file.path(retroDir,dir(retroDir))),writecsv=F)
mcmc25[[length(mcmc25)+1]] <- tmp$model1
n <- length(mcmc25)
recdevs <- NULL
  for(imodel in (1:n)){
    tmp <- unique(c(grep("_RecrDev_",names(mcmc25[[imodel]])),
                    #grep("_InitAge_",names(tmp[[imodel]])),
                    grep("ForeRecr_",names(mcmc25[[imodel]]))))
    if(length(tmp) > 0) { #there are some mcmc values to use
      mcmc.tmp <- mcmc25[[imodel]][,tmp] # subset of columns from MCMC for this model
      mcmclabs <- names(mcmc.tmp)
      med   <- apply(mcmc.tmp,2,quantile,prob=0.5)   #hard-wired probability
      recdevs <- cbind(recdevs,med)
    }
  }
recdevs <- cbind(as.numeric(unlist(lapply(strsplit(rownames(recdevs),"_"),function(x){x[length(x)]}))),recdevs)
colnames(recdevs)[1] <- "Yr"

yrs <- (2014-n):2013
cohorts <- 1999:2012
tmp <- (min(yrs)-max(cohorts)):(max(yrs)-min(cohorts))
ageMat25 <- matrix(NA,nrow=length(cohorts),ncol=length(tmp),dimnames=list(cohorts,tmp))
for(coh in cohorts) {
    ages <- yrs-coh
    ageMat25[as.character(coh),as.character(ages)] <- recdevs[recdevs[,"Yr"]==coh,(1:n)+1]
}

ageMat[,as.character(0:5)]
ageMat25[,as.character(0:5)]











#Appendix
tableDir <- "WriteUp/Tables"
write.csv(cbind(apply(base$mcmc[,3:201],2,median)),file=paste(tableDir,"paramMedians.csv",sep="/"))

















x.fn <- function(x,d=2) {paste(round(x[,1],d),round(x[,2],d),sep="-")}
spb.mcmc <- DerivedQuants.ex(models=list(base),variable="SPB_",mcmc=c(T),scalar=1e6,years=yrs,probs=c(0.025,0.975))
bratio.mcmc <- rbind(c(NA,NA),DerivedQuants.ex(models=list(base),variable="Bratio_",mcmc=c(T),scalar=1,years=yrs[-1],probs=c(0.025,0.975)))
recr.mcmc <- DerivedQuants.ex(models=list(base),variable="Recr_",mcmc=c(T),scalar=1e6,years=yrs,probs=c(0.025,0.975))
spr.mcmc <- DerivedQuants.ex(models=list(base),variable="SPRratio_",mcmc=c(T),scalar=1,years=yrs,probs=c(0.025,0.975))
f.mcmc <- DerivedQuants.ex(models=list(base),variable="F_",mcmc=c(T),scalar=1,years=yrs,probs=c(0.025,0.975))
x <- data.frame(x.fn(spb.mcmc),x.fn(bratio.mcmc),x.fn(recr.mcmc),x.fn(spr.mcmc),x.fn(f.mcmc))
names(x) <- c("spb.mcmc","bratio.mcmc","recr.mcmc","spr.mcmc","f.mcmc")
write.csv(x,file=paste(tableDir,"ExecSummTimeSeriesInterval.csv",sep="/"))




































#setwd("C:/NOAA2013/Hake/Models")
######modelsPath   <- file.path("..","metricsTableRuns")
SSdir <- "C:/NOAA2013/Hake/Models/metricsTableRuns"
modelsPath   <- file.path(SSdir)
models       <- list.dirs(modelsPath)[-1]
mcmc         <- SSgetMCMC(models,writecsv=F)
metricsTable <- HakeMetricsTable(mcmc,models,makePercentages=F,decPlaces=4)
#metricsTable <- HakeMetricsTable(mcmc,models)
write.csv(metricsTable,"WriteUp/Tables/metricsTable.csv",quote=F)
metricsTable <- HakeMetricsTableRisk(mcmc,models,makePercentages=F,decPlaces=4)
write.csv(metricsTable,"WriteUp/Tables/metricsTableRisk.csv",quote=F)

############
#Decision table based on lower 10% of 2010 recruitment
SSdir <- "C:/NOAA2013/Hake/Models/decisionTablesRuns"
models <- dir(SSdir)
tmp <- SSgetMCMC(dir=paste(SSdir,models,sep="/"),writecsv=F)

###THE SORTING OF CATCH IS BY CHAARCTER, NOT NUMERIC. MANUALLY resort FIX FOR NOW
out <- HakeDecisionTablesSort.ex(tmp,years=2013:2014,sortVar="Recr_2010",outVar="Bratio_",percentages=c(0.1,0.8,0.1),scalar=1,csvFileName="WriteUp/Tables/BratioLower10.csv")
out <- HakeDecisionTablesSort.ex(tmp,years=2013:2014,sortVar="Recr_2010",outVar="SPRratio_",percentages=c(0.1,0.8,0.1),scalar=1,csvFileName="WriteUp/Tables/SPRrationLower10.csv")

SSdir <- "C:/NOAA2013/Hake/Models/metricsTableRuns"
modelsPath   <- file.path(SSdir)
models       <- list.dirs(modelsPath)[-1]
mcmc         <- SSgetMCMC(models,writecsv=F)
out <- HakeDecisionTablesSort.ex(mcmc,years=2013:2014,sortVar="Recr_2010",outVar="Bratio_",percentages=c(0.1,0.8,0.1),scalar=1,csvFileName=NULL)
write.csv(out$metrics,file="Writeup/Tables/metricsLower10.csv")



#draw a figure of metrics
metrics <- read.csv("Models/SRGrequests/lower10metrics.csv")
metrics$Catch <- metrics$Catch/1e3
catches <- metrics$Catch


ht <- 3.75; wd<- 6.5
if(doPNG) {png("Models/SRGrequests/lower10metrics.png",height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(width=wd,height=ht)}
par(mfrow=c(1,1),las=1,mar=c(3.6,3.6,1,1),oma=c(0,0,0,0),mgp=c(2.5,1,0))
plot(metrics$Catch,metrics[,2],ylim=c(0,1),xaxt="n",ylab="Probability",xlab="Catch ('000 mt)",type="b",lty=2,pch=16)
lines(metrics$Catch,metrics[,3],col="blue",type="b",lty=2,pch=17)
lines(metrics$Catch,metrics[,4],col="green",type="b",lty=2,pch=17)
lines(metrics$Catch,metrics[,5],col="orange",type="b",lty=2,pch=17)
lines(metrics$Catch,metrics[,6],col="red",type="b",lty=2,pch=15)
lines(metrics$Catch,metrics[,7],col="tan",type="b",lty=2,pch=18)
abline(h=0.5,lty=2,lwd=1,col="grey")
legend("topleft",c("P(B2014 < B2013): Stock declines in 2014","P(2013 Fishing Intensity > Target of 40%)","P(C2014 < C2013): F40% catch declines in 2014","P(B2013 < B40%)","P(B2013 < B25%)","P(B2013 < B10%)"),col=c("black","red","tan","blue","green","orange"),lty=1,lwd=2,pch=c(16,15,18,17,17,17),cex=0.7)
axis(1,at=round(metrics$Catch,0),cex.axis=0.9,las=2)
if(doPNG){dev.off()}







#Reference points

round(quantile(base$mcmc$SSB_Unfished,prob=c(0.025,0.5,0.975))/2e6,3)
round(quantile(base$mcmc$Recr_Virgin,prob=c(0.025,0.5,0.975))/1e6,3)

round(quantile(base$mcmc$SSB_Btgt,prob=c(0.025,0.5,0.975))/2e6,3)
round(100*quantile(base$mcmc$SPR_Btgt,prob=c(0.025,0.5,0.975)),1)
round(100*quantile(base$mcmc$Fstd_Btgt,prob=c(0.025,0.5,0.975)),1)
round(quantile(base$mcmc$TotYield_Btgt,prob=c(0.025,0.5,0.975))/1e6,3)

round(quantile(base$mcmc$SSB_SPRtgt,prob=c(0.025,0.5,0.975))/2e6,3)
round(100*quantile(base$mcmc$Fstd_SPRtgt,prob=c(0.025,0.5,0.975)),1)
round(quantile(base$mcmc$TotYield_SPRtgt,prob=c(0.025,0.5,0.975))/1e6,3)

round(quantile(base$mcmc$SSB_MSY,prob=c(0.025,0.5,0.975))/2e6,3)
round(100*quantile(base$mcmc$SPR_MSY,prob=c(0.025,0.5,0.975)),1)
round(100*quantile(base$mcmc$Fstd_MSY,prob=c(0.025,0.5,0.975)),1)
round(quantile(base$mcmc$TotYield_MSY,prob=c(0.025,0.5,0.975))/1e6,3)





















###########################
tableDir <- "C:\\NOAA2013\\Hake\\WriteUp\\Tables"
DerivedQuantsTables.ex(models=list(base),mcmc=c(T),scalar=1e6,csvFileName=paste(tableDir,"Table.b_SPB",sep="\\"))
DerivedQuantsTables.ex(models=list(base),variable="Bratio_",mcmc=T,csvFileName=paste(tableDir,"Table.c_depl",sep="\\"))
DerivedQuantsTables.ex(models=list(base),variable="Recr_",mcmc=T,scalar=c(1e6,1),csvFileName=paste(tableDir,"Table.d_Recr",sep="\\"))  #check the scalar!
DerivedQuantsTables.ex(models=list(base),variable="SPRratio_",years=2002:2011,mcmc=T,csvFileName=paste(tableDir,"Table.e_SPRratio",sep="\\"))
DerivedQuantsTables.ex(models=list(base),variable="F_",years=2002:2011,mcmc=T,csvFileName=paste(tableDir,"Table.f_ExpRate",sep="\\"))


#################################
# Decision tables

#probability metrics table with base only
HakeMetricsTable.fn(mcmc,year=2013)





#Bridge
EA2013.00 <- SS_output(dir=paste(SSdir,"2013hake_00",sep="/"),covar=T,verbose=F)
EA2013.09 <- SS_output(dir=paste(SSdir,"2013hake_09",sep="/"),covar=T,verbose=F)
EA2013.10 <- SS_output(dir=paste(SSdir,"2013hake_10",sep="/"),covar=T,verbose=F)
c(EA2013.00$derived_quants[EA2013.00$derived_quants$LABEL=="SPB_Virgin","Value"],EA2013.09$derived_quants[EA2013.09$derived_quants$LABEL=="SPB_Virgin","Value"],EA2013.10$derived_quants[EA2013.10$derived_quants$LABEL=="SPB_Virgin","Value"],EA2013.19$derived_quants[EA2013.19$derived_quants$LABEL=="SPB_Virgin","Value"])/2
c(EA2013.00$derived_quants[EA2013.00$derived_quants$LABEL=="SPB_2012","Value"],EA2013.09$derived_quants[EA2013.09$derived_quants$LABEL=="SPB_2012","Value"],EA2013.10$derived_quants[EA2013.10$derived_quants$LABEL=="SPB_2012","Value"],EA2013.19$derived_quants[EA2013.19$derived_quants$LABEL=="SPB_2012","Value"])/2
c(EA2013.00$derived_quants[EA2013.00$derived_quants$LABEL=="SPB_2013","Value"],EA2013.09$derived_quants[EA2013.09$derived_quants$LABEL=="SPB_2013","Value"],EA2013.10$derived_quants[EA2013.10$derived_quants$LABEL=="SPB_2013","Value"],EA2013.19$derived_quants[EA2013.19$derived_quants$LABEL=="SPB_2013","Value"])/2
c(EA2013.00$derived_quants[EA2013.00$derived_quants$LABEL=="Bratio_2011","Value"],EA2013.09$derived_quants[EA2013.09$derived_quants$LABEL=="Bratio_2011","Value"],EA2013.10$derived_quants[EA2013.10$derived_quants$LABEL=="Bratio_2011","Value"],EA2013.19$derived_quants[EA2013.19$derived_quants$LABEL=="Bratio_2011","Value"])
c(EA2013.00$derived_quants[EA2013.00$derived_quants$LABEL=="Bratio_2012","Value"],EA2013.09$derived_quants[EA2013.09$derived_quants$LABEL=="Bratio_2012","Value"],EA2013.10$derived_quants[EA2013.10$derived_quants$LABEL=="Bratio_2012","Value"],EA2013.19$derived_quants[EA2013.19$derived_quants$LABEL=="Bratio_2012","Value"])
c(EA2013.00$derived_quants[EA2013.00$derived_quants$LABEL=="Bratio_2013","Value"],EA2013.09$derived_quants[EA2013.09$derived_quants$LABEL=="Bratio_2013","Value"],EA2013.10$derived_quants[EA2013.10$derived_quants$LABEL=="Bratio_2013","Value"],EA2013.19$derived_quants[EA2013.19$derived_quants$LABEL=="Bratio_2013","Value"])

mymodels <- list(EA2013.00,EA2013.09,EA2013.10,EA2013.19); modelnames <- c("2012 base model","Update 2011 data","Add 2012 fishery data      ","All 2012 data")
models <- SSsummarize(mymodels)
tableDir <- "C:\\NOAA2013\\Hake\\WriteUp\\Tables"
SStableComparisons(models,modelnames=modelnames,
    names=c("SPB_Virgin","SPB_2012","SPB_2013","Bratio_2012","Bratio_2012","Bratio_2012","Recr_2008","Recr_2010"),
    csv=TRUE,
    csvdir=tableDir,
    csvfile="mleBridgeTable.csv"
)






SSdir <- "C:\\NOAA2013\\Hake\\Models"
base <- SS_output(dir=paste(SSdir,"2013hake_19",sep="/"),covar=F)
mcmc <- SSgetMCMC(dir=paste(SSdir,"2013hake_19_mcmc",sep="/"),writecsv=F)
base$mcmc <- data.frame(mcmc$model1)

yrs <- 1966:2014

tableDir <- "C:\\NOAA2013\\Hake\\WriteUp\\Tables"
totalb.mle <- base$timeseries$Bio_all[base$timeseries$Yr%in%(yrs)]/1e6
expb.mle <- NA
spb.mle <- DerivedQuants.ex(models=list(base),variable="SPB_",mcmc=c(F),scalar=1e6,years=yrs,probs=0.5)
spb.mcmc <- DerivedQuants.ex(models=list(base),variable="SPB_",mcmc=c(T),scalar=1e6,years=yrs,probs=0.5)
bratio.mcmc <- c(NA,DerivedQuants.ex(models=list(base),variable="Bratio_",mcmc=c(T),scalar=1,years=yrs[-1],probs=0.5))
recr.mcmc <- DerivedQuants.ex(models=list(base),variable="Recr_",mcmc=c(T),scalar=1e6,years=yrs,probs=0.5)
spr.mcmc <- DerivedQuants.ex(models=list(base),variable="SPRratio_",mcmc=c(T),scalar=1,years=yrs,probs=0.5)
f.mcmc <- DerivedQuants.ex(models=list(base),variable="F_",mcmc=c(T),scalar=1,years=yrs,probs=0.5)
x <- data.frame(totalb.mle,expb.mle,spb.mle,spb.mcmc,bratio.mcmc,recr.mcmc,spr.mcmc,f.mcmc)
names(x) <- c("totalb.mle","expb.mle","spb.mle","spb.mcmc","bratio.mcmc","recr.mcmc","spr.mcmc","f.mcmc")
write.csv(x,file=paste(tableDir,"timeSeries.csv",sep="/"))

x.fn <- function(x,d=2) {paste(round(x[,1],d),round(x[,2],d),sep="-")}
spb.mcmc <- DerivedQuants.ex(models=list(base),variable="SPB_",mcmc=c(T),scalar=1e6,years=yrs,probs=c(0.025,0.975))
bratio.mcmc <- rbind(c(NA,NA),DerivedQuants.ex(models=list(base),variable="Bratio_",mcmc=c(T),scalar=1,years=yrs[-1],probs=c(0.025,0.975)))
recr.mcmc <- DerivedQuants.ex(models=list(base),variable="Recr_",mcmc=c(T),scalar=1e6,years=yrs,probs=c(0.025,0.975))
spr.mcmc <- DerivedQuants.ex(models=list(base),variable="SPRratio_",mcmc=c(T),scalar=1,years=yrs,probs=c(0.025,0.975))
f.mcmc <- DerivedQuants.ex(models=list(base),variable="F_",mcmc=c(T),scalar=1,years=yrs,probs=c(0.025,0.975))
x <- data.frame(x.fn(spb.mcmc),x.fn(bratio.mcmc),x.fn(recr.mcmc),x.fn(spr.mcmc),x.fn(f.mcmc))
names(x) <- c("spb.mcmc","bratio.mcmc","recr.mcmc","spr.mcmc","f.mcmc")
write.csv(x,file=paste(tableDir,"timeSeriesInterval.csv",sep="/"))


x <- base$natage
x <- x[x[,"Beg/Mid"]=="B" & x$Yr%in%1966:2013,as.character(0:20)]
x[,"15"] <- apply(x[,as.character(15:20)],1,sum)
x <- x[,as.character(0:15)]/1e6
row.names(x) <- as.character(1966:2013)
write.csv(x,file=paste(tableDir,"numAtAge.csv",sep="/"))



#################
#  decision table sorted on 2010 recruitment
SSdir <- "C:/NOAA2013/Hake/Models/decisionTablesRuns"
models <- dir(SSdir)
tmp <- SSgetMCMC(dir=paste(SSdir,models,sep="/"),writecsv=F)

out <- HakeDecisionTablesSort.ex(tmp,years=2013:2014,sortVar="Recr_2010",outVar="Bratio_",percentages=c(0.25,0.5,0.25),scalar=1,csvFileName=NULL)

out$metrics[seq(1,nrow(out$metrics),3),]

out <- HakeDecisionTablesSort.ex(tmp,years=2013:2014,sortVar="Recr_2010",outVar="SPRratio_",percentages=c(0.25,0.5,0.25),scalar=1,csvFileName=NULL)





######################################
#retro recruitment
SSdir <- "C:/NOAA2013/Hake/Models"
models <- paste("2013hake_19_retro",1:12,"_mcmc",sep="")
tmp <- SSgetMCMC(dir=paste(SSdir,models,sep="/"),writecsv=F)
cohorts <- 1999:2010
out <- matrix(NA,ncol=length(-13:13),nrow=length(cohorts))
dimnames(out) <- list(as.character(1999:2010),as.character(-13:13))
for(i in 1:12) {
    yr <- 2013-i
    age <- yr-cohorts-1
    x <- apply(tmp[[i]][,paste("Recr_",1999:2010,sep="")],2,median)
    for(a in 1:length(age)) {
        out[a,as.character(age)[a]] <- x[a]
    }
}
age <- 2013-cohorts-1
x <- apply(base$mcmc[,paste("Recr_",1999:2010,sep="")],2,median)
for(a in 1:length(age)) {
    out[a,as.character(age)[a]] <- x[a]
}
out[,as.character(0:13)]

#by years as columns
cohorts <- 1999:2010
out <- matrix(NA,ncol=length(1999:2012),nrow=length(cohorts))
dimnames(out) <- list(as.character(1999:2010),as.character(1999:2012))
for(i in 1:12) {
    yr <- 2013-i
    x <- apply(tmp[[i]][,paste("Recr_",1999:2010,sep="")],2,median)
    out[,as.character(yr)] <- x
}


#2008 was 2 years old in retro -2
i <- 2
quantile(tmp[[i]]$Recr_2008,prob=c(0,0.01,0.025,0.5,0.975))







######################################
#retro recruitment no 2009 survey index
SSdir <- "C:/NOAA2013/Hake/Models/retrosNo2009survey"
models <- paste("2013hake_retro",0:12,"_mcmc",sep="")
tmp <- SSgetMCMC(dir=paste(SSdir,models,sep="/"),writecsv=F)
cohorts <- 1999:2010
out <- matrix(NA,ncol=length(-13:13),nrow=length(cohorts))
dimnames(out) <- list(as.character(1999:2010),as.character(-13:13))
for(i in 0:12) {
    yr <- 2013-i
    age <- yr-cohorts-1
    x <- apply(tmp[[i+1]][,paste("Recr_",1999:2010,sep="")],2,median)
    for(a in 1:length(age)) {
        out[a,as.character(age)[a]] <- x[a]
    }
}
age <- 2013-cohorts-1
#x <- apply(base$mcmc[,paste("Recr_",1999:2010,sep="")],2,median)
#for(a in 1:length(age)) {
#    out[a,as.character(age)[a]] <- x[a]
#}
out[,as.character(0:13)]

#by years as columns
cohorts <- 1999:2010
out <- matrix(NA,ncol=length(1999:2013),nrow=length(cohorts))
dimnames(out) <- list(as.character(1999:2010),as.character(1999:2012))
for(i in 1:12) {
    yr <- 2013-i
    x <- apply(tmp[[i]][,paste("Recr_",1999:2010,sep="")],2,median)
    out[,as.character(yr)] <- x
}




















if(F) {
#################################
# Post-SRG, JMC requested catch values

#set up model runs for within model
SSdir <- "C:/NOAA2012/Hake/Models/Post-SRG Decision table runs"
models <- dir(SSdir)
mcmc <- SSgetMCMC(dir=paste(SSdir,models,sep="/"),writecsv=F)

HakeDecisionTablesQuantiles.ex(mcmc,years=2012:2014,outVar="Bratio_",quantiles=c(0.05,0.25,0.5,0.75,0.95),scalar=0.01)
HakeDecisionTablesQuantiles.ex(mcmc,years=2012:2014,outVar="SPRratio_",quantiles=c(0.05,0.25,0.5,0.75,0.95),scalar=0.01)

#probability metrics table with base only
HakeMetricsTable.fn(mcmc,year=2013)

#draw a figure of metrics
metrics <- read.csv("C:/NOAA2012/Hake/Models/Rcode/metrics.csv")
metrics$Catch <- metrics$Catch/1e3
metrics <- metrics[-nrow(metrics),]

windows(width=10,height=7)
plot(metrics$Catch,metrics$P1,ylim=c(0,1),xaxt="n",ylab="Probability",xlab="Catch ('000 mt)",type="b",lty=2,pch=16)
lines(metrics$Catch,metrics$P2,col="blue",type="b",lty=2,pch=17)
lines(metrics$Catch,metrics$P3,col="green",type="b",lty=2,pch=17)
lines(metrics$Catch,metrics$P4,col="orange",type="b",lty=2,pch=17)
lines(metrics$Catch,metrics$P5,col="red",type="b",lty=2,pch=15)
abline(h=0.5,lty=1,lwd=1)
legend("bottomright",c("P(B2013>B2012)","P(B2013>B40%)","P(B2013>B25%)","P(B2013>B10%)","P(FI>Target)"),col=c("black","blue","green","orange","red"),lty=1,lwd=2,pch=c(16,17,17,17,15))
axis(1,at=seq(0,250,25),cex.axis=0.9)



median(x$Bratio_2012)
median(x$Bratio_2013)
median(x$SPB_2012)
median(x$SPB_2013)
sum(x$SPB_2013 > x$SPB_2012)
sum(x$SPB_2013 > x$SPB_2012)/nrow(x)
plot(density(x$SPB_2013),type="l",col="red")
lines(density(x$SPB_2012),type="l",col="green")
abline(v=median(x$SPB_2013),col="red")
abline(v=median(x$SPB_2012),col="green")

plot(density((x$SPB_2013-x$SPB_2012)/x$SPB_2012),type="l")

#set up model runs for between models
csvDir <- "C:/NOAA2012/Hake/WriteUp/csvFiles"

SSdir <- "C:/NOAA2012/Hake/Models/DecisionTableModels/age5 forecast decision runs"
models <- dir(SSdir)
mcmc <- SSgetMCMC(dir=paste(SSdir,models,sep="/"),writecsv=F)
HakeDecisionTablesQuantiles.ex(mcmc,years=2012:2014,outVar="Bratio_",quantiles=c(0.5),scalar=1,csvFileName=paste(csvDir,"DecTable2_SSage5_Bratio.csv",sep="/"))
HakeDecisionTablesQuantiles.ex(mcmc,years=2012:2014,outVar="SPRratio_",quantiles=c(0.5),scalar=1,csvFileName=paste(csvDir,"DecTable2_SSage5_SPRratio.csv",sep="/"))

SSdir <- "C:/NOAA2012/Hake/Models/Base forecast decision runs"
models <- dir(SSdir)
mcmc <- SSgetMCMC(dir=paste(SSdir,models,sep="/"),writecsv=F)
HakeDecisionTablesQuantiles.ex(mcmc,years=2012:2014,outVar="Bratio_",quantiles=c(0.5),scalar=1,csvFileName=paste(csvDir,"DecTable2_SSbase_Bratio.csv",sep="/"))
HakeDecisionTablesQuantiles.ex(mcmc,years=2012:2014,outVar="SPRratio_",quantiles=c(0.5),scalar=1,csvFileName=paste(csvDir,"DecTable2_SSbase_SPRratio.csv",sep="/"))

SSdir <- "C:/NOAA2012/Hake/Models/DecisionTableModels/age7 forecast decision runs"
models <- dir(SSdir)
mcmc <- SSgetMCMC(dir=paste(SSdir,models,sep="/"),writecsv=F)
HakeDecisionTablesQuantiles.ex(mcmc,years=2012:2014,outVar="Bratio_",quantiles=c(0.5),scalar=1,csvFileName=paste(csvDir,"DecTable2_SSage7_Bratio.csv",sep="/"))
HakeDecisionTablesQuantiles.ex(mcmc,years=2012:2014,outVar="SPRratio_",quantiles=c(0.5),scalar=1,csvFileName=paste(csvDir,"DecTable2_SSage7_SPRratio.csv",sep="/"))





#Reference points
SSdir <- "C:/NOAA2012/Hake/Models"
base <- SS_output(dir=paste(SSdir,"81_base_MCMC",sep="/"),covar=F)
tmp <- SSgetMCMC(dir=paste(SSdir,"81_base_MCMC",sep="/"),writecsv=F)
base$mcmc <- data.frame(tmp$model1)

round(quantile(base$mcmc$SSB_Unfished,prob=c(0.025,0.5,0.975))/2e6,3)
round(quantile(base$mcmc$Recr_Virgin,prob=c(0.025,0.5,0.975))/1e6,3)

round(quantile(base$mcmc$SSB_Btgt,prob=c(0.025,0.5,0.975))/2e6,3)
round(100*quantile(base$mcmc$SPR_Btgt,prob=c(0.025,0.5,0.975)),1)
round(100*quantile(base$mcmc$Fstd_Btgt,prob=c(0.025,0.5,0.975)),1)
round(quantile(base$mcmc$TotYield_Btgt,prob=c(0.025,0.5,0.975))/1e6,3)

round(quantile(base$mcmc$SSB_SPRtgt,prob=c(0.025,0.5,0.975))/2e6,3)
round(100*quantile(base$mcmc$Fstd_SPRtgt,prob=c(0.025,0.5,0.975)),1)
round(quantile(base$mcmc$TotYield_SPRtgt,prob=c(0.025,0.5,0.975))/1e6,3)

round(quantile(base$mcmc$SSB_MSY,prob=c(0.025,0.5,0.975))/2e6,3)
round(100*quantile(base$mcmc$SPR_MSY,prob=c(0.025,0.5,0.975)),1)
round(100*quantile(base$mcmc$Fstd_MSY,prob=c(0.025,0.5,0.975)),1)
round(quantile(base$mcmc$TotYield_MSY,prob=c(0.025,0.5,0.975))/1e6,3)


#Reference points
SSdir <- "C:/NOAA2012/Hake/Models"
base <- SS_output(dir=paste(SSdir,"81_base_MCMC",sep="/"),covar=F)
tmp <- SSgetMCMC(dir=paste(SSdir,"81_base_MCMC",sep="/"),writecsv=F)
base$mcmc <- data.frame(tmp$model1)
SSdir <- "C:/NOAA2012/Hake/Models/DecisionTableModels/age5 forecast decision runs"
age5 <- SS_output(dir=paste(SSdir,"82_age5_MCMC_252_default_hcr",sep="/"),covar=F)
tmp <- SSgetMCMC(dir=paste(SSdir,"82_age5_MCMC_252_default_hcr",sep="/"),writecsv=F)
age5$mcmc <- data.frame(tmp$model1)

round(quantile(age5$mcmc$SSB_Unfished,prob=0.5)/2e6,3)
round(quantile(age5$mcmc$Recr_Virgin,prob=0.5)/1e6,3)

round(quantile(age5$mcmc$SSB_Btgt,prob=0.5)/2e6,3)
round(quantile(age5$mcmc$SPR_Btgt,prob=0.5),3)
round(quantile(age5$mcmc$Fstd_Btgt,prob=0.5),3)
round(quantile(age5$mcmc$TotYield_Btgt,prob=0.5)/1e6,3)

round(quantile(age5$mcmc$SSB_SPRtgt,prob=0.5)/2e6,3)
round(quantile(age5$mcmc$Fstd_SPRtgt,prob=0.5),3)
round(quantile(age5$mcmc$TotYield_SPRtgt,prob=0.5)/1e6,3)

round(quantile(age5$mcmc$SSB_MSY,prob=0.5)/2e6,3)
round(quantile(age5$mcmc$SPR_MSY,prob=0.5),3)
round(quantile(age5$mcmc$Fstd_MSY,prob=0.5),3)
round(quantile(age5$mcmc$TotYield_MSY,prob=0.5)/1e6,3)


SSdir <- "C:/NOAA2012/Hake/Models/DecisionTableModels/age7 forecast decision runs"
age7 <- SS_output(dir=paste(SSdir,"83_age7_MCMC_252_default_hcr",sep="/"),covar=F)
tmp <- SSgetMCMC(dir=paste(SSdir,"83_age7_MCMC_252_default_hcr",sep="/"),writecsv=F)
age7$mcmc <- data.frame(tmp$model1)

round(quantile(age7$mcmc$SSB_Unfished,prob=0.5)/2e6,3)
round(quantile(age7$mcmc$Recr_Virgin,prob=0.5)/1e6,3)

round(quantile(age7$mcmc$SSB_Btgt,prob=0.5)/2e6,3)
round(quantile(age7$mcmc$SPR_Btgt,prob=0.5),3)
round(quantile(age7$mcmc$Fstd_Btgt,prob=0.5),3)
round(quantile(age7$mcmc$TotYield_Btgt,prob=0.5)/1e6,3)

round(quantile(age7$mcmc$SSB_SPRtgt,prob=0.5)/2e6,3)
round(quantile(age7$mcmc$Fstd_SPRtgt,prob=0.5),3)
round(quantile(age7$mcmc$TotYield_SPRtgt,prob=0.5)/1e6,3)

round(quantile(age7$mcmc$SSB_MSY,prob=0.5)/2e6,3)
round(quantile(age7$mcmc$SPR_MSY,prob=0.5),3)
round(quantile(age7$mcmc$Fstd_MSY,prob=0.5),3)
round(quantile(age7$mcmc$TotYield_MSY,prob=0.5)/1e6,3)




#########################################################
### MAIN DOCUMENT ###
SSdir <- "C:/NOAA2012/Hake/Models"
base <- SS_output(dir=paste(SSdir,"81_base_MCMC",sep="/"),covar=F)
tmp <- SSgetMCMC(dir=paste(SSdir,"81_base_MCMC",sep="/"),writecsv=F)
base$mcmc <- data.frame(tmp$model1)
baseMLE <- SS_output(dir=paste(SSdir,"81_2012_revised_acoustic_base_mle",sep="/"),covar=T)


tableDir <- "C:\\NOAA2012\\Hake\\WriteUp\\Tables"
totalb.mle <- baseMLE$timeseries$Bio_all[base$timeseries$Yr%in%(1966:2012)]/1e6
expb.mle <- NA
spb.mle <- DerivedQuants.ex(models=list(baseMLE),variable="SPB_",mcmc=c(F),scalar=1e6,years=1966:2012,probs=0.5)
spb.mcmc <- DerivedQuants.ex(models=list(base),variable="SPB_",mcmc=c(T),scalar=1e6,years=1966:2012,probs=0.5)
bratio.mcmc <- c(NA,DerivedQuants.ex(models=list(base),variable="Bratio_",mcmc=c(T),scalar=1,years=1967:2012,probs=0.5))
recr.mcmc <- DerivedQuants.ex(models=list(base),variable="Recr_",mcmc=c(T),scalar=1e6,years=1966:2012,probs=0.5)
spr.mcmc <- DerivedQuants.ex(models=list(base),variable="SPRratio_",mcmc=c(T),scalar=1,years=1966:2012,probs=0.5)
f.mcmc <- DerivedQuants.ex(models=list(base),variable="F_",mcmc=c(T),scalar=1,years=1966:2012,probs=0.5)
x <- data.frame(totalb.mle,expb.mle,spb.mle,spb.mcmc,bratio.mcmc,recr.mcmc,spr.mcmc,f.mcmc)
names(x) <- c("totalb.mle","expb.mle","spb.mle","spb.mcmc","bratio.mcmc","recr.mcmc","spr.mcmc","f.mcmc")
write.csv(x,file=paste(tableDir,"timeSeries.csv",sep="/"))

x.fn <- function(x,d=2) {paste(round(x[,1],d),round(x[,2],d),sep="-")}
spb.mcmc <- DerivedQuants.ex(models=list(base),variable="SPB_",mcmc=c(T),scalar=1e6,years=1966:2012,probs=c(0.025,0.975))
bratio.mcmc <- rbind(c(NA,NA),DerivedQuants.ex(models=list(base),variable="Bratio_",mcmc=c(T),scalar=1,years=1967:2012,probs=c(0.025,0.975)))
recr.mcmc <- DerivedQuants.ex(models=list(base),variable="Recr_",mcmc=c(T),scalar=1e6,years=1966:2012,probs=c(0.025,0.975))
spr.mcmc <- DerivedQuants.ex(models=list(base),variable="SPRratio_",mcmc=c(T),scalar=1,years=1966:2012,probs=c(0.025,0.975))
f.mcmc <- DerivedQuants.ex(models=list(base),variable="F_",mcmc=c(T),scalar=1,years=1966:2012,probs=c(0.025,0.975))
x <- data.frame(x.fn(spb.mcmc),x.fn(bratio.mcmc),x.fn(recr.mcmc),x.fn(spr.mcmc),x.fn(f.mcmc))
names(x) <- c("spb.mcmc","bratio.mcmc","recr.mcmc","spr.mcmc","f.mcmc")
write.csv(x,file=paste(tableDir,"timeSeriesInterval.csv",sep="/"))

x <- base$natage
x <- x[x[,"Beg/Mid"]=="B" & x$Yr%in%1966:2011,as.character(0:20)]
x[,"15"] <- apply(x[,as.character(15:20)],1,sum)
x <- x[,as.character(0:15)]/1e6
row.names(x) <- as.character(1966:2011)
write.csv(x,file=paste(tableDir,"numAtAge.csv",sep="/"))



baseMLE$likelihoods_used

exp(baseMLE$parameters[baseMLE$parameters$Label=="SR_LN(R0)","Value"])/1e6
baseMLE$parameters[baseMLE$parameters$Label=="SR_BH_steep","Value"]
baseMLE$parameters[baseMLE$parameters$Label=="NatM_p_1_Fem_GP_1","Value"]
Q
baseMLE$parameters[baseMLE$parameters$Label=="Q_extraSD_2_Acoustic_Survey","Value"]
DerivedQuants.ex(models=list(baseMLE),variable="SPB_",mcmc=c(F),scalar=1e6,years=2012,probs=0.5)
DerivedQuants.ex(models=list(baseMLE),variable="Bratio_",mcmc=c(F),scalar=0.01,years=2012,probs=0.5)
DerivedQuants.ex(models=list(baseMLE),variable="SPRratio_",mcmc=c(F),scalar=0.01,years=2011,probs=0.5)

exp(quantile(base$mcmc$SR_LN.R0.,prob=c(0.5)))/1e6
quantile(base$mcmc$SR_BH_steep,prob=c(0.5))
quantile(base$mcmc$NatM_p_1_Fem_GP_1,prob=c(0.5))
Q
quantile(base$mcmc$Q_extraSD_2_Acoustic_Survey,prob=c(0.5))
quantile(base$mcmc$SPB_2012,prob=c(0.5))/2e6
quantile(base$mcmc$Bratio_2012,prob=c(0.5))*100
quantile(base$mcmc$SPRratio_2011,prob=c(0.5))*100




SSdir <- "C:/NOAA2012/Hake/Models/DecisionTableModels/age5 forecast decision runs"
age5 <- SS_output(dir=paste(SSdir,"82_age5_MCMC_252_default_hcr",sep="/"),covar=F)
dirs <- paste(SSdir,"82_age5_MCMC_252_default_hcr",sep="/")
SSdir <- "C:/NOAA2012/Hake/Models"
ss <- SS_output(dir=paste(SSdir,"81_2012_revised_acoustic_base_mle",sep="/"))  #read non-mcmc file
dirs <- c(dirs,paste(SSdir,"81_base_MCMC",sep="/"))
SSdir <- "C:/NOAA2012/Hake/Models/DecisionTableModels/age7 forecast decision runs"
age7 <- SS_output(dir=paste(SSdir,"83_age7_MCMC_252_default_hcr",sep="/"),covar=F)
dirs <- c(dirs,paste(SSdir,"83_age7_MCMC_252_default_hcr",sep="/"))
tmp <- SSgetMCMC(dir=dirs,writecsv=F)
#create the list of models and add mcmc results manually below
mymodels <- list(age5,ss,age7)
models <- SSsummarize(mymodels)
models$mcmc <- vector(mode="list",length=length(mymodels))  #create the mcmc list of model dataframes
models$mcmc <- tmp
modelnames <- c("age5","SS","age7")

tdir <- "C:/NOAA2012/Hake/WriteUp/Tables"
SStableComparisons(models,mcmc=T,modelnames=modelnames,
    names=c("Recr_Virgin","SR_BH_steep","NatM_p_1_Fem_GP_1","Q_extraSD_2_Acoustic_Survey",
    "Recr_2008","SPB_Virgin","Bratio_2012","SPRratio_2011",
    "SSB_Btgt","SPR_Btgt","Fstd_Btgt","TotYield_Btgt",
    "SSB_SPRtgt","Fstd_SPRtgt","TotYield_SPRtgt",
    "SSB_MSY","SPR_MSY","Fstd_MSY","TotYield_MSY"),
    csv=TRUE,
    csvdir=tdir,
    csvfile="top5_sensTable.csv"
)


mymodels <- list(ss)
models <- SSsummarize(mymodels)
models$mcmc <- vector(mode="list",length=length(mymodels))  #create the mcmc list of model dataframes
models$mcmc <- tmp
modelnames <- c("SS")
SStableComparisons(models,modelnames=modelnames,
    names=c("Recr_Virgin","SR_BH_steep","NatM_p_1_Fem_GP_1","Q",
    "Recr_2008","SPB_Virgin","Bratio_2012","SPRratio_2011",
    "SSB_Btgt","SPR_Btgt","Fstd_Btgt","TotYield_Btgt",
    "SSB_SPRtgt","Fstd_SPRtgt","TotYield_SPRtgt",
    "SSB_MSY","SPR_MSY","Fstd_MSY","TotYield_MSY"),
    csv=TRUE,
    csvdir=tdir,
    csvfile="mleBaseTable.csv"
)





SSdir <- "C:/NOAA2012/Hake/Models"
ss <- SS_output(dir=paste(SSdir,"58_2012_hake_base",sep="/"))  #read non-mcmc file
dirs <- paste(SSdir,"58_MCMC_default",sep="/")
SSdir <- "C:/NOAA2012/Hake/Models/sensitivity"
mprior0.2 <- SS_output(dir=paste(SSdir,"61_MCMC_sens_mprior0.2",sep="/"),covar=F)
dirs <- c(dirs,paste(SSdir,"61_MCMC_sens_mprior0.2",sep="/"))
mprior0.3 <- SS_output(dir=paste(SSdir,"67_MCMC_sens_mprior0.3",sep="/"),covar=F)
dirs <- c(dirs,paste(SSdir,"67_MCMC_sens_mprior0.3",sep="/"))
sigmaprior <- SS_output(dir=paste(SSdir,"68_MCMC_sens_sigmarprior0.1",sep="/"),covar=F)
dirs <- c(dirs,paste(SSdir,"68_MCMC_sens_sigmarprior0.1",sep="/"))
tmp <- SSgetMCMC(dir=dirs,writecsv=F)
#create the list of models and add mcmc results manually below
mymodels <- list(ss,mprior0.2,mprior0.3,sigmaprior)
models <- SSsummarize(mymodels)
models$mcmc <- vector(mode="list",length=length(mymodels))  #create the mcmc list of model dataframes
models$mcmc <- tmp
modelnames <- c("base","mprior0.2","mprior0.3","sigmaprior")

tdir <- "C:/NOAA2012/Hake/WriteUp/Tables"
SStableComparisons(models,mcmc=T,modelnames=modelnames,
    names=c("Recr_Virgin","SR_BH_steep","NatM_p_1_Fem_GP_1","Q_extraSD_2_Acoustic_Survey",
    "Recr_2008","SPB_Virgin","Bratio_2012","SPRratio_2011",
    "SSB_Btgt","SPR_Btgt","Fstd_Btgt","TotYield_Btgt",
    "SSB_SPRtgt","Fstd_SPRtgt","TotYield_SPRtgt",
    "SSB_MSY","SPR_MSY","Fstd_MSY","TotYield_MSY"),
    csv=TRUE,
    csvdir=tdir,
    csvfile="prior_sensTable.csv"
)




SSdir <- "C:/NOAA2012/Hake/Models"
ss <- SS_output(dir=paste(SSdir,"58_2012_hake_base",sep="/"))  #read non-mcmc file
dirs <- paste(SSdir,"58_MCMC_default",sep="/")
SSdir <- "C:/NOAA2012/Hake/Models/retrospective"
retro1 <- SS_output(dir=paste(SSdir,"62_MCMC_retro1",sep="/"),covar=F)
dirs <- c(dirs,paste(SSdir,"62_MCMC_retro1",sep="/"))
retro2 <- SS_output(dir=paste(SSdir,"63_MCMC_retro2",sep="/"),covar=F)
dirs <- c(dirs,paste(SSdir,"63_MCMC_retro2",sep="/"))
retro3 <- SS_output(dir=paste(SSdir,"64_MCMC_retro3",sep="/"),covar=F)
dirs <- c(dirs,paste(SSdir,"64_MCMC_retro3",sep="/"))
retro4 <- SS_output(dir=paste(SSdir,"65_MCMC_retro4",sep="/"),covar=F)
dirs <- c(dirs,paste(SSdir,"65_MCMC_retro4",sep="/"))
retro5 <- SS_output(dir=paste(SSdir,"66_MCMC_retro5",sep="/"),covar=F)
dirs <- c(dirs,paste(SSdir,"66_MCMC_retro5",sep="/"))
tmp <- SSgetMCMC(dir=dirs,writecsv=F)
#create the list of models and add mcmc results manually below
mymodels <- list(ss,retro1,retro2,retro3,retro4,retro5)
models <- SSsummarize(mymodels)
models$mcmc <- vector(mode="list",length=length(mymodels))  #create the mcmc list of model dataframes
models$mcmc <- tmp
modelnames <- c("base","retro1","retro2","retro3","retro4","retro5")

tdir <- "C:/NOAA2012/Hake/WriteUp/Tables"
SStableComparisons(models,mcmc=T,modelnames=modelnames,
    names=c("Recr_Virgin","SR_BH_steep","NatM_p_1_Fem_GP_1","Q_extraSD_2_Acoustic_Survey",
    "Recr_2008","SPB_Virgin","Bratio_2012","SPRratio_2011",
    "SSB_Btgt","SPR_Btgt","Fstd_Btgt","TotYield_Btgt",
    "SSB_SPRtgt","Fstd_SPRtgt","TotYield_SPRtgt",
    "SSB_MSY","SPR_MSY","Fstd_MSY","TotYield_MSY"),
    csv=TRUE,
    csvdir=tdir,
    csvfile="retroTable.csv"
)






origDir <- getwd()
setwd("C:/iSCAM/iscam-project/src/R_pkg")
source("iscam.R")
setwd(origDir)

ccamDir <- "C:/NOAA2012/Hake/Models/CCAM/2012_CCAM_post_SRG/ccam_Base_Msd_005_30M"

#read in values from CCAM model and put into appropriate format
ifile <- paste(ccamDir,"/iscam",sep="")
repFile <- paste(ifile,".rep",sep="")
psvFile <- paste(ifile,".psv",sep="")

A <- read.rep(repFile)
age <- 1:15
burnin <- 1000
thin    <-  1       #thinning interval for mcmc samples

A$recYrs <- (min(A$yr)-max(age)+1):max(A$yr)
A$mcmc <- read.table(paste(ccamDir,"iscam.mcmc",sep="/"),header=T)
A$fit <- read.fit(paste(ccamDir,"iscam",sep="/"))
A$mc.sbt <- read.table(paste(ccamDir,"sbt.mcmc",sep="/"),h=F) * 2e6
A$mc.rt <- read.table(paste(ccamDir,"rt.mcmc",sep="/"),h=F) * 1e6
A$mc.spr <- read.table(paste(ccamDir,"spr40_status.mcmc",sep="/"),h=F)
A$mc.sb0 <- A$mcmc$bo * 2e6
#ADD in 2013 and 2014 from forecast file (-777 is fixed SS base catches)
tmp <- read.table(paste(ccamDir,"iscam_forecast_mcmc.rep",sep="/"),h=T)
tmp <- tmp[tmp$CtStream== -777,]
tmp <- as.data.frame(split(tmp$depletion,tmp$Year))
A$mc.depl <- read.table(paste(ccamDir,"sbtdepletion.mcmc",sep="/"),h=F)
A$mc.depl <- cbind(A$mc.depl,tmp[,-1])
#I'm not certain if these values are comparable to SS
A$mc.logR0 <- A$mcmc$log.ro
A$mc.MSY <- A$mcmc$msy

#Set up MLE CCAM results (I have not completely checked this)
ccam1 <- list(npar=A$fit$nopar,maxgrad=A$MaxGrad,nsexes=1,
                SpawnBio=data.frame(c(1964,1965,A$yrs),c(A$sbo,A$sbo,A$sbt)*1e6,0,qnorm(0.025,c(A$so,A$so,A$sbt)*1e6,0),qnorm(0.975,c(A$so,A$so,A$sbt)*1e6,0)),     #there is an estimated parameter called sd_sbt, but it is a single value
                Bratio=data.frame(A$yrs,A$sbt/A$sbo,0,qnorm(0.025,A$sbt/A$sbo,0),qnorm(0.975,A$sbt/A$sbo,0)),
                SPRratio=data.frame(A$yr,A$spr,0,qnorm(0.025,A$spr,0),qnorm(0.975,A$spr,0)),
                recruits=data.frame(A$yr[-1],A$rt*1e6,0,qnorm(0.025,A$rt*1e5,0),qnorm(0.975,A$rt*1e6,0)),   #NEED TO GET REAL VALUES
                recdevs=data.frame(A$yr[-1],A$rt*1e6,0,qnorm(0.025,A$rt*1e5,0),qnorm(0.975,A$rt*1e6,0)),
                indices = data.frame(A$iyr,1e6*A$it,1e6*A$pit,rep(A$q,length(A$iyr)),rep(0.4,length(A$iyr)),rep(0,length(A$iyr)),rep(1,length(A$iyr)))
            )
mcmcInd <- seq(burnin+1,nrow(A$mc.sbt),thin)
ccam1$mcmc <- data.frame(A$mc.sb0[mcmcInd],A$mc.sbt[mcmcInd,],A$mc.depl[mcmcInd,],A$mc.spr[mcmcInd,],A$mc.rt[mcmcInd,],log(A$mc.logR0[mcmcInd]),A$mc.MSY[mcmcInd])
names(ccam1$mcmc) <- c("SPB_Virgin",paste("SPB",A$yrs,sep="_"),paste("Bratio",c(A$yrs,2013:2014),sep="_"),paste("SPRratio",A$yr,sep="_"),paste("Recr",A$yr[-length(A$yr)],sep="_"),"SR_LN(R0)","TotYield_MSY")

median(A$mcmc$h[mcmcInd])
median(exp(A$mcmc$log.m[mcmcInd]))
median(A$mcmc$q[mcmcInd])
median(ccam1$mcmc$Recr_2008)
median(ccam1$mcmc$Bratio_2012)
median(ccam1$mcmc$SPRratio_2011)

#####
ccamDir <- "C:/NOAA2012/Hake/Models/CCAM/2012_CCAM_post_SRG/ccam_steep_surv_sel_30M"
#read in values from CCAM model and put into appropriate format
ifile <- paste(ccamDir,"/iscam",sep="")
repFile <- paste(ifile,".rep",sep="")
psvFile <- paste(ifile,".psv",sep="")

A <- read.rep(repFile)
age <- 1:15
burnin <- 1000
thin    <-  1       #thinning interval for mcmc samples

A$recYrs <- (min(A$yr)-max(age)+1):max(A$yr)
A$mcmc <- read.table(paste(ccamDir,"iscam.mcmc",sep="/"),header=T)
A$fit <- read.fit(paste(ccamDir,"iscam",sep="/"))
A$mc.sbt <- read.table(paste(ccamDir,"sbt.mcmc",sep="/"),h=F) * 2e6
A$mc.rt <- read.table(paste(ccamDir,"rt.mcmc",sep="/"),h=F) * 1e6
A$mc.spr <- read.table(paste(ccamDir,"spr40_status.mcmc",sep="/"),h=F)
A$mc.sb0 <- A$mcmc$bo * 2e6
#ADD in 2013 and 2014 from forecast file (-777 is fixed SS base catches)
tmp <- read.table(paste(ccamDir,"iscam_forecast_mcmc.rep",sep="/"),h=T)
tmp <- tmp[tmp$CtStream== -777,]
tmp <- as.data.frame(split(tmp$depletion,tmp$Year))
A$mc.depl <- read.table(paste(ccamDir,"sbtdepletion.mcmc",sep="/"),h=F)
A$mc.depl <- cbind(A$mc.depl,tmp[,-1])
#I'm not certain if these values are comparable to SS
A$mc.logR0 <- A$mcmc$log.ro
A$mc.MSY <- A$mcmc$msy

#Set up MLE CCAM results (I have not completely checked this)
ccam2 <- list(npar=A$fit$nopar,maxgrad=A$MaxGrad,nsexes=1,
                SpawnBio=data.frame(c(1964,1965,A$yrs),c(A$sbo,A$sbo,A$sbt)*1e6,0,qnorm(0.025,c(A$so,A$so,A$sbt)*1e6,0),qnorm(0.975,c(A$so,A$so,A$sbt)*1e6,0)),     #there is an estimated parameter called sd_sbt, but it is a single value
                Bratio=data.frame(A$yrs,A$sbt/A$sbo,0,qnorm(0.025,A$sbt/A$sbo,0),qnorm(0.975,A$sbt/A$sbo,0)),
                SPRratio=data.frame(A$yr,A$spr,0,qnorm(0.025,A$spr,0),qnorm(0.975,A$spr,0)),
                recruits=data.frame(A$yr[-1],A$rt*1e6,0,qnorm(0.025,A$rt*1e5,0),qnorm(0.975,A$rt*1e6,0)),   #NEED TO GET REAL VALUES
                recdevs=data.frame(A$yr[-1],A$rt*1e6,0,qnorm(0.025,A$rt*1e5,0),qnorm(0.975,A$rt*1e6,0)),
                indices = data.frame(A$iyr,1e6*A$it,1e6*A$pit,rep(A$q,length(A$iyr)),rep(0.4,length(A$iyr)),rep(0,length(A$iyr)),rep(1,length(A$iyr)))
            )
mcmcInd <- seq(burnin+1,nrow(A$mc.sbt),thin)
ccam2$mcmc <- data.frame(A$mc.sb0[mcmcInd],A$mc.sbt[mcmcInd,],A$mc.depl[mcmcInd,],A$mc.spr[mcmcInd,],A$mc.rt[mcmcInd,],log(A$mc.logR0[mcmcInd]),A$mc.MSY[mcmcInd])
names(ccam2$mcmc) <- c("SPB_Virgin",paste("SPB",A$yrs,sep="_"),paste("Bratio",c(A$yrs,2013:2014),sep="_"),paste("SPRratio",A$yr,sep="_"),paste("Recr",A$yr[-length(A$yr)],sep="_"),"SR_LN(R0)","TotYield_MSY")



median(A$mcmc$h[mcmcInd])
median(exp(A$mcmc$log.m[mcmcInd]))
median(A$mcmc$q[mcmcInd])
median(ccam2$mcmc$Recr_2008)
median(ccam2$mcmc$Bratio_2012)
median(ccam2$mcmc$SPRratio_2011)





















[,c("SPB_Virgin",paste("SPB",yrs,sep="_"),
                                    paste("Bratio",yrs[-1],sep="_"),
                                    paste("Recr",yrs,sep="_"),
                                    paste("F",yrs,sep="_"),
                                    paste("SPRratio",yrs,sep="_"),
                                    paste("ForeCatch",yrs[-(1:46)],sep="_"))])











#MOCK SETUP OF TINSS
#TINSS <- NULL
#TINSS$nsexes <- 1
#TINSS$mcmc <- as.data.frame(matrix(0,nrow=1000,ncol=length(names(EA$mcmc))))
#names(TINSS$mcmc) <- names(EA$mcmc)
#TINSS$timeseries <- matrix(c(2000:2010,rep(-1,length(2000:2010))),ncol=2,dimnames=list(NULL,c("Yr","Hrate:_1")))


##############
#Derived quantities tables
DerivedQuantsTables.ex(models=list(base,base),mcmc=c(T,T),scalar=c(1e6,1))   #it does spawning biomass (SPB_) by default
DerivedQuantsTables.ex(models=list(EA,TINSS),variable="Bratio_",mcmc=c(T,T))
DerivedQuantsTables.ex(models=list(EA,TINSS),variable="Recr_",mcmc=c(T,T),scalar=c(1e6,1))
DerivedQuantsTables.ex(models=list(EA,TINSS),variable="F_",years=2001:2010,mcmc=c(T,T),scalar=c(1,1))
DerivedQuantsTables.ex(models=list(EA,TINSS),variable="SPRratio_",years=2001:2010,mcmc=c(T,T))

tableDir <- "C:\\NOAA2011\\Hake\\WriteUp\\Tables"
DerivedQuantsTables.ex(models=list(EA,TINSS),mcmc=c(T,T),scalar=c(1e6,1),csvFileName=paste(tableDir,"Table.b_SPB",sep="\\"))
DerivedQuantsTables.ex(models=list(EA,TINSS),variable="Recr_",mcmc=c(T,T),scalar=c(1e6,1),csvFileName=paste(tableDir,"Table.c_Recr",sep="\\"))  #check the scalar!
DerivedQuantsTables.ex(models=list(EA,TINSS),variable="Bratio_",mcmc=c(T,T),csvFileName=paste(tableDir,"Table.d_depl",sep="\\"))
DerivedQuantsTables.ex(models=list(EA,TINSS),variable="F_",years=2001:2010,mcmc=c(T,T),csvFileName=paste(tableDir,"Table.f_ExpRate",sep="\\"))
DerivedQuantsTables.ex(models=list(EA,TINSS),variable="SPRratio_",years=2001:2010,mcmc=c(T,T),csvFileName=paste(tableDir,"Table.e_SPRratio",sep="\\"))


#################################
# Decision tables

#set up model runs
SSdir <- "C:/NOAA2012/Hake/Models/DecisionTable"
models <- dir(SSdir)
mcmc <- SSgetMCMC(dir=paste(SSdir,models,sep="/"),writecsv=F)

HakeDecisionTables.ex(mcmc,years=2012:2014,sortVar="Bratio_2012",outVar="Bratio_",percentages=c(0.1,0.2,0.4,0.2,0.1))
HakeDecisionTables.ex(mcmc,years=2012:2014,sortVar="Bratio_2012",outVar="SPRratio_",percentages=c(0.1,0.2,0.4,0.2,0.1))


HakeDecisionTables.ex(mcmc,years=2011:2013,sortVar="Recr_2008",outVar="Bratio_",percentages=c(0.25,0.5,0.25))
HakeDecisionTables.ex(mcmc,years=2011:2013,sortVar="Recr_2008",outVar="SPRratio_",percentages=c(0.25,0.5,0.25))

tableDir <- "C:\\NOAA2011\\Hake\\WriteUp\\Tables"
tableDir <- "C:\\NOAA2011\\Hake\\STAR\\Tables"
HakeDecisionTables.ex(mcmc,years=2011:2013,sortVar="Recr_2008",outVar="SPB_",percentages=c(0.25,0.5,0.25),scalar=2e6,csvFileName=paste(tableDir,"Table.h1_DecSPB",sep="\\"))
HakeDecisionTables.ex(mcmc,years=2011:2013,sortVar="Recr_2008",outVar="Bratio_",percentages=c(0.25,0.5,0.25),csvFileName=paste(tableDir,"Table.h2_DecDepl",sep="\\"))
HakeDecisionTables.ex(mcmc,years=2011:2013,sortVar="Recr_2008",outVar="SPRratio_",percentages=c(0.25,0.5,0.25),csvFileName=paste(tableDir,"Table.h3_DecSPR",sep="\\"))


HakeDecisionTables.ex(mcmc,years=2011:2013,sortVar="Recr_2008",outVar="Bratio_",percentages=c(0,0,1))


i <- 1
tmp <- order(mcmc[[i]][,"Recr_2008"])[1:249]
mcmc[[i]][1,"ForeCatch_2013"]
median(mcmc[[i]][tmp,"Bratio_2011"])
median(mcmc[[i]][tmp,"Bratio_2012"])
median(mcmc[[i]][tmp,"Bratio_2013"])

median(mcmc[[i]][,"Bratio_2011"])




SSdir <- "C:/NOAA2012/Hake/Models"
base <- SS_output(dir=paste(SSdir,"81_base_MCMC",sep="/"),covar=F)
tmp <- SSgetMCMC(dir=paste(SSdir,"81_base_MCMC",sep="/"),writecsv=F)
base$mcmc <- data.frame(tmp$model1)

head(base$mcmc)
x <- read.table(paste(SSdir,"81_base_MCMC","posteriors.sso",sep="/"),header=T)
cbind(apply(x,2,median))

}











