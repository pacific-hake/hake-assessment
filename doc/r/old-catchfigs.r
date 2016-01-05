#######################################
#Catch plot
catches <- read.csv("2015HakeCatches.csv")
catches[is.na(catches)] <- 0
catch <- catches[,c("CAN_forgn","CAN_JV","CAN_Shoreside","CAN_FreezeTrawl","US_foreign","US_JV","atSea_US_MS","atSea_US_CP","US_shore")]
cols <- c(         rgb(0,0.8,0),rgb(0,0.6,0),rgb(0.8,0,0),rgb(0.4,0,0),      rgb(0,0.2,0),rgb(0,0.4,0),rgb(0,0,0.7),rgb(0,0,0.4),rgb(0,0,1))
#catch <- catches[,c("US_foreign","CAN_forgn","US_JV","CAN_JV","CAN_Shoreside","CAN_FreezeTrawl","atSea_US_MS","atSea_US_CP","US_shore")]
#cols<- c(rgb(70,130,180,maxColorValue = 255),rgb(128,0,0,maxColorValue = 255),rgb(218,165,32,maxColorValue = 255),rgb(107,142,35,maxColorValue = 255),rgb(0,0,205,maxColorValue = 255),rgb(72,61,139,maxColorValue = 255),rgb(200,200,220,maxColorValue = 255),rgb(0.5,0.5,0.5))

legOrder <- c(6,5,2,1,4,3,NA,NA,9,8,7)  #c(9,8,7,NA,NA,4,3,6,5,2,1)

ht <- 4; wd<- 6.5
if(doPNG) {png("Figures/catches.png",height=ht,width=wd,pointsize=10,units="in",res=300)}
if(!doPNG) {windows(height=ht,width=wd)}
par(las=1,mar=c(4, 4, 6, 2) + 0.1,cex.axis=0.9)
tmp <- barplot(t(as.matrix(catch))/1000,beside=F,names=catches[,1],
        col=cols,xlab="Year",ylab="",cex.lab=1,xaxs="i",mgp=c(2.2,1,0))
axis(1,at=tmp,label=F,line=-0.12)
grid(NA,NULL,lty=1,lwd = 1)
mtext("Catch ('000 mt)",side=2,line=2.8,las=0,cex=1.3)
barplot(t(as.matrix(catch))/1000,beside=F,names=catches[,1],
        #legend.text=c("Canadian Foreign","Canadian Joint-Venture","Canadian Shoreside","Canadian Freezer Trawl",
       # 	          "U.S. Foreign","U.S. Joint-Venture","U.S. MS","U.S. CP","U.S. Shore-based")[legOrder],
        col=cols,
        #args.legend=list(x=60.5,y=430,bg="white",horiz=F,xpd=NA,cex=0.83,ncol=3,col=cols[legOrder]),
        xlab="Year",ylab="",cex.lab=1,xaxs="i",add=T,mgp=c(2.2,1,0))
legend(x=0,y=510,
	   c("Canadian Foreign","Canadian Joint-Venture","Canadian Shoreside","Canadian Freezer Trawl",
        	          "U.S. Foreign","U.S. Joint-Venture","U.S. MS","U.S. CP","U.S. Shore-based")[legOrder],
	   bg="white",horiz=F,xpd=NA,cex=1,ncol=3,fill=cols[legOrder],border=cols[legOrder],bty="n")
if(doPNG){dev.off()}
#run barplot a second time to overwrite grid lines




########################################################################################
## Fishing depth distribution

setwd("C:/NOAA2015/Hake/Data")
library(RODBC)
source("Rcode/functions/Functions.R")
source("Rcode/functions/processNorpacCatch.R")
options(digits=19)  #set this so that the full haul join number is displayed
source("Rcode/box95.R")

if(F) {
    ncatch <- queryDB(queryFilename="NORPAC.domestic.catch.detailed",db="NORPAC",uid="hicksa",start=2008,end=2014,querydir="sql/")
    save(ncatch,file=paste("extractedData/NORPACdomesticCatchDetailed.Rdat",sep=""))
    file.copy("extractedData/NORPACdomesticCatchDetailed.Rdat",paste("extractedData/NORPACdomesticCatchDetailed_",format(Sys.time(),"%Y.%m.%d"),".Rdat",sep=""),overwrite=T)
    #ncatch2 <- queryDB(queryFilename="NORPAC.domestic.catch.detailed2",db="NORPAC",uid="hicksa",start=2008,end=2014,querydir="sql/")
    #ncatch3 <- queryDB(queryFilename="testHaul",db="NORPAC",uid="hicksa",start=2014,end=2014,querydir="sql/")
}
setwd("C:/NOAA2015/Hake/Data")
source("Rcode/functions/processNorpacCatch.R")
load("extractedData/NORPACdomesticCatchDetailed.Rdat")
source("Rcode\\box95.R")

ncatch <- ncatch[ncatch$SPECIES==206,]  #only hauls that caught hake (and removes duplicate haul entries for other species)

ncatch$Year <- as.numeric(substring(ncatch$HAUL_DATE,1,4))
ncatch$Month <- as.numeric(substring(ncatch$HAUL_DATE,6,7))
catch.yr <- split(ncatch,ncatch$Year)
#Be careful that these deep depths are not confidential
# boxplot(lapply(catch.yr,function(x)x$FISHING_DEPTH_FATHOMS),main="Fishing Depth (Fathoms)")
# boxplot(lapply(catch.yr,function(x)x$BOTTOM_DEPTH_FATHOMS),main="Bottom Depth (Fathoms)")
# axis(2,at=seq(100,400,100))

# ncatch$VESSEL_TYPE==1 is CP
# ncatch$VESSEL_TYPE==2 is MS
table(catch.yr[["2014"]][catch.yr[["2014"]]$BOTTOM_DEPTH_FATHOMS>900,"VESSEL_TYPE"],
	catch.yr[["2014"]][catch.yr[["2014"]]$BOTTOM_DEPTH_FATHOMS>900,"Month"])



par(mar=c(5,5,2,1))
out <- box95(lapply(catch.yr,function(x)x$BOTTOM_DEPTH_FATHOMS),main="At-sea sectors: Bottom Depth",las=1,xlab="Year",ylab="Bottom Depth (fathoms)",ylim=c(0,1650))
axis(2,at=seq(100,400,100),las=1)

par(mar=c(5,5,2,1))
out <- box95(lapply(catch.yr,function(x)x$FISHING_DEPTH_FATHOMS),main="At-sea sectors: Fishing Depth",las=1,xlab="Year",ylab="Fishing Depth (Fathoms)",ylim=c(0,1650))
axis(2,at=seq(100,400,100),las=1)


#check for confidentiality
table(catch.yr[["2014"]][catch.yr[["2014"]]$BOTTOM_DEPTH_FATHOMS>1650,"VESSEL"])





##Shoreside fleet
load("C:\\NOAA2015\\Hake\\Data\\Logbook\\PacFIN.Logbook.PWHT.08.Jan.2015.dmp")

dat <- PacFIN.Logbook.PWHT.08.Jan.2015
dat$Date <- as.Date(dat$DDATE,"%d-%b-%Y")
dat$Year <- as.numeric(substr(dat$Date,1,4))
dat$Month <- as.numeric(substr(dat$Date,6,7))

table(dat$DEPTH_TYPE1)

tmp <- dat[dat$Year==2014,]
plot(tmp$SET_LONG,tmp$SET_LAT)
points(catch.yr[["2014"]][catch.yr[["2014"]]$BOTTOM_DEPTH_FATHOMS>400,"RETRV_LONGITUDE"],
	   catch.yr[["2014"]][catch.yr[["2014"]]$BOTTOM_DEPTH_FATHOMS>400,"RETRV_LATITUDE"],
	   pch=16,col="red")

depth.yr <- split(dat$DEPTH1[dat$Year>=2008],dat$Year[dat$Year>=2008])
#boxplot(depth.yr)
out <- box95(depth.yr,main="Shoreside Bottom? Depth by year (fathoms?)")
#check for confidentiality
ind <- dat$DEPTH1>450
table(dat$DRVID[ind],dat$Year[ind])





#compare where sectors fished
library(maptools)
library(PBSmapping)
library(RgoogleMaps)
setwd("C:/NOAA2015/Hake/Data")

ncatch$LAT <- floor(ncatch$RETRV_LATITUDE/100) + 100*(ncatch$RETRV_LATITUDE/100-floor(ncatch$RETRV_LATITUDE/100))/60
ncatch$LON <- -1*(floor(ncatch$RETRV_LONGITUDE/100) + 100*(ncatch$RETRV_LONGITUDE/100-floor(ncatch$RETRV_LONGITUDE/100))/60)

#plot(atsea.bio$LONDD_START,atsea.bio$LATDD_START,pch=16)
#plot(ncatch$LON,ncatch$LAT,pch=16)
atsea <- ncatch[ncatch$Year==2014,]

bb <- qbbox(lat=c(41.4,48.6), lon=-c(126,124), TYPE = "all",margin=list(m=c(1,1,1,1),TYPE = c("perc", "abs")[1]))
siteTerrain <- GetMap.bbox(lonR=bb$lonR,latR=bb$latR, size = c(320, 640), destfile = "Figures/RgoogleMap.png",
                           MINIMUMSIZE = FALSE, RETURNIMAGE = TRUE, GRAYSCALE = FALSE,
                           NEWMAP = TRUE, verbose = 1, SCALE = 2,maptype="hybrid")#,frame=10,zoom=6)  #,markers=mymarkers)  #zoom is another argument

#confidential plot
#PlotOnStaticMap(siteTerrain,lat=atsea$LAT,lon=atsea$LON,add=FALSE,col="red",pch=16)
#PlotOnStaticMap(siteTerrain,lat=tmp$SET_LAT,lon=tmp$SET_LONG,add=TRUE,col=rgb(0,1,0,0.5),pch=15)
