#Create the overview map for the hake assessment
#save it as an eps file for the LateX document


# load more packages and get mapping data only if map will be updated
devtools::install_github("nwfsc-assess/nwfscMapping")
library(maps)
library(nwfscMapping)
library(PBSmapping)
data(westCoastLL)
data(WCstatesInlandPBS)

source("C:/Mapping/WestCoastMapping.R")
LMEoffshore <- importShapefile("C:/Mapping/Shapefiles/LME66_Offshore/LME66_Offshore.shp",readDBF=T)
LME <- importShapefile("C:/Mapping/Shapefiles/LME66/LME66.shp",readDBF=T)
province <- importShapefile("C:/Mapping/Shapefiles/province/province.shp",readDBF=T)
alberta <- attributes(province)$PolyData[attributes(province)$PolyData$NAME == "Alberta","PID"]
CCLME <- attributes(LME)$PolyData[attributes(LME)$PolyData$LME_NAME == "California Current","PID"]
GOALME <- attributes(LME)$PolyData[attributes(LME)$PolyData$LME_NAME == "Gulf of Alaska","PID"]
data(nepacLL)

figDir <- "C:/NOAA2016/Hake/WriteUp/Figures"

##########################################################################
## Map of area
portLats <- read.csv("C:/Mapping/Data/portLats.csv")
theCities <- c("Newport","Westport","Astoria","Eureka","Charleston (Coos Bay)")
theCities <- portLats[portLats$Name%in%theCities,]

doPNG <- F
doTIFF <- F
doEPS <- T


ht <- 10; wd<- 6.5
if(doPNG) {png(filename = paste(figDir,"overviewMap.png",sep="\\"), width = wd, height = ht,units="in",res=300, pointsize = 11)}
if(doTIFF) {tiff(filename = paste(figDir,"overviewMap.tiff",sep="\\"), width = wd, height = ht,units="in",res=300, pointsize = 11)}
if(doEPS) {postscript(file = paste(figDir,"overviewMap.eps",sep="\\"), width = wd, height = ht, pointsize=11)}
if(!doPNG & !doTIFF & !doEPS) {windows(height=ht,width=wd)}
par(mfrow=c(1,1),mar=c(4,4,0,0)+0.1,las=1)
plotMap(westCoastLL, tck = c(-0.02), xlim=c(-140,-113.0), ylim=c(29.9,59.1),col=gray(0.8))
addLines(WCstatesInlandPBS)
map('state',add=T,region=c("Idaho","Montana","Nevada","Arizona"))
addLines(province,polyProps=data.frame(PID=alberta))
addLines(province,polyProps=data.frame(PID=12,SID=c(123)))
addLines(LMEoffshore,polyProps=data.frame(PID=76)) #foudn this by x <- addLines(LMEoffshore) then looking at x
addLines(LMEoffshore,polyProps=data.frame(PID=169))
points(-theCities$Lon,theCities$Lat,pch=16,col=gray(0.4))
text(-theCities$Lon,theCities$Lat,theCities$Name,pos=4,cex=0.7,col=gray(0.4))

text(-123,56,"BC",cex=1.2)
text(-120,47.1,"WA",cex=1.1)
text(-120,44.3,"OR",cex=1.2)
text(-118.8,35.9,"CA",cex=1.2)
text(-122.8,51,"Strait of\nGeorgia",adj=0,cex=0.7)
arrows(-122.8,51,-123.55,49.67,length=0.05)
text(-120.7,48.4,"Puget\nSound",adj=0,cex=0.7)
arrows(-120.7,48.4,-122.1,47.7,length=0.05)
text(-133.2,52,"Haida\nGwaii",adj=1,cex=0.7)
arrows(-133.2,52,-132.2,52.5,length=0.05)
text(-117.8,30.5,"Baja\nCalifornia",adj=1,cex=0.7)
arrows(-117.8,30.5,-115.5,30.5,length=0.05)
text(-131.2,58,"SE\nAlaska",adj=0,cex=0.7)
arrows(-131.2,58,-132.3,57.5,length=0.05)
text(-128.3,49.9,"Vancouver\nIsland",adj=1,cex=0.7)
arrows(-128.3,49.9,-127.4,49.8,length=0.05)
text(-129.5,51.6,"Queen\nCharlotte\nSound",adj=0.5,cex=0.6)
text(-133.5,54.5,"Dixon Entrance",adj=1,cex=0.7)
arrows(-133.5,54.45,-132.5,54.45,length=0.05)
text(-126.2,48.5,"Strait of\nJuan de Fuca",adj=1,cex=0.7)
arrows(-126.2,48.5,-124.7,48.5,length=0.05)

text(-128,39,"California Current LME",cex=1.4,srt=285)
text(-138,56.8,"Gulf Of Alaska\nLME",cex=0.9,srt=300)
if(doPNG | doTIFF | doEPS) {dev.off()}

