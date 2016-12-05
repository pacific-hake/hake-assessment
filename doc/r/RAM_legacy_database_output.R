# calculate posterior medians for quantities needed for RAM Legacy Database
# assembled by Ian Taylor based on a request from Lewis Barnett on June 1, 2016
# this file is based on figures-age-comp-forecast.r and both are  messy and not
# generalized but it shouldn't be too hard to generalize them

base.dir <- "c:/github/hake-assessment/models/55_2016base/"

# empty tables
natage.array <- array(NA, dim=c(51,22,999))
catage.array <- array(NA, dim=c(50,22,999))
natage.medians <- matrix(NA, 51, 22)
catage.medians <- matrix(NA, 50, 22)

# unique text to identify rows
natage.text.start <- "NUMBERS_AT_AGE_Annual_2 With_fishery"
natage.text.end <- "Z_AT_AGE_Annual_2 With_fishery"
catage.text.start <- "CATCH_AT_AGE"
catage.text.end <- "BIOLOGY"

natage.medians

# loop over all report files to read them, find the location of lines of interest,
# and then read those lines as tables
# NOTE: THIS TAKES A WHILE AND SHOULD BE COMBINED WITH THE "PARTEST" STUFF
#for(irep in 1:10){
for(irep in 1:999){
  if(irep %% 100 == 0){
    cat("irep:", irep, "\n")
  }
  rep <- file.path(base.dir, "partest/reports/", paste0("Report_", irep, ".sso"))
  # need to reach each report file because length of "SPR/YPR_Profile"
  # may differ among files
  repLines <- readLines(rep)

  # numbers at age
  natage.lines <- (grep(natage.text.start, repLines) + 1):
                     (grep(natage.text.end, repLines) - 5) # manually adjust offset

  natage.table <- read.table(file=rep, skip=natage.lines[1]-1,
                             nrow=length(natage.lines), header=TRUE)
  # remove unnecessary columns
  natage.table.slim <- as.matrix(natage.table[natage.table$Year %in% 1966:2016,
                                              c("Year",paste0("X",0:20))])

  # catch at age
  catage.lines <- (grep(catage.text.start, repLines)[2] + 1):
                     (grep(catage.text.end, repLines)[2] - 3) # manually adjust offset

  catage.table <- read.table(file=rep, skip=catage.lines[1]-1,
                             nrow=length(catage.lines), header=TRUE)
  # remove unnecessary columns
  catage.table.slim <- as.matrix(catage.table[catage.table$Yr %in% 1966:2015,
                                              c("Yr",paste0("X",0:20))])

  # add tables to array
  natage.array[,,irep] <- natage.table.slim
  catage.array[,,irep] <- catage.table.slim
}

for(icol in 1:22){
  natage.medians[,icol] <- apply(natage.array[,icol,], 1, median, na.rm=TRUE)
  catage.medians[,icol] <- apply(catage.array[,icol,], 1, median, na.rm=TRUE)
}

natage.medians <- as.data.frame(natage.medians)
names(natage.medians)=c("Year", paste0("Age", 0:20))
catage.medians <- as.data.frame(catage.medians)
names(catage.medians)=c("Year", paste0("Age", 0:20))

# compare to MLE
h1 <- SS_output(base.dir)
natage.MLE <- h1$natage[h1$natage$Time %in% 1966:2016,c("Yr",0:20)]
catage.MLE <- h1$catage[h1$catage$Yr %in% 1966:2015,c("Yr",0:20)]

# summary of ratios: NAs are from 0 values in denominator for catches at age 0
summary(as.numeric(as.matrix(natage.MLE/natage.medians)))
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.1603  0.4570  0.6564  0.6306  0.7910  1.0000 
summary(as.numeric(as.matrix(catage.MLE/catage.medians)))
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
## 0.2621  0.8289  1.0710  1.1200  1.3270  3.2560      50 

write.csv(natage.medians, file.path(base.dir,"numbers.at.age.medians.csv"), row.names=FALSE)
write.csv(catage.medians, file.path(base.dir,"catch.at.age.medians.csv"), row.names=FALSE)
