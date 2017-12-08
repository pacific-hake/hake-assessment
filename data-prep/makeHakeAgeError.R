makeHakeAgeError <- function(startyr=1973, endyr=2017,
                             cohorts=c(1980, 1984, 1999, 2010, 2014),
                             adjust=0.55,
                             print=FALSE,
                             csv=TRUE,
                             dir=getwd(),
                             filename="ageing_error_inputs_2017.csv",
                             minAdjustAge=2){
  # function to produce ageing error inputs for hake model in Stock Synthesis
  # Ian Taylor on Dec. 21, 2012
  # updated Jan. 20, 2015
  # updated again Dec. 4, 2017
  
  # there may be fewer unique ageing error definition than years, but it is simpler
  # and less prone to mismatch if a new definition is created for every year
  # this approach also follows the setup of the recent assessments

  # Ian's prefered way to make the file pretty is to copy the CSV file contents
  # from Excel into emacs, set tabs to 11 using "(setq default-tab-width 11)"
  # and then untabify (via M-x untabify)
  
  # this vector could be replaced with function to generate these values
  base <- c(0.329242, 0.329242, 0.346917, 0.368632, 0.395312, 0.428090, 0.468362,
            0.517841, 0.578630, 0.653316, 0.745076, 0.857813, 0.996322, 1.166500,
            1.375570, 1.632440, 1.858000, 2.172000, 2.530000, 2.934000, 3.388000)
  yrs <- startyr:endyr
  nyrs <- length(yrs)
  ncols <- length(base)
  last3 <- ncols + 1:3


  # make data frame to fill in
  agemat <- data.frame(matrix(0,nyrs*2,ncols),
                       yr="",def="",comment="",stringsAsFactors=FALSE)
  names(agemat)[1:ncols] <- c("#age0",paste("age",1:(ncols-1),sep=""))

  # loop over pairs of rows
  for(iyr in 1:nyrs){
    y <- yrs[iyr]
    # which ages/columns have cohort effect?
    adjustAges <- sort(y - cohorts)
    adjustAges <- adjustAges[adjustAges <= 20 &
                             adjustAges >= minAdjustAge]
    adjustColumns <-  adjustAges + 1

    # description of cohort effect
    adjustComment <- ""
    if(length(adjustAges) > 0){
      adjustComment <- paste0(paste0(adjust, "*age", adjustAges), collapse=", ")
    }

    # create new row with some values adjusted
    newrow <- base
    newrow[adjustColumns] <- adjust*base[adjustColumns]

    # vector of mean values (half-year = unbiased value in SS)
    agemat[iyr*2 - 1, -last3] <- 0.5+0:20
    agemat[iyr*2 - 1,  last3] <- c(paste("#",y),
                                   paste("def",iyr,sep=""),
                                   "'Expected ages'")
    agemat[iyr*2,     -last3] <- newrow
    agemat[iyr*2,      last3] <- c(paste("#",y),
                                   paste("def",iyr,sep=""),
                                   paste0("'SD of age. ", adjustComment, "'"))
  }

  # print to screen if requested (expected values are reported with many zeros)
  if(print){
    oldwidth <- options()$width
    options(width=300)
    print(agemat)
    options(width=oldwidth)
  }

  if(csv){
    write.csv(agemat,paste(dir,filename,sep='/'),row.names=FALSE)
  }
  
  # return invisibly
  return(invisible(agemat))
}
