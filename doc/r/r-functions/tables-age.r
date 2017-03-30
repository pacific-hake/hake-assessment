make.input.age.data.table <- function(model,
                                      fleet = 1,            ## 1=Fishery, 2=Survey
                                      start.yr,             ## Start the table on this year
                                      end.yr,
                                      xcaption = "default",
                                      xlabel = "default",
                                      font.size = 9,        ## Size of the font for the table
                                      space.size = 10,      ## Size of the spaces for the table
                                      placement = "H",
                                      decimals = 2
                                      ){
  ## Returns an xtable in the proper format for the main tables section for combined
  ##  fishery or survey age data.
  ##  age data.

  ## Get ages from header names
  age.df <- model$dat$agecomp
  nm <- colnames(age.df)
  yr <- age.df$Yr
  flt <- age.df$FltSvy
  n.samp <- age.df$Nsamp
  ## Get ages from column names
  ages.ind <- grep("^a[[:digit:]]+$", nm)
  ages <- gsub("^a([[:digit:]]+)$", "\\1", nm[ages.ind])
  ## Make all bold
  ages <- latex.bold(ages)
  ## Put ampersands in between each item and add newline to end
  ages.tex <- paste0(latex.paste(ages), latex.nline)

  ## Construct age data frame
  age.df <- age.df[,ages.ind]
  age.df <- t(apply(age.df, 1, function(x){x / sum(x)}))
  age.headers <- paste0(latex.mcol(1, "c", ages), latex.amp())
  age.df <- cbind(yr, n.samp, flt, age.df)

  ## Fishery or survey?
  age.df <- age.df[age.df[,"flt"] == fleet,]
  ## Remove fleet information from data frame
  age.df <- age.df[,-3]
  ## Extract years
  age.df <- age.df[age.df[,"yr"] >= start.yr & age.df[,"yr"] <= end.yr,]

  ## Make number of samples pretty
  age.df[,2] <- f(age.df[,2])
  ## Make percentages for age proportions
  age.df[,-c(1,2)] <- as.numeric(age.df[,-c(1,2)]) * 100
  age.df[,-c(1,2)] <- f(as.numeric(age.df[,-c(1,2)]), decimals)
  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(age.df)

  addtorow$command <-
    c(paste0(latex.hline,
             latex.bold("Year"),
             latex.amp(),
             latex.mlc(c("Number", "of samples")),
             latex.amp(),
             latex.mcol(length(ages),
                        "c",
                        latex.bold("Age (\\% of total for each year)")),
             latex.nline,
             latex.hline,
             latex.amp(2),
             ages.tex),
      latex.hline)

  ## Make the size string for font and space size
  size.string <- latex.size.str(font.size, space.size)

  return(print(xtable(age.df,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(age.df))),
               caption.placement = "top",
               include.rownames = FALSE,
               include.colnames = FALSE,
               sanitize.text.function = function(x){x},
               size = size.string,
               add.to.row = addtorow,
               table.placement = placement,
               tabular.environment = "tabular",
               hline.after = NULL))
}

make.can.age.data.table <- function(dat,
                                    fleet = 1,            ## 1=Can-Shoreside, 2=Can-FT, 3=Can-JV
                                    start.yr,             ## Start the table on this year
                                    end.yr,
                                    xcaption = "default",
                                    xlabel = "default",
                                    font.size = 9,        ## Size of the font for the table
                                    space.size = 10,      ## Size of the spaces for the table
                                    placement = "H",
                                    decimals = 2
                                    ){
  ## Returns an xtable in the proper format for the main tables section for
  ##  Canadian age data.
  ages.df <- dat[[fleet]]
  n.trip.haul <- as.numeric(dat[[fleet + 3]])
  ages.df <- cbind(n.trip.haul, ages.df)
  dat <- cbind(as.numeric(rownames(ages.df)), ages.df)
  dat <- dat[dat[,1] >= start.yr & dat[,1] <= end.yr,]

  dat[,2] <- as.numeric(f(dat[,2]))
  ## Make percentages
  dat[,-c(1,2)] <- dat[,-c(1,2)] * 100
  dat[,-c(1,2)] <- f(dat[,-c(1,2)], decimals)
  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(dat)
  age.headers <- colnames(dat)[grep("^[[:digit:]].*", colnames(dat))]
  ages.tex <- latex.paste(latex.bold(1:length(age.headers)))

  if(fleet == 2 | fleet == 3){
    mlc <- latex.mlc(c("Number", "of hauls"))
  }else{
    mlc <- latex.mlc(c("Number", "of trips"))
  }
  addtorow$command <-
    c(paste0(latex.hline,
             latex.bold("Year"),
             latex.amp(),
             mlc,
             latex.amp(),
             latex.mcol(length(age.headers),
                        "c",
                        latex.bold("Age (\\% of total for each year)")),
             latex.nline,
             latex.hline,
             latex.amp(2),
             ages.tex,
             latex.nline),
      latex.hline)


  ## Make the size string for font and space size
  size.string <- latex.size.str(font.size, space.size)

  return(print(xtable(dat,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(dat))),
               caption.placement = "top",
               include.rownames = FALSE,
               include.colnames = FALSE,
               sanitize.text.function = function(x){x},
               size = size.string,
               add.to.row = addtorow,
               table.placement = placement,
               tabular.environment = "tabular",
               hline.after = NULL))
}

make.us.age.data.table <- function(dat,
                                   fleet = 1,
                                   start.yr,
                                   end.yr,
                                   xcaption = "default",
                                   xlabel = "default",
                                   font.size = 9,
                                   space.size = 10,
                                   placement = "H",
                                   decimals = 2
                                   ){

  ## Returns an xtable in the proper format for the main tables section for US
  ##  age data.
  ##
  ## fleet - 1=US-CP, 2=US-MS, 3=US-Shoreside
  ## start.yr - start the table on this year
  ## end.yr - end the table on this year
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of table
  ## decimals - number of decimals in the numbers in the table

  if(fleet == 1 | fleet == 2){
    dat[,c(2,3)] <- f(dat[,c(2,3)])
    ## Make percentages
    dat[,-c(1,2,3)] <- dat[,-c(1,2,3)] * 100
    ## Make pretty
    dat[,-c(1,2,3)] <- f(dat[,-c(1,2,3)], decimals)
  }else{
    dat[,2] <- f(dat[,2])
    ## Make percentages
    dat[,-c(1,2)] <- dat[,-c(1,2)] * 100
    ## Make pretty
    dat[,-c(1,2)] <- f(dat[,-c(1,2)], decimals)
  }
  dat <- dat[dat[,1] >= start.yr & dat[,1] <= end.yr,]

  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(dat)
  age.headers <- colnames(dat)[grep("^a.*", colnames(dat))]
  ages.tex <- latex.paste(latex.bold(1:length(age.headers)))

  if(fleet == 1 | fleet == 2){
    mlc <- latex.mlc(c("Number", "of hauls"))
  }else{
    mlc <- latex.mlc(c("Number", "of trips"))
  }
  addtorow$command <-
    c(paste0(latex.hline,
             latex.bold("Year"),
             latex.amp(),
             ifelse(fleet == 1 | fleet == 2,
                    paste0(latex.mlc(c("Number", "of fish")), latex.amp()),
                    ""),
             mlc,
             latex.amp(),
             latex.mcol(length(age.headers),
                        "c",
                        latex.bold("Age (\\% of total for each year)")),
             latex.nline,
             latex.hline,
             ifelse(fleet == 1 | fleet == 2,
                    latex.amp(3),
                    latex.amp(2)),
             ages.tex,
             latex.nline),
      latex.hline)

  ## Make the size string for font and space size
  size.string <- latex.size.str(font.size, space.size)

  return(print(xtable(dat,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(dat))),
               caption.placement = "top",
               include.rownames = FALSE,
               include.colnames = FALSE,
               sanitize.text.function = function(x){x},
               size = size.string,
               add.to.row = addtorow,
               table.placement = placement,
               tabular.environment = "tabular",
               hline.after = NULL))
}

make.est.numbers.at.age.table <- function(model,
                                          start.yr,
                                          end.yr,
                                          weight.factor = 1000,
                                          plus.group = 15,
                                          table.type = 1,
                                          csv.dir = "out-csv",
                                          xcaption = "default",
                                          xlabel   = "default",
                                          font.size = 9,
                                          space.size = 10){
  ## Returns an xtable in the proper format for the main tables section for
  ##  credibility intervals for biomass, relative biomass, recruitment,
  ##  fishing intensity, and exploitation fraction. Assumes data-tables.r
  ##  has been loaded for the csv file name definitions.
  ##
  ## model - an mcmc run, output of the r4ss package's function SSgetMCMC()
  ## start.yr - the first year to show in the table
  ## end.yr - the last year to show in the table
  ## weight.factor - divide catches by this factor
  ## plus.group - ages this and older will be grouped in table
  ## table.type - 1=NAA, 2=Exploitation Rate by age, 3=CAA in number, 4=CAA in
  ##  biomass, 5=Biomass at age
  ## csv.dir - where to write the output csv file
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table

  if(!dir.exists(csv.dir)){
    dir.create(csv.dir)
  }

  yrs <- start.yr:end.yr

  naa <- model$natage
  caa <- model$catage
  waa <- model$wtatage

  n.age.b <- naa[naa$"Beg/Mid" == "B", -c(1:6, 8:11)]
  n.age.b <- n.age.b[n.age.b$Yr %in% yrs,]
  n.age.m <- naa[naa$"Beg/Mid" == "M", -c(1:6, 8:11)]
  n.age.m <- n.age.m[n.age.m$Yr %in% yrs,]

  c.age <- caa[caa$Yr %in% yrs,]
  c.age <- c.age[, -(1:10)]
  ## Get weight-at-age matrix (currently the same matrix for fleet = -1,
  ##  0, 1, and 2)
  wt.at.age <- waa[waa$fleet == 1,]

  if(table.type == 1){
    ## Numbers-at-age
    ## Group ages greater than or equal to plus.group together by summing
    dat <- n.age.b
    max.age <- max(as.numeric(names(dat[,-1])))
    plus.ages <- plus.group:max.age
    dat.plus <- dat[,names(dat) %in% plus.ages]
    dat.minus <- dat[,!names(dat) %in% plus.ages]
    dat <- cbind(dat.minus, apply(dat.plus, 1, sum))
    names(dat)[ncol(dat)] <- paste0(plus.group, "+")
    names(dat)[1] <- "Year"
    ## Apply division by weight factor
    dat[,-1] <- apply(dat[-1], c(1, 2), function(x) x / weight.factor)
    ## Write to CSV before formatting for xtable output
    write.csv(dat,
              file.path(csv.dir, out.est.naa.file),
              row.names = FALSE)
    ## Apply formatting
    dat[,-1] <- apply(dat[-1], c(1,2), f)
    dat[,1] <- as.character(dat[,1])
  }else if(table.type == 2){
    ## Exploitation rate at age
    ## Remove year column, do calculation and add year column back
    n.age.m <- n.age.m[,-1]
    dat <- f((c.age / n.age.m) * 100, 2)
    dat <- cbind(as.character(yrs), dat)
    colnames(dat)[1] <- "Year"
    write.csv(dat,
              file.path(csv.dir, "estimated-exploitation-at-age.csv"),
              row.names = FALSE)
  }else if(table.type == 3){
    ## Catch-at-age in numbers
    csv.dat <- c.age
    csv.dat <- cbind(yrs, csv.dat)
    colnames(csv.dat)[1] <- "Year"
    ## Write to CSV before formatting for xtable output
    write.csv(csv.dat,
              file.path(csv.dir, out.est.caa.file),
              row.names = FALSE)
    ## Apply formatting
    c.age <- f(c.age)
    dat <- cbind(yrs, c.age)
  }else if(table.type == 4){
    ## Catch-at-age in biomass
    wt.at.age <- wt.at.age[,-(1:6)]
    dat <- c.age * wt.at.age
    csv.dat <- cbind(yrs, dat)
    colnames(csv.dat)[1] <- "Year"
    ## Write to CSV before formatting for xtable output
    write.csv(csv.dat,
              file.path(csv.dir, out.est.caa.bio.file),
              row.names = FALSE)
    ## Apply formatting
    dat <- f(dat)
    dat <- cbind(yrs, dat)
  }else if(table.type == 5){
    ## Biomass-at-age
    dat <- n.age.b
    dat <- dat[,-1]
    ## If needed, add years using mean across years
    missing.yrs <- as.numeric(yrs)[!as.numeric(yrs) %in% abs(wt.at.age$yr)]
    if(length(missing.yrs) > 0){
      ## get mean vector (assuming it is the one associated with -1966)
      mean.wt.at.age <- wt.at.age[wt.at.age$yr == -1966,]
      ## loop over missing years (if any present)
      for(iyr in 1:length(missing.yrs)){
        mean.wt.at.age$yr <- missing.yrs[iyr]
        wt.at.age <- rbind(wt.at.age, mean.wt.at.age)
      }
      ## sort by year just in case the missing years weren't contiguous
      wt.at.age <- wt.at.age[order(abs(wt.at.age$yr)),]
    }
    wt.at.age <- wt.at.age[,-(1:6)]
    dat <- dat * wt.at.age
    dat <- cbind(yrs, dat)
    names(dat)[1] <- "Year"

    ## Apply division by weight factor and formatting
    dat[,-1] <- apply(dat[-1], c(1,2), function(x) x / weight.factor)
    ## Write to CSV before formatting
    write.csv(dat,
              file.path(csv.dir, out.est.baa.file),
              row.names = FALSE)
    ## Apply formatting
    dat[,-1] <- apply(dat[-1], c(1,2), f)
    dat[,1] <- as.character(dat[,1])
  }else{
    return(invisible())
  }
  ## Add latex headers
  ##colnames(dat) <- sapply(colnames(dat), latex.bold)
  ## Remove the "Year" from the first column as it will go above with "Age"
  ##colnames(dat)[1] <- ""
  ages <- colnames(dat)[-1]
  ages.tex <- sapply(ages, latex.bold)
  ages.tex <- paste0(latex.paste(ages.tex), latex.nline)

  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(dat)

  addtorow$command <-
    c(paste0(latex.hline,
             latex.bold("Year"),
             latex.amp(),
             latex.mcol(ncol(dat) - 1,
                        "c",
                        latex.bold("Age")),
             latex.nline,
             latex.hline,
             latex.amp(),
             ages.tex),
      latex.hline)

  ## Make the size string for font and space size
  size.string <- latex.size.str(font.size, space.size)
  return(print(xtable(dat,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(dat))),
               caption.placement = "top",
               include.rownames = FALSE,
               include.colnames = FALSE,
               sanitize.text.function = function(x){x},
               size = size.string,
               add.to.row = addtorow,
               table.placement = "H",
               tabular.environment = "tabular",
               hline.after = NULL))

}

make.cohort.table <- function(model,
                              cohorts,              ## a vector of cohort years which are to appear in the table
                              start.yr,             ## year to start the at-age matrix calcs
                              end.yr,               ## year to end the  at-age matrix calcs
                              weight.factor = 1000, ## divide catches by this factor
                              csv.dir = "out-csv",  ## The outputs will be written to a csv file in this directory
                              xcaption = "default",
                              xlabel   = "default",
                              font.size = 9,
                              space.size = 10,      ## Size of the spacing for the table
                              scalebox = '1.0'      ## attempt to allow table to be narrower
                              ){
  ## Returns an xtable in the proper format for cohort's start biomass,
  ## catch weight, natural mortality weight, and surviving biomass all by age

  if(!length(cohorts)){
    return(invisible())
  }

  ## Make sure the csv directory exists
  if(!dir.exists(csv.dir)){
    dir.create(csv.dir)
  }

  get.cohorts <- function(d, cohorts){
    ## Returns a list of the cohort values for data frame d
    ## cohorts is a vector of the years you want the cohort
    ## info for. Diagonals of the data frame d make up these cohorts.
    ## Assumes the year is in column 1.
    coh.inds <- as.character(which(d[,1] %in% cohorts) - 1)
    delta <- row(d[-1]) - col(d[-1])
    coh.list <- split(as.matrix(d[,-1]), delta)
    lapply(coh.inds, function(x){get(x, coh.list)})
  }

  ## Extract the numbers-at-age
  naa <- model$natage[model$natage$"Beg/Mid" == "B", -c(1:6,8:11)]
  ## Numbers at age in next year
  naa.next <- naa[naa$Yr >= start.yr+1 & naa$Yr <= end.yr+1,]
  ## Numbers at age in same year
  naa <- naa[naa$Yr >= start.yr & naa$Yr <= end.yr,]

  ## Change year to match other
  naa.next$Yr <- naa.next$Yr - 1
  # vector of years for use in other places
  yrs <- naa$Yr

  coh.naa <- get.cohorts(naa, cohorts)

  ## Cohort numbers at age in next year
  coh.naa.next <- get.cohorts(naa.next, cohorts - 1)
  ## Throw away the first one so that this represents shifted by 1 year values
  coh.naa.next <- lapply(coh.naa.next, function(x){x[-1]})

  ## Extract weight-at-age for the fishery (fleet 1)
  waa <- model$wtatage[model$wtatage$fleet == 1,]
  ## if needed, add years to weight-at-age matrix using mean across years
  missing.yrs <- as.numeric(yrs)[!as.numeric(yrs) %in% abs(waa$yr)]
  if(length(missing.yrs)>0){
    # get mean vector (assuming it is the one associated with -1966)
    mean.waa <- waa[waa$yr==-1966,]
    # loop over missing years (if any present)
    for(iyr in 1:length(missing.yrs)){
      mean.waa$yr <- missing.yrs[iyr]
      waa <- rbind(waa, mean.waa)
    }
    # sort by year just in case the missing years weren't contiguous
    # (this is probably not needed)
    waa <- waa[order(abs(waa$yr)),]
  }
  # strip off initial columns of waa matrix (except year)
  waa <- waa[,-(2:6)]
  waa$yr <- abs(waa$yr)
  coh.waa <- get.cohorts(waa, cohorts)

  ## Catch-at-age
  caa <- model$catage
  caa <- caa[,-c(1:6,8:10)]
  # add a row of zeros to make dimensions match other matrices
  # (which may include forecast year)
  # not bothering to make this more complicated like waa above
  caa <- rbind(caa, c(max(caa$Yr)+1, rep(0, ncol(caa)-1)))
  coh.caa <- get.cohorts(caa, cohorts)
  ages <- 0:(ncol(caa) - 2)

  ## Start biomass-at-age
  baa <- cbind(naa$Yr, naa[-1] * waa[-1] / weight.factor)
  coh.baa <- get.cohorts(baa, cohorts)

  ## Catch weight
  coh.catch <- lapply(1:length(coh.waa),
                      function(i, waa, caa){
                        waa[[i]] * caa[[i]] / weight.factor},
                      waa = coh.waa,
                      caa = coh.caa)

  coh.surv <- lapply(1:length(coh.naa),
                     function(i, waa, naa){
                       waa[[i]] * naa[[i]] / weight.factor},
                     waa = coh.waa,
                     naa = coh.naa.next)

  ## Natural mortality weight
  coh.m <- lapply(1:length(coh.surv),
                  function(i, baa, catch, surv){
                    baa[[i]] - surv[[i]] - catch[[i]]},
                  baa = coh.baa,
                  catch = coh.catch,
                  surv = coh.surv)

  ##----------------------------------------------------------------------------
  ## write the CSV
  ## Bind the individual cohort value vectors into matrices
  csv.coh.sum <-
    lapply(1:length(coh.naa),
           function(i, baa, catch, m, surv){
             do.call(cbind, list(baa[[i]],
                                 catch[[i]],
                                 m[[i]],
                                 surv[[i]]))},
           baa = coh.baa,
           catch = coh.catch,
           m = coh.m,
           surv = coh.surv)
  ## Remove entries in all but the first column (baa is only known value)
  ## Not bothering to figure out how to do this with lapply
  for(i in 1:length(csv.coh.sum)){
    csv.coh.sum[[i]][nrow(csv.coh.sum[[i]]), -1] <- NA
  }
  ## Add a column in the first column for the ages
  csv.coh.sum <- append(csv.coh.sum, list(as.data.frame(ages)), after = 0)
  ## Bind the list of cohort value matrices into a single ragged matrix
  n <- max(sapply(csv.coh.sum, nrow))
  csv.coh.sum.mat <- do.call(cbind,
                             lapply(csv.coh.sum, function(x){
                               rbind(x, matrix(, n - nrow(x), ncol(x)))}))
  csv.coh.sum.mat <- as.data.frame(csv.coh.sum.mat)
  csv.headers <- lapply(1:length(cohorts),
                        function(i, cohort){
                          c(paste(cohort[i], "Start Biomass"),
                            paste(cohort[i], "Catch Weight"),
                            paste(cohort[i], "M Weight"),
                            paste(cohort[i], "Surviving Biomass"))},
                        cohort = cohorts)
  csv.headers <- c("Age", unlist(csv.headers))
  colnames(csv.coh.sum.mat) <- csv.headers
  write.csv(csv.coh.sum.mat,
            file.path(csv.dir, "cohort-effects.csv"),
            row.names = FALSE,
            na = "")
  ##----------------------------------------------------------------------------

  ##----------------------------------------------------------------------------
  ## Create the latex table (same steps as above but with nice formatting
  ## Bind the individual cohort value vectors into matrices
  coh.sum <- lapply(1:length(coh.naa),
                    function(i, baa, catch, m, surv){
                      do.call(cbind, list(f(baa[[i]], 1),
                                          f(catch[[i]], 1),
                                          f(m[[i]], 1),
                                          f(surv[[i]], 1)))},
                    baa = coh.baa,
                    catch = coh.catch,
                    m = coh.m,
                    surv = coh.surv)
  ## Remove entries in all but the first column (baa is only known value)
  ## Not bothering to figure out how to do this with lapply
  for(i in 1:length(coh.sum)){
    coh.sum[[i]][nrow(coh.sum[[i]]), -1] <- NA
  }
  ## Add a column in the first column for the ages
  coh.sum <- append(coh.sum, list(as.data.frame(ages)), after = 0)
  ## Bind the list of cohort value matrices into a single ragged matrix
  n <- max(sapply(coh.sum, nrow))
  coh.sum.mat <- do.call(cbind,
                         lapply(coh.sum, function(x){
                           rbind(x, matrix(, n - nrow(x), ncol(x)))}))
  coh.sum.mat <- as.data.frame(coh.sum.mat)

  ## Add latex headers
  colnames(coh.sum.mat) <-
    c(latex.bold("Age"),
      rep(c(latex.mlc(c("Start", "Biomass", "000s t")),
            latex.mlc(c("Catch", "Weight", "000s t")),
            latex.mlc(c("M", "000s t")),
            latex.mlc(c("Surviving", "Biomass", "000s t"))),
          length(cohorts)))
  ##----------------------------------------------------------------------------

  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$command <- latex.hline
  for(i in 1:length(cohorts)){
    addtorow$command <-
      paste0(addtorow$command,
             latex.amp(),
             latex.mcol(4,
                        "c",
                        latex.bold(paste(cohorts[i], " cohort"))))
  }
  addtorow$command <- paste0(addtorow$command, latex.nline)
  size.string <- latex.size.str(font.size, space.size)
  return(print(xtable(coh.sum.mat,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(coh.sum.mat))),
               caption.placement = "top",
               add.to.row = addtorow,
               table.placement = "H",
               include.rownames = FALSE,
               sanitize.text.function = function(x){x},
               size = size.string, scalebox=scalebox))

}
