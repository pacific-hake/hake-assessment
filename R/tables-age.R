make.input.age.data.table <- function(model,
                                      fleet = 1,
                                      start.yr,
                                      end.yr,
                                      csv.dir = "out-csv",
                                      xcaption = "default",
                                      xlabel = "default",
                                      font.size = 9,
                                      space.size = 10,
                                      placement = "htbp",
                                      decimals = 2){
  ## Returns an xtable in the proper format for the main tables section for
  ##  combined fishery or survey age data
  ##
  ## fleet - 1 = Fishery, 2 = Survey
  ## start.yr - start the table on this year
  ## end.yr - end the table on this year
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of table
  ## decimals - number of decimals in the numbers in the table

  if(!dir.exists(csv.dir)){
    dir.create(csv.dir)
  }

  ## Get ages from header names
  age.df <- model$dat$agecomp
  nm <- colnames(age.df)
  yr <- age.df$Yr
  flt <- age.df$FltSvy
  n.samp <- age.df$Nsamp
  ## Get ages from column names
  ages.ind <- grep("^a[[:digit:]]+$", nm)
  ages.num <- gsub("^a([[:digit:]]+)$", "\\1", nm[ages.ind])
  ## Make all bold
  ages <- latex.bold(ages.num)
  ## Put ampersands in between each item and add newline to end
  ages.tex <-latex.paste(ages)

  ## Construct age data frame
  age.df <- age.df[,ages.ind]
  age.df <- t(apply(age.df,
                    1,
                    function(x){
                      as.numeric(x) / sum(as.numeric(x))
                    }))
  age.headers <- paste0(latex.mcol(1, "c", ages), latex.amp())
  age.df <- cbind(yr, n.samp, flt, age.df)

  ## Fishery or survey?
  age.df <- age.df[age.df[,"flt"] == fleet,]
  ## Remove fleet information from data frame
  age.df <- age.df[,-3]
  ## Extract years
  age.df <- age.df[age.df[,"yr"] >= start.yr & age.df[,"yr"] <= end.yr,]

  ## Make number of samples pretty
  age.df[,2] <- f(as.numeric(age.df[,2]))
  ## Make percentages for age proportions
  age.df[,-c(1,2)] <- as.numeric(age.df[,-c(1,2)]) * 100
  age.df[,-c(1,2)] <- f(as.numeric(age.df[,-c(1,2)]), decimals)
  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1

  addtorow$command <-
    paste0(latex.hline,
           latex.bold("Year"),
           latex.amp(),
           latex.mlc(c("Number",
                       "of samples")),
           latex.amp(),
           latex.mcol(length(ages),
                      "c",
                      latex.bold("Age (\\% of total for each year)")),
           latex.nline,
           latex.amp(2),
           ages.tex,
           latex.nline,
           latex.hline,
           latex.rephead(ncol(age.df)))

  size.string <- latex.size.str(font.size, space.size)
  ## Write the CSV
  cnames <- colnames(age.df)
  cnames[3:length(cnames)] <- ages.num
  ## Add + for plus group
  cnames[length(cnames)] <- paste0(cnames[length(cnames)], "+")
  colnames(age.df) <- cnames
  write.csv(age.df,
            file.path(csv.dir,
                      ifelse(fleet == 1,
                             "fishery-input-age-proportions.csv",
                             "survey-input-age-proportions.csv")),
            na = "")

  print(xtable(age.df,
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
        tabular.environment = "longtable",
        hline.after = NULL)
}

make.can.age.data.table <- function(dat,
                                    fleet = 1,
                                    start.yr,
                                    end.yr,
                                    xcaption = "default",
                                    xlabel = "default",
                                    font.size = 9,
                                    space.size = 10,
                                    placement = "H",
                                    decimals = 2){
  ## Returns an xtable in the proper format for the main tables section for
  ##  Canadian age data.
  ##
  ## fleet - 1 = Can-Shoreside, 2 = Can-FT, 3 = Can-JV
  ## start.yr - start the table on this year
  ## end.yr - end the table on this year
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of table
  ## decimals - number of decimals in the numbers in the table

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
  age.headers <- colnames(dat)[grep("^[[:digit:]].*", colnames(dat))]
  ages.tex <- latex.paste(latex.bold(1:length(age.headers)))

  if(fleet == 2 | fleet == 3){
    mlc <- latex.mlc(c("Number",
                       "of hauls"))
  }else{
    mlc <- latex.mlc(c("Number",
                       "of trips"))
  }
  addtorow$command <-
    paste0(latex.hline,
           latex.bold("Year"),
           latex.amp(),
           mlc,
           latex.amp(),
           latex.mcol(length(age.headers),
                      "c",
                      latex.bold("Age (\\% of total for each year)")),
           latex.nline,
           latex.amp(2),
           ages.tex,
           latex.nline,
           latex.hline,
           latex.rephead(ncol(dat)))

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(dat,
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
        tabular.environment = "longtable",
        hline.after = NULL)
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
                                   decimals = 2){

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
  ncolumns <- grep("n\\.", colnames(dat))
  dat[, ncolumns] <- f(dat[, ncolumns])
  dat[, -c(1, ncolumns)] <- f(dat[, -c(1, ncolumns)]* 100, decimals)
  dat <- dat[dat[,1] >= start.yr & dat[,1] <= end.yr,]

  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  age.headers <- colnames(dat)[grep("^a.*", colnames(dat))]
  ages.tex <- latex.paste(latex.bold(1:length(age.headers)))

  if(fleet == 1 | fleet == 2){
    mlc <- latex.mlc(c("Number",
                       "of hauls"))
  }else{
    mlc <- latex.mlc(c("Number",
                       "of trips"))
  }
  addtorow$command <-
    paste0(latex.hline,
           latex.bold("Year"),
           latex.amp(),
           latex.mlc(c("Number", "of fish")),
           latex.amp(),
           mlc,
           latex.amp(),
           latex.mcol(length(age.headers),
                      "c",
                      latex.bold("Age (\\% of total for each year)")),
           latex.nline,
           latex.amp(3),
           ages.tex,
           latex.nline,
           latex.hline,
           latex.rephead(ncol(dat)))

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(dat,
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
        tabular.environment = "longtable",
        hline.after = NULL)
}

#' Makes a table of the estimated -at-age values for 5 different values:
#' Numbers-at-age, Catch-at-age, Biomass-at-age, Exploitation-at-age,
#' and Catch-at-age-biomass
#'
#' @param model A model in this project
#' @param start_yr Start year for the table
#' @param end_yr End year for the table
#' @param table_type 1 = Numbers-at-age, 2 = Exploitation-rate-at-age, 3 = Catch-at-age-number
#' 4 = Catch-at-age-biomass, 5 = Biomass-at-age
#' @param csv_dir Directory for CSV output
#' @param xcaption Table caption
#' @param xlabel The label used to reference the table in latex
#' @param font_size Size of the font for the table
#' @param space_size Size of the vertical spaces for the table
#'
#' @return An [xtable::xtable()]
#' @export
atage_table <- function(model,
                        start_yr = NA,
                        end_yr = NA,
                        table_type = 1,
                        csv_dir = "out-csv",
                        xcaption = "default",
                        xlabel   = "default",
                        font_size = 9,
                        space_size = 10){

  if(!dir.exists(csv_dir)){
    dir.create(csv_dir)
  }

  tbl <- switch (table_type,
                 model$extra.mcmc$natage_median,
                 model$extra.mcmc$expatage_median,
                 model$extra.mcmc$catage_median,
                 model$extra.mcmc$catage_biomass_median,
                 model$extra.mcmc$batage_median)
  fn <- switch (table_type,
                file.path(csv_dir, out.est.naa.file),
                file.path(csv_dir, out.est.eaa.file),
                file.path(csv_dir, out.est.caa.file),
                file.path(csv_dir, out.est.caa.bio.file),
                file.path(csv_dir, out.est.baa.file))

  yrs_in_table <- sort(unique(tbl$Yr))
  min_yr <- min(yrs_in_table)
  max_yr <- max(yrs_in_table)
  start_yr <- ifelse(is.na(start_yr), min_yr, start_yr)
  end_yr <- ifelse(is.na(end_yr), max_yr, end_yr)
  if(start_yr > end_yr){
    start_yr <- min_yr
    end_yr <- max_yr
  }
  start_yr <- ifelse(start_yr < min_yr, min_yr, start_yr)
  end_yr <- ifelse(end_yr > max_yr, max_yr, end_yr)
  yrs <- start_yr:end_yr

  dat <- tbl %>%
    filter(Yr %in% yrs) %>%
    rename(Year = Yr) %>%
    mutate(Year = as.character(Year))
  write_csv(dat, fn)
  dat <- dat %>%
    mutate_at(.vars = vars(-Year), ~{f(.x, 0)})
  names(dat)[length(names(dat))] <- paste0(names(dat)[length(names(dat))], "+")

  # Add latex headers
  ages <- colnames(dat)[-1]
  ages_tex <- map_chr(ages, latex.bold)
  ages_tex <- paste0(latex.paste(ages_tex), latex.nline)

  # Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1

  addtorow$command <-
    paste0(latex.hline,
           latex.bold("Year"),
           latex.amp(),
           latex.mcol(ncol(dat) - 1,
                      "c",
                      latex.bold("Age")),
           latex.nline,
           latex.amp(),
           ages_tex,
           latex.hline,
           latex.rephead(ncol(dat)))

  # Make the size string for font and space size
  size_string <- latex.size.str(font_size, space_size)
  return(print(xtable(dat,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(dat))),
               caption.placement = "top",
               include.rownames = FALSE,
               include.colnames = FALSE,
               sanitize.text.function = function(x){x},
               size = size_string,
               add.to.row = addtorow,
               table.placement = "H",
               tabular.environment = "longtable",
               #latex.environments = "center",
               hline.after = NULL))

}

make.cohort.table <- function(model,
                              cohorts,
                              start.yr,
                              end.yr,
                              weight.factor = 1000,
                              csv.dir = "out-csv",
                              xcaption = "default",
                              xlabel   = "default",
                              font.size = 9,
                              space.size = 10,
                              scalebox = "1.0"){
  ## Returns an xtable in the proper format for cohort's start biomass,
  ## catch weight, natural mortality weight, and surviving biomass all by age
  ##
  ## model - an mcmc run, output of the r4ss package's function SSgetMCMC()
  ## cohorts - a vector of cohort years which are to appear in the table
  ## start.yr - the first year to show in the table
  ## end.yr - the last year to show in the table
  ## weight.factor - divide catches by this factor
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## scalebox - attempt to allow table to be narrower
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

  # vector of ages used in population dynamics
  ages <- 0:model$accuage

  ## Extract the numbers-at-age
  naa <- model$natage[model$natage$"Beg/Mid" == "B",
                      names(model$natage) %in% c("Yr",ages)]
  ## Numbers at age in next year
  naa.next <- naa[naa$Yr >= start.yr+1 & naa$Yr <= end.yr+1,]
  ## Numbers at age in same year
  naa <- naa[naa$Yr >= start.yr & naa$Yr <= end.yr,]

  ## Change year to match other
  naa.next$Yr <- naa.next$Yr - 1
  # vector of years for use in other places
  yrs <- naa$Yr

  ## Subset to match range of years:
  naa <- naa[naa$Yr %in% yrs,]
  naa.next <- naa.next[naa.next$Yr %in% yrs,]

  ## Cohort numbers at age
  coh.naa <- get.cohorts(naa, cohorts)
  coh.naa.next <- get.cohorts(naa.next, cohorts - 1)

  ## Throw away the first one so that this represents shifted by 1 year values
  coh.naa.next <- lapply(coh.naa.next, function(x){x[-1]})

  ## Extract weight-at-age for the fishery (fleet 1)
  waa <- model$wtatage[model$wtatage$Fleet == 1,]
  ## if needed, add years to weight-at-age matrix using mean across years
  missing.yrs <- as.numeric(yrs)[!as.numeric(yrs) %in% abs(waa$Yr)]
  if(length(missing.yrs)>0){
    # get mean vector (assuming it is the one associated with -1966)
    mean.waa <- waa[waa$Yr==-1966,]
    # loop over missing years (if any present)
    for(iyr in 1:length(missing.yrs)){
      mean.waa$Yr <- missing.yrs[iyr]
      waa <- rbind(waa, mean.waa)
    }
    # sort by year just in case the missing years weren't contiguous
    # (this is probably not needed)
    waa <- waa[order(abs(waa$Yr)),]
  }
  # weight-at-age matrix may have negative values for year
  waa$Yr <- abs(waa$Yr)
  # subset years and columns
  waa <- waa[waa$Yr %in% yrs, names(waa) %in% c("Yr", ages)]
  coh.waa <- get.cohorts(waa, cohorts)

  ## Catch-at-age
  caa <- model$catage
  # subset years and columns
  caa <- caa[caa$Yr %in% yrs, names(caa) %in% c("Yr", ages)]
  coh.caa <- get.cohorts(caa, cohorts)

  ## Start-of-year biomass-at-age
  baa <- cbind(naa$Yr, naa[,-1] * waa[,-1] / weight.factor)
  coh.baa <- get.cohorts(baa, cohorts)

  ## Catch weight
  coh.catch <- lapply(1:length(coh.waa),
                      function(i, waa, caa){
                        waa[[i]] * caa[[i]] / weight.factor},
                      waa = coh.waa,
                      caa = coh.caa)
  coh.surv <- lapply(1:length(coh.naa),
                     function(i, waa, naa){
                       ## Throw away the last one so that a warning is not generated
                       if(i == 1)
                         waa[[i]] <- waa[[i]][-length(waa[[i]])]
                       waa[[i]] * naa[[i]] / weight.factor},
                     waa = coh.waa,
                     naa = coh.naa.next)
  ## Natural mortality weight
  coh.m <- lapply(1:length(coh.surv),
                  function(i, baa, catch, surv){
                    ## Throw away the last one so that a warning is not generated
                    if(i == 1){
                      baa[[i]] <- baa[[i]][-length(baa[[i]])]
                      catch[[i]] <- catch[[i]][-length(catch[[i]])]
                    }
                    baa[[i]] - surv[[i]] - catch[[i]]},
                  baa = coh.baa,
                  catch = coh.catch,
                  surv = coh.surv)

  ##----------------------------------------------------------------------------
  ## write the CSV
  ## Bind the individual cohort value vectors into matrices

  suppressWarnings(
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
  )

  ## Remove entries in all but the first column (baa is only known value)
  ## Not bothering to figure out how to do this with lapply
  for(i in 1:length(csv.coh.sum)){
    csv.coh.sum[[i]][nrow(csv.coh.sum[[i]]), -1] <- NA
  }

  ## Add a column in the first column for the ages
  csv.coh.sum <- append(csv.coh.sum, list(as.data.frame(ages)), after = 0)
  ## Bind the list of cohort value matrices into a single ragged matrix
  n <- max(sapply(csv.coh.sum, nrow))
  suppressWarnings(
    csv.coh.sum.mat <- do.call(cbind,
                               lapply(csv.coh.sum, function(x){
                                 rbind(x, matrix(, n - nrow(x), ncol(x)))}))
  )
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
  suppressWarnings(
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
  )
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
      rep(c(latex.mlc(c("Start",
                        "Biomass",
                        "000s t")),
            latex.mlc(c("Catch",
                        "Weight",
                        "000s t")),
            latex.mlc(c("M",
                        "000s t")),
            latex.mlc(c("Surviving",
                        "Biomass",
                        "000s t"))),
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
                        latex.bold(paste(cohorts[i],
                                         " cohort"))))
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
               size = size.string,
               scalebox = scalebox))
}
