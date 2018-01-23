SS_output <-
function (dir = "C:/myfiles/mymodels/myrun/", model = "ss3", 
    repfile = "Report.sso", compfile = "CompReport.sso", covarfile = "covar.sso", 
    forefile = "Forecast-report.sso", wtfile = "wtatage.ss_new", 
    warnfile = "warning.sso", ncols = 200, forecast = TRUE, warn = TRUE, 
    covar = TRUE, readwt = TRUE, checkcor = TRUE, cormax = 0.95, 
    cormin = 0.01, printhighcor = 10, printlowcor = 10, verbose = TRUE, 
    printstats = TRUE, hidewarn = FALSE, NoCompOK = FALSE, aalmaxbinrange = 4) 
{
    flush.console()
    emptytest <- function(x) {
        sum(!is.na(x) & x == "")/length(x)
    }
    matchfun <- function(string, obj = rawrep[, 1], substr1 = TRUE) {
        match(string, if (substr1) {
            substring(obj, 1, nchar(string))
        }
        else {
            obj
        })
    }
    matchfun2 <- function(string1, adjust1, string2, adjust2, 
        cols = "nonblank", matchcol1 = 1, matchcol2 = 1, objmatch = rawrep, 
        objsubset = rawrep, substr1 = TRUE, substr2 = TRUE, header = FALSE) {
        line1 <- match(string1, if (substr1) {
            substring(objmatch[, matchcol1], 1, nchar(string1))
        }
        else {
            objmatch[, matchcol1]
        })
        line2 <- match(string2, if (substr2) {
            substring(objmatch[, matchcol2], 1, nchar(string2))
        }
        else {
            objmatch[, matchcol2]
        })
        if (is.na(line1) | is.na(line2)) 
            return("absent")
        if (is.numeric(cols)) 
            out <- objsubset[(line1 + adjust1):(line2 + adjust2), 
                cols]
        if (cols[1] == "all") 
            out <- objsubset[(line1 + adjust1):(line2 + adjust2), 
                ]
        if (cols[1] == "nonblank") {
            out <- objsubset[(line1 + adjust1):(line2 + adjust2), 
                ]
            out <- out[, apply(out, 2, emptytest) < 1]
        }
        if (header && nrow(out) > 0) {
            out[1, out[1, ] == ""] <- "NoName"
            names(out) <- out[1, ]
            out <- out[-1, ]
        }
        return(out)
    }
    df.rename <- function(df, oldnames, newnames) {
        for (iname in 1:length(oldnames)) {
            names(df)[names(df) == oldnames[iname]] <- newnames[iname]
        }
        return(df)
    }
    shortrepfile <- repfile
    repfile <- file.path(dir, repfile)
    parfile <- dir(dir, pattern = ".par$")
    if (length(parfile) > 1) {
        filetimes <- file.info(file.path(dir, parfile))$mtime
        parfile <- parfile[filetimes == max(filetimes)][1]
        if (verbose) 
            cat("Multiple files in directory match pattern *.par\n", 
                "choosing most recently modified:", parfile, 
                "\n")
    }
    if (length(parfile) == 0) {
        if (!hidewarn) 
            cat("Some stats skipped because the .par file not found:\n  ", 
                parfile, "\n")
        parfile <- NA
    }
    else {
        parfile <- file.path(dir, parfile)
    }
    if (file.exists(repfile)) {
        if (file.info(repfile)$size > 0) {
            if (verbose) 
                cat("Getting header info from:\n  ", repfile, 
                  "\n")
        }
        else {
            stop("report file is empty: ", repfile)
        }
    }
    else {
        stop("can't find report file: ", repfile)
    }
    rephead <- readLines(con = repfile, n = 15)
    SS_versionCode <- rephead[grep("#V", rephead)]
    SS_version <- rephead[grep("Stock_Synthesis", rephead)]
    SS_version <- SS_version[substring(SS_version, 1, 2) != "#C"]
    if (substring(SS_version, 1, 2) == "#V") {
        SS_version <- substring(SS_version, 3)
    }
    if (substring(SS_version, 1, 4) == "3.30") {
        SS_versionshort <- "3.30"
        SS_versionNumeric <- as.numeric(SS_versionshort)
    }
    else {
        SS_versionshort <- toupper(substr(SS_version, 1, 8))
        SS_versionNumeric <- as.numeric(substring(SS_versionshort, 
            5))
    }
    SS_versionMax <- 3.3
    SS_versionMin <- 3.24
    if (SS_versionNumeric < SS_versionMin | SS_versionNumeric > 
        SS_versionMax) {
        cat("\n! Warning, this function tested on SS-V3.24 through SS-V3.30.07.\n", 
            "  you are using ", substr(SS_version, 1, 9), " which MIGHT NOT WORK with this R code.\n\n", 
            sep = "")
    }
    else {
        if (verbose) 
            cat("! Warning, this function tested on SS-V", SS_versionMin, 
                " through SS-V", SS_versionMax, ".\n", "  you are using ", 
                substr(SS_version, 1, 9), " which SHOULD work with this R code.\n", 
                sep = "")
    }
    findtime <- function(lines) {
        time <- strsplit(lines[grep("ime", lines)], "ime: ")[[1]]
        if (length(time) < 2) 
            return()
        else return(time[2])
    }
    repfiletime <- findtime(rephead)
    if (verbose) 
        cat("Report file time:", repfiletime, "\n")
    corfile <- NA
    if (covar) {
        if (!is.na(parfile)) {
            corfile <- sub(".par", ".cor", parfile, fixed = TRUE)
            if (!file.exists(corfile)) {
                cat("Some stats skipped because the .cor file not found:", 
                  corfile, "\n")
                corfile <- NA
            }
        }
        covarfile <- file.path(dir, covarfile)
        if (!file.exists(covarfile)) {
            warning("covar file not found, input 'covar' changed to FALSE")
            covar <- FALSE
        }
        covarhead <- readLines(con = covarfile, n = 10)
        covarskip <- grep("active-i", covarhead) - 1
        covartime <- findtime(covarhead)
        if (is.null(covartime) || is.null(repfiletime)) {
            cat("problem comparing the file creation times:\n")
            cat("  Report.sso:", repfiletime, "\n")
            cat("  covar.sso:", covartime, "\n")
        }
        else {
            if (covartime != repfiletime) {
                cat("covar time:", covartime, "\n")
                stop(shortrepfile, " and ", covarfile, " were from different model runs. Change input to covar=FALSE")
            }
        }
        nowrite <- grep("do not write", covarhead)
        if (length(nowrite) > 0) {
            warning("covar file contains the warning\n", "     '", 
                covarhead[nowrite], "'\n", "  input 'covar' changed to FALSE.\n")
            covar <- FALSE
        }
    }
    compfile <- file.path(dir, compfile)
    if (file.exists(compfile)) {
        comphead <- readLines(con = compfile, n = 30)
        compskip <- grep("Composition_Database", comphead)
        compend <- grep(" end ", comphead)
        if (length(compend) == 0) 
            compend <- 999
        comptime <- findtime(comphead)
        if (is.null(comptime) || is.null(repfiletime)) {
            cat("problem comparing the file creation times:\n")
            cat("  Report.sso:", repfiletime, "\n")
            cat("  CompReport.sso:", comptime, "\n")
        }
        else {
            if (comptime != repfiletime) {
                cat("CompReport time:", comptime, "\n")
                stop(shortrepfile, " and ", compfile, " were from different model runs.")
            }
        }
        comp <- TRUE
    }
    else {
        if (!NoCompOK) 
            stop("Missing ", compfile, ". Change the compfile input or rerun model to get the file.\n", 
                sep = "")
        else comp <- FALSE
    }
    if (verbose) 
        cat("Reading full report file\n")
    flush.console()
    rawrep <- read.table(file = repfile, col.names = 1:ncols, 
        fill = TRUE, quote = "", colClasses = "character", nrows = -1, 
        comment.char = "")
    nonblanks <- apply(rawrep, 2, emptytest) < 1
    maxnonblank = max(0, (1:ncols)[nonblanks == TRUE])
    if (maxnonblank == ncols) {
        stop("all columns are used and some data may been missed,\n", 
            "  increase 'ncols' input above current value (ncols=", 
            ncols, ")")
    }
    if (verbose) {
        if ((maxnonblank + 1) == ncols) 
            cat("Got all columns.\n")
        if ((maxnonblank + 1) < ncols) 
            cat("Got all columns. To speed code, use ncols=", 
                maxnonblank + 1, " in the future.\n", sep = "")
        cat("Got Report file\n")
    }
    flush.console()
    yielddat <- NA
    if (forecast) {
        forecastname <- file.path(dir, forefile)
        temp <- file.info(forecastname)$size
        if (is.na(temp) | temp == 0) {
            stop("Forecast-report.sso file is empty.\n", "Change input to 'forecast=FALSE' or rerun model with forecast turned on.")
        }
        rawforecast1 <- read.table(file = forecastname, col.names = 1:ncols, 
            fill = TRUE, quote = "", colClasses = "character", 
            nrows = -1)
        sprtarg <- as.numeric(rawforecast1[matchfun("SPR_target", 
            rawforecast1[, 1]), 2])
        btarg <- as.numeric(rawforecast1[matchfun("Btarget", 
            rawforecast1[, 1]), 2])
        endyield <- matchfun("MSY_not_calculated", rawforecast1[, 
            1])
        if (is.na(endyield)) 
            yesMSY <- TRUE
        else yesMSY <- FALSE
        if (yesMSY) 
            endyield <- matchfun("findFmsy", rawforecast1[, 10])
        if (verbose) 
            cat("Got Forecast-report file\n")
        startline <- matchfun("profile", rawforecast1[, 11])
        if (!is.na(startline)) {
            yieldraw <- rawforecast1[(startline + 1):endyield, 
                ]
        }
    }
    else {
        if (verbose) 
            cat("You skipped the forecast file\n", "  setting SPR target and Biomass target to -999\n", 
                "  lines won't be drawn for these targets\n", 
                "  (can replace or override in SS_plots by setting 'sprtarg' and 'btarg')\n")
        sprtarg <- -999
        btarg <- -999
    }
    minbthresh <- -999
    if (!is.na(btarg) & btarg == 0.4) {
        if (verbose) 
            cat("Setting minimum biomass threshhold to 0.25\n", 
                "  based on US west coast assumption associated with biomass target of 0.4.\n", 
                "  (can replace or override in SS_plots by setting 'minbthresh')\n")
        minbthresh <- 0.25
    }
    if (!is.na(btarg) & btarg == 0.25) {
        if (verbose) 
            cat("Setting minimum biomass threshhold to 0.25\n", 
                "  based on US west coast assumption associated with flatfish target of 0.25.\n", 
                "  (can replace or override in SS_plots by setting 'minbthresh')\n")
        minbthresh <- 0.125
    }
    if (SS_versionNumeric >= 3.3) {
        yieldraw <- matchfun2("SPR/YPR_Profile", 1, "Finish", 
            -2)
    }
    else {
        yieldraw <- matchfun2("SPR/YPR_Profile", 1, "Dynamic_Bzero", 
            -2)
    }
    if (yieldraw[[1]][1] == "absent") {
        cat("!warning: Report.sso appears to be early version from before Hessian was estimated.\n", 
            "         equilibrium yield estimates not included in output.\n")
        yieldraw <- NA
    }
    if (!is.na(yieldraw[[1]][1])) {
        names <- yieldraw[1, ]
        names[names == "SSB/Bzero"] <- "Depletion"
        yielddat <- yieldraw[c(2:(as.numeric(length(yieldraw[, 
            1]) - 1))), ]
        names(yielddat) <- names
        for (icol in 1:ncol(yielddat)) {
            yielddat[, icol] <- as.numeric(yielddat[, icol])
        }
        yielddat <- yielddat[order(yielddat$Depletion, decreasing = FALSE), 
            ]
    }
    flush.console()
    logfile <- dir(dir, pattern = ".log$")
    logfile <- logfile[logfile != "fmin.log"]
    if (length(logfile) > 1) {
        filetimes <- file.info(file.path(dir, logfile))$mtime
        logfile <- logfile[filetimes == max(filetimes)]
        if (verbose) 
            cat("Multiple files in directory match pattern *.log\n", 
                "choosing most recently modified file:", logfile, 
                "\n")
    }
    if (length(logfile) == 1 && file.info(file.path(dir, logfile))$size > 
        0) {
        logfile <- read.table(file.path(dir, logfile))[, c(4, 
            6)]
        names(logfile) <- c("TempFile", "Size")
        maxtemp <- max(logfile$Size)
        if (maxtemp == 0) {
            if (verbose) 
                cat("Got log file. There were NO temporary files were written in this run.\n")
        }
        else {
            if (verbose) {
                cat("!warning: temporary files were written in this run:\n")
                print(logfile)
            }
        }
    }
    else {
        logfile <- NA
        if (verbose) 
            cat("No non-empty log file in directory or too many files matching pattern *.log\n")
    }
    if (warn) {
        warnname <- file.path(dir, warnfile)
        if (!file.exists(warnname)) {
            cat(warnfile, "file not found\n")
            nwarn <- NA
            warn <- NA
        }
        else {
            warn <- readLines(warnname, warn = FALSE)
            warnstring <- warn[grep("N warnings: ", warn)]
            if (length(warnstring) > 0) {
                nwarn <- as.numeric(strsplit(warnstring, "N warnings: ")[[1]][2])
                textblock <- c(paste("were", nwarn, "warnings"), 
                  paste("was", nwarn, "warning"))[1 + (nwarn == 
                  1)]
                if (verbose) 
                  cat("Got warning file.\n", " There", textblock, 
                    "in", warnname, "\n")
            }
            else {
                cat(warnfile, "file is missing the string 'N warnings'!\n")
                nwarn <- NA
            }
        }
    }
    else {
        if (verbose) 
            cat("You skipped the warnings file\n")
        nwarn <- NA
    }
    if (verbose) 
        cat("Finished reading files\n")
    flush.console()
    begin <- matchfun("TIME_SERIES") + 2
    end <- matchfun("SPR_series") - 1
    selex <- matchfun2("LEN_SELEX", 6, "AGE_SELEX", -1, header = TRUE)
    selex <- df.rename(selex, oldnames = c("fleet", "year", "seas", 
        "gender", "morph", "label"), newnames = c("Fleet", "Yr", 
        "Seas", "Sex", "Morph", "Label"))
    for (icol in (1:ncol(selex))[!(names(selex) %in% c("Factor", 
        "Label"))]) {
        selex[, icol] <- as.numeric(selex[, icol])
    }
    rawdefs <- matchfun2("DEFINITIONS", 1, "LIKELIHOOD", -1)
    nseasons <- as.numeric(rawdefs[1, 2])
    seasdurations <- as.numeric(rawdefs[3, 1 + 1:nseasons])
    seasfracs <- round(12 * cumsum(seasdurations))/12
    seasfracs <- seasfracs - seasdurations/2
    if (SS_versionNumeric >= 3.3) {
        defs <- rawdefs[-(1:3), apply(rawdefs[-(1:3), ], 2, emptytest) < 
            1]
        defs[defs == ""] <- NA
        FleetNames <- as.character(defs[grep("fleet_names", defs$X1), 
            -1])
        FleetNames <- FleetNames[!is.na(FleetNames)]
        nfleets <- length(FleetNames)
        fleet_ID <- 1:nfleets
        defs <- defs[-(1:3), 1:7]
        names(defs) <- c("fleet_type", "timing", "area", "units", 
            "catch_mult", "survey_units", "survey_error")
        for (icol in 1:ncol(defs)) {
            defs[, icol] <- as.numeric(defs[, icol])
        }
        fleet_type <- defs$fleet_type
        fleet_timing <- defs$timing
        fleet_area <- defs$area
        catch_units <- defs$units
        equ_catch_se <- defs$equ_catch_se
        catch_se <- defs$catch_se
        survey_units <- defs$survey_units
        survey_error <- defs$survey_error
        IsFishFleet <- fleet_type <= 2
    }
    else {
        defs <- rawdefs[-(1:3), apply(rawdefs[-(1:3), ], 2, emptytest) < 
            1]
        defs[defs == ""] <- NA
        lab <- defs$X1
        fleet_ID <- as.numeric(defs[grep("fleet_ID", lab), -1])
        names(defs) <- c("Label", paste("Fleet", fleet_ID, sep = ""))
        FleetNames <- as.character(defs[grep("fleet_names", lab), 
            -1])
        fleet_area <- as.numeric(defs[grep("fleet_area", lab), 
            -1])
        catch_units <- as.numeric(defs[grep("Catch_units", lab), 
            -1])
        catch_error <- as.numeric(defs[grep("Catch_error", lab), 
            -1])
        survey_units <- as.numeric(defs[grep("Survey_units", 
            lab), -1])
        survey_error <- as.numeric(defs[grep("Survey_error", 
            lab), -1])
        IsFishFleet <- !is.na(catch_units)
        nfleets <- length(FleetNames)
    }
    nfishfleets <- sum(IsFishFleet)
    nsexes <- length(unique(as.numeric(selex$Sex)))
    nareas <- max(as.numeric(rawrep[begin:end, 1]))
    startyr <- min(as.numeric(rawrep[begin:end, 2])) + 2
    temptime <- rawrep[begin:end, 2:3]
    endyr <- max(as.numeric(temptime[temptime[, 2] == "TIME", 
        1]))
    tempaccu <- as.character(rawrep[matchfun("Natural_Mortality") + 
        1, -(1:5)])
    accuage <- max(as.numeric(tempaccu[tempaccu != ""]))
    if (SS_versionNumeric >= 3.3) {
        ncpue_column <- 11
        INDEX_1 <- matchfun2("INDEX_1", 1, "INDEX_3", -4, header = TRUE)
        ncpue <- sum(as.numeric(INDEX_1$N), na.rm = TRUE)
    }
    else {
        ncpue_column <- 11
        ncpue <- sum(as.numeric(rawrep[matchfun("INDEX_1") + 
            1 + 1:nfleets, ncpue_column]))
    }
    if (comp) {
        allbins <- read.table(file = compfile, col.names = 1:ncols, 
            fill = TRUE, colClasses = "character", skip = 3, 
            nrows = 15)
        lbins <- as.numeric(allbins[7, -1])
        lbins <- lbins[!is.na(lbins)]
        nlbins <- length(lbins)
        lbinspop <- as.numeric(allbins[3, -1])
        lbinspop <- lbinspop[!is.na(lbinspop)]
        nlbinspop <- length(lbinspop)
        Lbin_method <- as.numeric(allbins[matchfun("Method_for_Lbin_definition", 
            allbins[, 1]), 2])
        if (compend == compskip + 2) {
            cat("It appears that there is no composition data in CompReport.sso\n")
            comp <- FALSE
            agebins <- NA
            sizebinlist <- NA
            nagebins <- length(agebins)
        }
        else {
            if (SS_versionshort == "SS-V3.11") 
                col.names = 1:21
            else col.names = 1:22
            if (SS_versionNumeric >= 3.24) 
                col.names = 1:23
            rawcompdbase <- read.table(file = compfile, col.names = col.names, 
                fill = TRUE, colClasses = "character", skip = compskip, 
                nrows = -1)
            names(rawcompdbase) <- rawcompdbase[1, ]
            names(rawcompdbase)[names(rawcompdbase) == "Used?"] <- "Used"
            endfile <- grep("End_comp_data", rawcompdbase[, 1])
            compdbase <- rawcompdbase[2:(endfile - 2), ]
            compdbase <- df.rename(compdbase, oldnames = c("Pick_gender", 
                "Gender"), newnames = c("Pick_sex", "Sex"))
            compdbase$sex <- compdbase$Pick_sex
            compdbase$sex[compdbase$Pick_sex == 3] <- compdbase$Sex[compdbase$Pick_sex == 
                3]
            if (substr(SS_version, 1, 9) == "SS-V3.24f") {
                if (!hidewarn) 
                  cat("Correcting for bug in tag data output associated with SSv3.24f\n")
                tag1rows <- compdbase$Pick_sex == "TAG1"
                if (any(tag1rows)) {
                  tag1 <- compdbase[tag1rows, ]
                  tag1new <- tag1
                  tag1new[, 4:23] <- tag1new[, 3:22]
                  tag1new$Yr.S <- tag1new$Yr
                  tag1new$Yr <- floor(as.numeric(tag1new$Yr))
                  compdbase[tag1rows, ] <- tag1new
                }
            }
            compdbase <- compdbase[compdbase$Obs != "", ]
            compdbase[compdbase == "_"] <- NA
            compdbase$Used[is.na(compdbase$Used)] <- "yes"
            if (!("SuprPer" %in% names(compdbase))) 
                compdbase$SuprPer <- "No"
            compdbase$SuprPer[is.na(compdbase$SuprPer)] <- "No"
            n <- sum(is.na(compdbase$N) & compdbase$Used != "skip" & 
                compdbase$Kind != "TAG2")
            if (n > 0) {
                cat("Warning:", n, "rows from composition database have NA sample size\n  but are not part of a super-period. (Maybe input as N=0?)\n")
            }
            for (i in (1:ncol(compdbase))[!(names(compdbase) %in% 
                c("Kind", "SuprPer", "Used"))]) compdbase[, i] <- as.numeric(compdbase[, 
                i])
            if (nseasons > 1) 
                compdbase$YrSeasName <- paste(floor(compdbase$Yr), 
                  "s", compdbase$Seas, sep = "")
            else compdbase$YrSeasName <- compdbase$Yr
            if (!"Yr.S" %in% names(compdbase)) {
                if (any(floor(compdbase$Yr) != compdbase$Yr)) {
                  compdbase$Yr.S <- compdbase$Yr
                  compdbase$Yr <- floor(compdbase$Yr)
                }
                else {
                  compdbase$Yr.S <- compdbase$Yr + (0.5/nseasons) * 
                    compdbase$Seas
                }
            }
            compdbase$Lbin_range <- compdbase$Lbin_hi - compdbase$Lbin_lo
            compdbase$Lbin_mid <- 0.5 * (compdbase$Lbin_lo + 
                compdbase$Lbin_hi)
            Lbin_range <- compdbase$Lbin_range
            if (is.null(Lbin_range)) {
                notconditional <- TRUE
                conditional <- FALSE
            }
            else {
                notconditional <- !is.na(Lbin_range) & Lbin_range > 
                  aalmaxbinrange
                conditional <- !is.na(Lbin_range) & Lbin_range <= 
                  aalmaxbinrange
            }
            if (SS_versionNumeric >= 3.22) {
                lendbase <- compdbase[compdbase$Kind == "LEN" & 
                  compdbase$Used != "skip", ]
                sizedbase <- compdbase[compdbase$Kind == "SIZE" & 
                  compdbase$Used != "skip", ]
                agedbase <- compdbase[compdbase$Kind == "AGE" & 
                  compdbase$Used != "skip" & notconditional, 
                  ]
                condbase <- compdbase[compdbase$Kind == "AGE" & 
                  compdbase$Used != "skip" & conditional, ]
            }
            else {
                lendbase <- compdbase[compdbase$Kind == "LEN" & 
                  (compdbase$SuprPer == "Sup" | (!is.na(compdbase$N) & 
                    compdbase$N > 0)), ]
                sizedbase <- compdbase[compdbase$Kind == "SIZE" & 
                  (compdbase$SuprPer == "Sup" | (!is.na(compdbase$N) & 
                    compdbase$N > 0)), ]
                agedbase <- compdbase[compdbase$Kind == "AGE" & 
                  (compdbase$SuprPer == "Sup" | (!is.na(compdbase$N) & 
                    compdbase$N > 0)) & notconditional, ]
                condbase <- compdbase[compdbase$Kind == "AGE" & 
                  (compdbase$SuprPer == "Sup" | (!is.na(compdbase$N) & 
                    compdbase$N > 0)) & conditional, ]
            }
            ghostagedbase <- compdbase[compdbase$Kind == "AGE" & 
                compdbase$Used == "skip" & compdbase$SuprPer == 
                "No" & notconditional, ]
            ghostcondbase <- compdbase[compdbase$Kind == "AGE" & 
                compdbase$Used == "skip" & compdbase$SuprPer == 
                "No" & conditional, ]
            ghostlendbase <- compdbase[compdbase$Kind == "LEN" & 
                compdbase$Used == "skip" & compdbase$SuprPer == 
                "No", ]
            compdbase$Kind[compdbase$Kind == "L@A" & compdbase$Ageerr < 
                0] <- "W@A"
            if (!is.null(sizedbase) && nrow(sizedbase) > 0) {
                sizedbase$bio.or.num = c("bio", "num")[sizedbase$Lbin_lo]
                sizedbase$units = c("kg", "lb", "cm", "in")[sizedbase$Lbin_hi]
                sizedbase$method = sizedbase$Ageerr
                if (any(sizedbase$units %in% c("lb", "in"))) {
                  if (verbose) 
                    cat("Note: converting bins in generalized size comp data in sizedbase\n", 
                      " back to the original units of lbs or inches.\n")
                }
                sizedbase$Bin[sizedbase$units == "lb"] <- sizedbase$Bin[sizedbase$units == 
                  "lb"]/0.4536
                sizedbase$Bin[sizedbase$units == "in"] <- sizedbase$Bin[sizedbase$units == 
                  "in"]/2.54
                sizebinlist <- list()
                for (imethod in 1:max(sizedbase$method)) {
                  tmp <- sort(unique(sizedbase$Bin[sizedbase$method == 
                    imethod]))
                  if (length(tmp) == 0) 
                    tmp <- NULL
                  sizebinlist[[paste("size_method_", imethod, 
                    sep = "")]] <- tmp
                }
            }
            else {
                sizebinlist <- NA
            }
            if (is.null(compdbase$N)) {
                good <- TRUE
            }
            else {
                good <- !is.na(compdbase$N)
            }
            ladbase <- compdbase[compdbase$Kind == "L@A" & good, 
                ]
            wadbase <- compdbase[compdbase$Kind == "W@A" & good, 
                ]
            tagdbase1 <- compdbase[compdbase$Kind == "TAG1", 
                ]
            tagdbase2 <- compdbase[compdbase$Kind == "TAG2", 
                ]
            if (verbose) {
                cat("CompReport file separated by this code as follows (rows = Ncomps*Nbins):\n", 
                  "  ", nrow(lendbase), "rows of length comp data,\n", 
                  "  ", nrow(sizedbase), "rows of generalized size comp data,\n", 
                  "  ", nrow(agedbase), "rows of age comp data,\n", 
                  "  ", nrow(condbase), "rows of conditional age-at-length data,\n", 
                  "  ", nrow(ghostagedbase), "rows of ghost fleet age comp data,\n", 
                  "  ", nrow(ghostcondbase), "rows of ghost fleet conditional age-at-length data,\n", 
                  "  ", nrow(ghostlendbase), "rows of ghost fleet length comp data,\n", 
                  "  ", nrow(ladbase), "rows of mean length at age data,\n", 
                  "  ", nrow(wadbase), "rows of mean weight at age data,\n", 
                  "  ", nrow(tagdbase1), "rows of 'TAG1' comp data, and\n", 
                  "  ", nrow(tagdbase2), "rows of 'TAG2' comp data.\n")
            }
            if (nrow(agedbase) > 0) {
                Lbin_ranges <- as.data.frame(table(agedbase$Lbin_range))
                names(Lbin_ranges)[1] <- "Lbin_hi-Lbin_lo"
                if (length(unique(agedbase$Lbin_range)) > 1) {
                  cat("Warning!: different ranges of Lbin_lo to Lbin_hi found in age comps.\n")
                  print(Lbin_ranges)
                  cat("  consider increasing 'aalmaxbinrange' to designate\n")
                  cat("  some of these data as conditional age-at-length\n")
                }
                agebins <- sort(unique(agedbase$Bin[!is.na(agedbase$Bin)]))
            }
            else {
                agebins <- NA
            }
            nagebins <- length(agebins)
        }
    }
    else {
        lbins <- NA
        nlbins <- NA
        lbinspop <- NA
        nlbinspop <- ncol(selex) - 5
        agebins <- NA
        nagebins <- NA
        Lbin_method <- 2
        sizebinlist <- NA
    }
    endcode <- "SIZEFREQ_TRANSLATION"
    shift <- -1
    if (is.na(matchfun(endcode))) {
        endcode <- "MOVEMENT"
        shift <- -2
    }
    morph_indexing <- matchfun2("MORPH_INDEXING", 1, endcode, 
        shift, cols = 1:9, header = TRUE)
    for (i in 1:ncol(morph_indexing)) morph_indexing[, i] <- as.numeric(morph_indexing[, 
        i])
    morph_indexing <- df.rename(morph_indexing, oldnames = c("Gpattern", 
        "Bseas", "Gender"), newnames = c("GP", "BirthSeason", 
        "Sex"))
    ngpatterns <- max(morph_indexing$GP)
    if (forecast) {
        grab <- rawforecast1[, 1]
        nforecastyears <- as.numeric(rawforecast1[grab %in% c("N_forecast_yrs:"), 
            2])
        nforecastyears <- nforecastyears[1]
    }
    else {
        nforecastyears <- NA
    }
    if (verbose) 
        cat("Finished dimensioning\n")
    flush.console()
    stats <- list()
    stats$SS_version <- SS_version
    stats$SS_versionshort <- SS_versionshort
    stats$SS_versionNumeric <- SS_versionNumeric
    stats$StartTime <- paste(as.character(matchfun2("StartTime", 
        0, "StartTime", 0, cols = 1:6)), collapse = " ")
    stats$RunTime <- paste(as.character(matchfun2("StartTime", 
        2, "StartTime", 2, cols = 4:9)), collapse = " ")
    returndat <- list()
    tempfiles <- matchfun2("Data_File", 0, "Control_File", 0, 
        cols = 1:2)
    stats$Files_used <- paste(c(tempfiles[1, ], tempfiles[2, 
        ]), collapse = " ")
    returndat$Data_File <- tempfiles[1, 2]
    returndat$Control_File <- tempfiles[2, 2]
    stats$Nwarnings <- nwarn
    if (length(warn) > 20) {
        warn <- c(warn[1:20], paste("Note:", length(warn) - 20, 
            "additional lines truncated. Look in", warnfile, 
            "file to see full list."))
    }
    stats$warnings <- warn
    rawlike <- matchfun2("LIKELIHOOD", 2, "Fleet:", -2)
    like <- data.frame(signif(as.numeric(rawlike[, 2]), digits = 7))
    names(like) <- "values"
    rownames(like) <- rawlike[, 1]
    lambdas <- rawlike[, 3]
    lambdas[lambdas == ""] <- NA
    lambdas <- as.numeric(lambdas)
    like$lambdas <- lambdas
    stats$likelihoods_used <- like
    likelihoods_by_fleet <- matchfun2("Fleet:", 0, "Input_Variance_Adjustment", 
        -1, header = TRUE)
    Parm_devs_detail <- NA
    if (length(grep("Parm_devs_detail", likelihoods_by_fleet[, 
        1])) > 0) {
        likelihoods_by_fleet <- matchfun2("Fleet:", 0, "Parm_devs_detail", 
            -1, header = TRUE)
        Parm_devs_detail <- matchfun2("Parm_devs_detail", 1, 
            "Input_Variance_Adjustment", -1, header = TRUE)
    }
    if (length(grep("Tag_Group", likelihoods_by_fleet[, 1])) > 
        0) {
        likelihoods_by_fleet <- matchfun2("Fleet:", 0, "Tag_Group:", 
            -2, header = TRUE)
        likelihoods_by_tag_group <- matchfun2("Tag_Group:", 0, 
            "Input_Variance_Adjustment", -1, header = TRUE)
        likelihoods_by_tag_group[likelihoods_by_tag_group == 
            "_"] <- NA
        for (icol in 2:ncol(likelihoods_by_tag_group)) {
            likelihoods_by_tag_group[, icol] <- as.numeric(likelihoods_by_tag_group[, 
                icol])
        }
        names(likelihoods_by_tag_group) <- c("Label", "ALL", 
            paste0("TagGroup_", names(likelihoods_by_tag_group)[-(1:2)]))
        likelihoods_by_tag_group$Label[1] <- "Tag_Group"
        stats$likelihoods_by_tag_group <- likelihoods_by_tag_group
    }
    likelihoods_by_fleet[likelihoods_by_fleet == "_"] <- NA
    for (icol in 2:ncol(likelihoods_by_fleet)) {
        likelihoods_by_fleet[, icol] <- as.numeric(likelihoods_by_fleet[, 
            icol])
    }
    names(likelihoods_by_fleet) <- c("Label", "ALL", FleetNames)
    labs <- likelihoods_by_fleet$Label
    for (irow in 1:length(labs)) labs[irow] <- substr(labs[irow], 
        1, nchar(labs[irow]) - 1)
    likelihoods_by_fleet$Label <- labs
    stats$likelihoods_by_fleet <- likelihoods_by_fleet
    stats$Parm_devs_detail <- Parm_devs_detail
    if (SS_versionNumeric >= 3.23) 
        shift <- -1
    if (SS_versionNumeric == 3.22) 
        shift <- -2
    if (SS_versionNumeric < 3.22) 
        shift <- -1
    parameters <- matchfun2("PARAMETERS", 1, "DERIVED_QUANTITIES", 
        shift, header = TRUE)
    if (SS_versionNumeric >= 3.23) {
        temp <- tail(parameters, 2)[, 1:3]
        parameters <- parameters[1:(nrow(parameters) - 2), ]
    }
    parameters <- df.rename(parameters, oldnames = c("PR_type", 
        "Prior_Like"), newnames = c("Pr_type", "Pr_Like"))
    parameters[parameters == "_"] <- NA
    parameters[parameters == " "] <- NA
    parameters[parameters == "1.#INF"] <- Inf
    if (SS_versionNumeric >= 3.22) {
        for (i in (1:ncol(parameters))[!(names(parameters) %in% 
            c("Label", "Pr_type", "Status"))]) parameters[, i] <- as.numeric(parameters[, 
            i])
    }
    if (SS_versionNumeric == 3.21) {
        for (i in (1:ncol(parameters))[!(names(parameters) %in% 
            c("Label", "Pr_type", "Status"))]) parameters[, i] <- as.numeric(parameters[, 
            i])
        temp <- names(parameters)
        cat("Note: inserting new 13th column heading in parameters section due to error in Report.sso in SSv3.21f\n")
        temp <- c(temp[1:12], "PR_type_code", temp[-(1:12)])
        temp <- temp[-length(temp)]
        names(parameters) <- temp
    }
    if (SS_versionNumeric <= 3.2) {
        for (i in (1:ncol(parameters))[!(names(parameters) %in% 
            c("Label", "Status"))]) parameters[, i] <- as.numeric(parameters[, 
            i])
    }
    ParmLabels <- parameters$Label
    ParmLabels[duplicated(ParmLabels)] <- paste0(ParmLabels[duplicated(ParmLabels)], 
        "_2")
    rownames(parameters) <- ParmLabels
    activepars <- parameters$Label[!is.na(parameters$Active_Cnt)]
    if (!is.na(parfile)) {
        parline <- read.table(parfile, fill = TRUE, comment.char = "", 
            nrows = 1)
    }
    else {
        parline <- matrix(NA, 1, 16)
    }
    stats$N_estimated_parameters <- parline[1, 6]
    pars <- parameters[!is.na(parameters$Active_Cnt), ]
    if (nrow(pars) > 0) {
        pars$Afterbound <- ""
        pars$checkdiff <- pars$Value - pars$Min
        pars$checkdiff2 <- pars$Max - pars$Value
        pars$checkdiff3 <- abs(pars$Value - (pars$Max - (pars$Max - 
            pars$Min)/2))
        pars$Afterbound[pars$checkdiff < 0.001 | pars$checkdiff2 < 
            0.001 | pars$checkdiff2 < 0.001] <- "CHECK"
        pars$Afterbound[!pars$Afterbound %in% "CHECK"] <- "OK"
    }
    stats$table_of_phases <- table(parameters$Phase)
    estimated_non_dev_parameters <- pars[, names(pars) %in% c("Value", 
        "Phase", "Min", "Max", "Init", "Prior", "Gradient", "Pr_type", 
        "Pr_SD", "Pr_Like", "Parm_StDev", "Status", "Afterbound")]
    devnames <- c("RecrDev", "InitAge", "ForeRecr", "DEVadd", 
        "DEVmult", "DEVrwalk", "DEV_MR_rwalk", "ARDEV")
    devrows <- NULL
    for (iname in 1:length(devnames)) {
        devrows <- unique(c(devrows, grep(devnames[iname], rownames(estimated_non_dev_parameters))))
    }
    if (!is.null(devrows)) {
        estimated_non_dev_parameters <- estimated_non_dev_parameters[-devrows, 
            ]
    }
    stats$estimated_non_dev_parameters <- estimated_non_dev_parameters
    if (covar) {
        CoVar <- read.table(covarfile, header = TRUE, colClasses = c(rep("numeric", 
            4), rep("character", 4), "numeric"), skip = covarskip)
        if (verbose) 
            cat("Got covar file.\n")
        stdtable <- CoVar[CoVar$Par..j == "Std", c(7, 9, 5)]
        names(stdtable) = c("name", "std", "type")
        N_estimated_parameters2 <- sum(stdtable$type == "Par")
        if (is.na(stats$N_estimated_parameters)) {
            stats$N_estimated_parameters <- N_estimated_parameters2
        }
        else {
            if (stats$N_estimated_parameters != N_estimated_parameters2) {
                cat("!warning:\n")
                cat(" ", stats$N_estimated_parameters, "estimated parameters indicated by", 
                  parfile, "\n")
                cat(" ", N_estimated_parameters2, "estimated parameters shown in", 
                  covarfile, "\n")
                cat("  returning the first value,", stats$N_estimated_parameters, 
                  "\n")
                stats$N_estimated_parameters <- stats$N_estimated_parameters
            }
        }
        Nstd <- sum(stdtable$std > 0)
        checkbadrun <- unique(stdtable$std)
        if (length(checkbadrun) == 1) {
            if (checkbadrun %in% c(NA, "NaN", "na")) {
                stop(paste0("No quantities were estimated in the covar file \nand all", 
                  "estimates of standard deviation are ", checkbadrun, 
                  ". \nTry re-running", "stock synthesis."))
            }
        }
        if (Nstd <= 1) {
            stop("Too few estimated quantities in covar file (n=", 
                Nstd, "). Change input to covar=FALSE.")
        }
        if (checkcor == TRUE & stats$N_estimated_parameters > 
            1) {
            corfilter <- CoVar[CoVar$all.i != CoVar$all.j & CoVar$Par..i == 
                "Par" & CoVar$Par..j == "Par" & CoVar$label.i %in% 
                activepars & CoVar$label.j %in% activepars & 
                !substr(CoVar$label.i, 1, 8) == "ForeRecr" & 
                !substr(CoVar$label.j, 1, 8) == "ForeRecr", ]
            rangecor <- range(abs(corfilter$corr))
            corstats <- list()
            corstats$cormessage1 <- paste("Range of abs(parameter correlations) is", 
                min(rangecor), "to", max(rangecor))
            highcor <- corfilter[abs(corfilter$corr) >= cormax, 
                names(CoVar) %in% c("label.i", "label.j", "corr")]
            lowcorcandidates <- corfilter[abs(corfilter$corr) <= 
                cormin, names(CoVar) %in% c("label.i", "label.j", 
                "corr")]
            lowcortestlist <- data.frame(unique(c(lowcorcandidates$label.i, 
                lowcorcandidates$label.j)))
            lowcortestlist$name <- as.character(lowcortestlist[, 
                1])
            nlowcor <- 0
            lowcor <- 0
            if (nrow(lowcortestlist) > 0) {
                lowcortestlist$max <- NA
                for (i in 1:length(lowcortestlist[, 1])) {
                  lowcortestlist$max[i] <- max(corfilter$corr[corfilter$label.i == 
                    lowcortestlist$name[i]], corfilter$corr[corfilter$label.j == 
                    lowcortestlist$name[i]])
                }
                lowcor <- lowcortestlist[abs(lowcortestlist$max) <= 
                  cormin, 2:3]
                nlowcor <- nrow(lowcor)
            }
            nhighcor <- nrow(highcor)
            if (printhighcor > 0) {
                if (nhighcor == 0) 
                  textblock <- "No correlations"
                if (nhighcor == 1) 
                  textblock <- "1 correlation"
                if (nhighcor > 1) 
                  textblock <- paste(nhighcor, "correlations")
                corstats$cormessage2 <- paste(textblock, " above threshold (cormax=", 
                  cormax, ")", sep = "")
                if (nhighcor > 0 & nhighcor <= printhighcor) {
                  row.names(highcor) = paste("   ", 1:nhighcor)
                  corstats$cormessage3 <- highcor
                }
                if (nhighcor > 0 & nhighcor > printhighcor) {
                  highcorsub <- highcor[order(-abs(highcor$corr)), 
                    ]
                  highcorsub <- highcorsub[1:printhighcor, ]
                  row.names(highcorsub) <- paste("   ", 1:printhighcor)
                  corstats$cormessage4 <- paste("Highest", printhighcor, 
                    "parameter correlations above threshold (to print more, increase 'printhighcor' input):")
                  corstats$cormessage5 <- highcorsub
                }
            }
            else {
                corstats$cormessage6 <- "High correlations not reported. To report, change 'printhighcor' input to a positive value."
            }
            if (printlowcor > 0) {
                if (nlowcor == 0) 
                  textblock <- "No uncorrelated parameters"
                if (nlowcor == 1) 
                  textblock <- "1 uncorrelation"
                if (nlowcor > 1) 
                  textblock <- paste(nlowcor, "uncorrelated parameters")
                corstats$cormessage7 <- paste(textblock, " below threshold (cormin=", 
                  cormin, ")", sep = "")
                if (nlowcor > 0 & nlowcor <= printlowcor) {
                  corstats$cormessage8 <- lowcor
                }
                if (nlowcor > 0 & nlowcor > printlowcor) {
                  lowcorsub <- lowcor[order(abs(lowcor$max)), 
                    ]
                  lowcorsub <- lowcorsub[1:printlowcor, ]
                  corstats$cormessage9 <- paste("Lowest", printlowcor, 
                    "parameters uncorrelations below threshold (to print more, increase 'printlowcor' input):")
                  corstats$cormessage10 <- lowcorsub
                }
            }
            else {
                corstats$cormessage11 <- "Uncorrelated parameters not reported. To report, change 'printlowcor' input to a positive value."
            }
        }
        else {
            corstats <- NA
            if (verbose) {
                cat("You skipped the correlation check (or have only 1 parameter)\n")
            }
        }
    }
    else {
        if (verbose) {
            cat("You skipped the covar file\n")
        }
    }
    flush.console()
    wtatage <- NULL
    if (readwt) {
        wtfile <- file.path(dir, wtfile)
        if (!file.exists(wtfile) | file.info(wtfile)$size == 
            0) {
            if (verbose) 
                cat("Skipping weight-at-age file. File missing or empty:", 
                  wtfile, "\n")
        }
        else {
          wtatagelines <- readLines(wtfile, n = 20)
            wtatage <- read.table(wtfile, header = FALSE, comment.char = "", 
                skip = grep("Yr Seas Sex", wtatagelines), 
                stringsAsFactors = FALSE)
            wtatage_names <- c("yr", "seas", "gender", "growpattern", 
                "birthseas", "fleet", 0:accuage)
            if (SS_versionNumeric >= 3.3 & ncol(wtatage) == length(wtatage_names) + 
                1) {
                wtatage_names <- c(wtatage_names, "comment")
            }
            names(wtatage) <- wtatage_names
        }
    }
    der <- matchfun2("DERIVED_QUANTITIES", 4, "MGparm_By_Year_after_adjustments", 
        -1, header = TRUE)
    der <- df.rename(der, oldnames = "LABEL", newnames = "Label")
    der <- der[der$Label != "Bzero_again", ]
    der[der == "_"] <- NA
    der[der == ""] <- NA
    test <- grep("Parm_dev_details", der$Label)
    if (length(test) > 0) {
        der <- der[1:(min(test) - 1), ]
    }
    for (i in 2:ncol(der)) {
        der[, i] = as.numeric(der[, i])
    }
    rownames(der) <- der$Label
    managementratiolabels <- matchfun2("DERIVED_QUANTITIES", 
        1, "DERIVED_QUANTITIES", 3, cols = 1:2)
    names(managementratiolabels) <- c("Ratio", "Label")
    MGparmAdj <- matchfun2("MGparm_By_Year_after_adjustments", 
        1, "selparm(Size)_By_Year_after_adjustments", -1, header = TRUE)
    MGparmAdj <- df.rename(MGparmAdj, oldnames = "Year", newnames = "Yr")
    if (nrow(MGparmAdj) > 0) {
        for (icol in 1:ncol(MGparmAdj)) MGparmAdj[, icol] <- as.numeric(MGparmAdj[, 
            icol])
    }
    else {
        MGparmAdj <- NA
    }
    SelSizeAdj <- matchfun2("selparm(Size)_By_Year_after_adjustments", 
        2, "selparm(Age)_By_Year_after_adjustments", -1)
    if (nrow(SelSizeAdj) > 2) {
        SelSizeAdj <- SelSizeAdj[, apply(SelSizeAdj, 2, emptytest) < 
            1]
        SelSizeAdj[SelSizeAdj == ""] <- NA
        for (icol in 1:ncol(SelSizeAdj)) SelSizeAdj[, icol] <- as.numeric(SelSizeAdj[, 
            icol])
        names(SelSizeAdj) <- c("FleetSvy", "Yr", paste("Par", 
            1:(ncol(SelSizeAdj) - 2), sep = ""))
    }
    else {
        SelSizeAdj <- NA
    }
    SelAgeAdj <- matchfun2("selparm(Age)_By_Year_after_adjustments", 
        2, "RECRUITMENT_DIST", -1)
    if (nrow(SelAgeAdj) > 2) {
        SelAgeAdj <- SelAgeAdj[, apply(SelAgeAdj, 2, emptytest) < 
            1]
        SelAgeAdj[SelAgeAdj == ""] <- NA
        if (SelAgeAdj[1, 1] == "RECRUITMENT_DIST") {
            SelAgeAdj <- NA
        }
        else {
            for (icol in 1:ncol(SelAgeAdj)) SelAgeAdj[, icol] <- as.numeric(SelAgeAdj[, 
                icol])
            names(SelAgeAdj) <- c("FleetSvy", "Yr", paste("Par", 
                1:(ncol(SelAgeAdj) - 2), sep = ""))
        }
    }
    else {
        SelAgeAdj <- NA
    }
    recruitment_dist <- matchfun2("RECRUITMENT_DIST", 1, "MORPH_INDEXING", 
        -1, header = TRUE)
    if (length(grep("RECRUITMENT_DIST", recruitment_dist[, 1])) == 
        0) {
        for (i in 1:6) {
            recruitment_dist[, i] <- as.numeric(recruitment_dist[, 
                i])
        }
    }
    else {
        if (length(grep("RECRUITMENT_DIST_BENCHMARK", recruitment_dist[, 
            1])) > 0) {
            recruitment_dist <- matchfun2("RECRUITMENT_DIST", 
                0, "MORPH_INDEXING", -1, header = FALSE)
            rd <- list()
            rd.line.top <- 1
            rd.line.bench <- grep("RECRUITMENT_DIST_BENCHMARK", 
                recruitment_dist[, 1])
            rd.line.fore <- grep("RECRUITMENT_DIST_FORECAST", 
                recruitment_dist[, 1])
            rd.line.end <- nrow(recruitment_dist)
            rd$recruit_dist_endyr <- recruitment_dist[(rd.line.top + 
                1):(rd.line.bench - 1), ]
            rd$recruit_dist_benchmarks <- recruitment_dist[(rd.line.bench + 
                1):(rd.line.fore - 1), ]
            rd$recruit_dist_forecast <- recruitment_dist[(rd.line.fore + 
                1):(rd.line.end), ]
        }
        if (length(grep("RECRUITMENT_DIST_Bmark", recruitment_dist[, 
            1])) > 0) {
            recruitment_dist <- matchfun2("RECRUITMENT_DIST", 
                0, "MORPH_INDEXING", -1, header = FALSE)
            rd <- list()
            rd.line.top <- 1
            rd.line.Bmark <- grep("RECRUITMENT_DIST_Bmark", recruitment_dist[, 
                1])
            rd.line.endyr <- grep("RECRUITMENT_DIST_endyr", recruitment_dist[, 
                1])
            rd.line.end <- nrow(recruitment_dist)
            rd$recruit_dist <- recruitment_dist[(rd.line.top + 
                1):(rd.line.Bmark - 1), ]
            rd$recruit_dist_Bmark <- recruitment_dist[(rd.line.Bmark + 
                1):(rd.line.endyr - 1), ]
            rd$recruit_dist_endyr <- recruitment_dist[(rd.line.endyr + 
                1):(rd.line.end), ]
        }
        for (i in 1:length(rd)) {
            tmp <- rd[[i]]
            names(tmp) <- tmp[1, ]
            tmp <- tmp[-1, ]
            for (icol in 1:6) tmp[, icol] <- as.numeric(tmp[, 
                icol])
            rd[[i]] <- tmp
        }
        recruitment_dist <- rd
    }
    if (covar & !is.na(corfile)) {
        stats$log_det_hessian <- read.table(corfile, nrows = 1)[1, 
            10]
    }
    stats$maximum_gradient_component <- as.numeric(matchfun2("Convergence_Level", 
        0, "Convergence_Level", 0, cols = 2))
    if ("Gradient" %in% names(parameters)) {
        if (any(!is.na(parameters$Gradient))) {
            ngrads <- min(5, max(parameters$Active_Cnt, na.rm = TRUE))
            stats$parameters_with_highest_gradients <- head(parameters[order(abs(parameters$Gradient), 
                decreasing = TRUE), c("Value", "Gradient")], 
                n = 5)
        }
    }
    if (SS_versionNumeric >= 3.3 | substring(SS_version, 1, 9) %in% 
        paste0("SS-V3.24", LETTERS[21:26]) | substring(SS_version, 
        1, 10) %in% paste0("SS-V3.24A", LETTERS)) {
        last_row_index <- 11
    }
    else {
        last_row_index <- 10
    }
    srhead <- matchfun2("SPAWN_RECRUIT", 0, "SPAWN_RECRUIT", 
        last_row_index, cols = 1:6)
    rmse_table <- as.data.frame(srhead[-(1:(last_row_index - 
        1)), 1:5])
    for (icol in 2:5) {
        rmse_table[, icol] <- as.numeric(rmse_table[, icol])
    }
    names(rmse_table) <- srhead[last_row_index - 1, 1:5]
    names(rmse_table)[4] <- "RMSE_over_sigmaR"
    sigma_R_in <- as.numeric(srhead[last_row_index - 6, 1])
    rmse_table <- rmse_table
    biascol <- grep("breakpoints_for_bias", srhead)
    breakpoints_for_bias_adjustment_ramp <- srhead[grep("breakpoints_for_bias", 
        srhead[, biascol]), 1:5]
    colnames(breakpoints_for_bias_adjustment_ramp) <- c("last_yr_early", 
        "first_yr_full", "last_yr_full", "first_yr_recent", "max_bias_adj")
    rownames(breakpoints_for_bias_adjustment_ramp) <- NULL
    raw_recruit <- matchfun2("SPAWN_RECRUIT", last_row_index + 
        1, "INDEX_2", -1, cols = 1:9)
    names(raw_recruit) <- raw_recruit[1, ]
    raw_recruit[raw_recruit == "_"] <- NA
    raw_recruit <- raw_recruit[-(1:2), ]
    recruit <- raw_recruit[-(1:2), ]
    for (i in 1:(ncol(recruit) - 1)) recruit[, i] <- as.numeric(recruit[, 
        i])
    recruit <- df.rename(recruit, oldnames = c("year", "spawn_bio", 
        "adjusted"), newnames = c("Yr", "SpawnBio", "bias_adjusted"))
    vartune <- matchfun2("INDEX_1", 1, "INDEX_1", (nfleets + 
        1), cols = 1:21, header = TRUE)
    if (SS_versionNumeric >= 3.3) {
        fit_len_comps <- matchfun2("FIT_LEN_COMPS", 1, "Length_Comp_Fit_Summary", 
            -1, header = TRUE)
    }
    else {
        fit_len_comps <- NULL
    }
    if (!is.null(dim(fit_len_comps)) && nrow(fit_len_comps) > 
        0) {
        fit_len_comps[fit_len_comps == "_"] <- NA
        for (icol in which(!names(fit_len_comps) %in% "Use")) {
            fit_len_comps[, icol] <- as.numeric(fit_len_comps[, 
                icol])
        }
    }
    else {
        fit_len_comps <- NULL
    }
    if (SS_versionNumeric < 3.3) {
        lenntune <- matchfun2("FIT_AGE_COMPS", -(nfleets + 1), 
            "FIT_AGE_COMPS", -1, cols = 1:10, header = TRUE)
        names(lenntune)[10] <- "FleetName"
        lenntune <- lenntune[lenntune$N > 0, c(10, 1, 4:9)]
        lenntune$"MeaneffN/MeaninputN"[lenntune$"MeaneffN/MeaninputN" == 
            "-1.#IND"] <- NA
        for (icol in 2:ncol(lenntune)) lenntune[, icol] <- as.numeric(lenntune[, 
            icol])
        lenntune$"HarMean/MeanInputN" <- lenntune$"HarMean(effN)"/lenntune$"mean(inputN*Adj)"
    }
    else {
        lenntune <- matchfun2("Length_Comp_Fit_Summary", 1, "FIT_AGE_COMPS", 
            -1, header = TRUE)
        lenntune <- lenntune[lenntune$N > 0, ]
        for (icol in 1:8) {
            lenntune[, icol] <- as.numeric(lenntune[, icol])
        }
        lenntune$"HarMean(effN)/mean(inputN*Adj)" <- lenntune$HarMean/lenntune$"mean_inputN*Adj"
        lenntune <- df.rename(lenntune, oldnames = c("HarMean", 
            "mean_inputN*Adj"), newnames = c("HarMean(effN)", 
            "mean(inputN*Adj)"))
        lenntune <- lenntune[, names(lenntune) != "mean_effN"]
        end.names <- c("Recommend_Var_Adj", "FleetName")
        lenntune <- lenntune[, c(which(!names(lenntune) %in% 
            end.names), which(names(lenntune) %in% end.names))]
    }
    stats$Length_comp_Eff_N_tuning_check <- lenntune
    if (SS_versionNumeric < 3.3) {
        fit_age_comps <- matchfun2("FIT_AGE_COMPS", 1, "FIT_SIZE_COMPS", 
            -(nfleets + 2), header = TRUE)
    }
    else {
        fit_age_comps <- matchfun2("FIT_AGE_COMPS", 1, "Age_Comp_Fit_Summary", 
            -1, header = TRUE)
    }
    if (!is.null(dim(fit_age_comps)) && nrow(fit_age_comps) > 
        0) {
        fit_age_comps[fit_age_comps == "_"] <- NA
        for (icol in which(!names(fit_age_comps) %in% "Use")) {
            fit_age_comps[, icol] <- as.numeric(fit_age_comps[, 
                icol])
        }
    }
    else {
        fit_age_comps <- NULL
    }
    if (SS_versionNumeric < 3.3) {
        agentune <- matchfun2("FIT_SIZE_COMPS", -(nfleets + 1), 
            "FIT_SIZE_COMPS", -1, cols = 1:10, header = TRUE)
    }
    else {
        agentune <- matchfun2("Age_Comp_Fit_Summary", 1, "FIT_SIZE_COMPS", 
            -1, header = TRUE)
    }
    if (!is.null(dim(agentune))) {
        names(agentune)[ncol(agentune)] <- "FleetName"
        agentune <- agentune[agentune$N > 0, ]
        agentune$"MeaneffN/MeaninputN"[agentune$"MeaneffN/MeaninputN" == 
            "-1.#IND"] <- NA
        for (icol in which(!names(agentune) %in% "FleetName")) {
            agentune[, icol] <- as.numeric(agentune[, icol])
        }
        agentune$"HarMean(effN)/mean(inputN*Adj)" <- agentune$"HarMean(effN)"/agentune$"mean(inputN*Adj)"
        agentune$Recommend_Var_Adj <- agentune$Var_Adj * agentune$"HarMean(effN)/mean(inputN*Adj)"
        badnames <- c("mean_effN", "Mean(effN/inputN)", "MeaneffN/MeaninputN")
        agentune <- agentune[, !names(agentune) %in% badnames]
        agentune <- agentune[, c(which(names(agentune) != "FleetName"), 
            which(names(agentune) == "FleetName"))]
        agentune <- df.rename(agentune, oldnames = c("Var_Adj"), 
            newnames = c("Curr_Var_Adj"))
    }
    else {
        agentune <- NULL
    }
    stats$Age_comp_Eff_N_tuning_check <- agentune
    fit_size_comps <- NULL
    if (SS_versionNumeric >= 3.3) {
        fit_size_comps <- matchfun2("FIT_SIZE_COMPS", 1, "Size_Comp_Fit_Summary", 
            -(nfleets + 2), header = TRUE)
    }
    if (!is.null(dim(fit_size_comps)) && nrow(fit_size_comps) > 
        0) {
        fit_size_comps[fit_size_comps == "_"] <- NA
        for (icol in which(!names(fit_size_comps) %in% "Use")) {
            fit_size_comps[, icol] <- as.numeric(fit_size_comps[, 
                icol])
        }
    }
    if (SS_versionNumeric >= 3.3) {
        sizentune <- matchfun2("Size_Comp_Fit_Summary", 1, "OVERALL_COMPS", 
            -1, cols = 1:10, header = TRUE)
        if (!is.null(dim(sizentune))) {
            sizentune[, 1] <- sizentune[, 10]
            sizentune <- sizentune[sizentune$Npos > 0, c(1, 3, 
                4, 5, 6, 8, 9)]
        }
        else {
            sizentune <- NULL
        }
        stats$Size_comp_Eff_N_tuning_check <- sizentune
    }
    if (verbose) 
        cat("Finished primary run statistics list\n")
    flush.console()
    if (SS_versionNumeric <= 3.24) {
        returndat$definitions <- defs
        returndat$fleet_ID <- fleet_ID
        returndat$fleet_area <- fleet_area
        returndat$catch_units <- catch_units
        returndat$catch_error <- catch_error
    }
    if (SS_versionNumeric >= 3.3) {
        returndat$definitions <- defs
        returndat$fleet_ID <- fleet_ID
        returndat$fleet_type <- fleet_type
        returndat$fleet_timing <- fleet_timing
        returndat$fleet_area <- fleet_area
        returndat$catch_units <- catch_units
        returndat$catch_se <- catch_se
        returndat$equ_catch_se <- equ_catch_se
    }
    returndat$survey_units <- survey_units
    returndat$survey_error <- survey_error
    returndat$index_variance_tuning_check <- vartune
    returndat$IsFishFleet <- IsFishFleet
    returndat$nfishfleets <- nfishfleets
    returndat$nfleets <- nfleets
    returndat$nsexes <- nsexes
    returndat$ngpatterns <- ngpatterns
    returndat$lbins <- lbins
    returndat$Lbin_method <- Lbin_method
    returndat$nlbins <- nlbins
    returndat$lbinspop <- lbinspop
    returndat$nlbinspop <- nlbinspop
    returndat$sizebinlist <- sizebinlist
    returndat$agebins <- agebins
    returndat$nagebins <- nagebins
    returndat$accuage <- accuage
    returndat$nareas <- nareas
    returndat$startyr <- startyr
    returndat$endyr <- endyr
    returndat$nseasons <- nseasons
    returndat$seasfracs <- seasfracs
    returndat$seasdurations <- seasdurations
    returndat$nforecastyears <- nforecastyears
    returndat$morph_indexing <- morph_indexing
    returndat$MGparmAdj <- MGparmAdj
    returndat$SelSizeAdj <- SelSizeAdj
    returndat$SelAgeAdj <- SelAgeAdj
    returndat$recruitment_dist <- recruitment_dist
    returndat$recruit <- recruit
    returndat$breakpoints_for_bias_adjustment_ramp <- breakpoints_for_bias_adjustment_ramp
    begin <- matchfun("N_Used_morphs", rawrep[, 6]) + 1
    rawbio <- rawrep[begin:(begin + nlbinspop), 1:10]
    rawbio <- rawbio[, apply(rawbio, 2, emptytest) < 1]
    names(rawbio) <- rawbio[1, ]
    biology <- rawbio[-1, ]
    for (i in 1:ncol(biology)) biology[, i] <- as.numeric(biology[, 
        i])
    FecType <- 0
    pl <- parameters$Label
    FecGrep1 <- grep("Eggs/kg_slope_wt_Fem", pl)
    FecGrep2 <- grep("Eggs_exp_len_Fem", pl)
    FecGrep3 <- grep("Eggs_exp_wt_Fem", pl)
    FecGrep4 <- grep("Eggs_slope_len_Fem", pl)
    FecGrep5 <- grep("Eggs_slope_Wt_Fem", pl)
    if (length(FecGrep1) > 0) {
        FecType <- 1
        FecPar1name <- grep("Eggs/kg_inter_Fem", pl, value = TRUE)[1]
        FecPar2name <- pl[FecGrep1[1]]
    }
    if (length(FecGrep2) > 0) {
        FecType <- 2
        FecPar1name <- grep("Eggs_scalar_Fem", pl, value = TRUE)[1]
        FecPar2name <- pl[FecGrep2[1]]
    }
    if (length(FecGrep3) > 0) {
        FecType <- 3
        FecPar1name <- grep("Eggs_scalar_Fem", pl, value = TRUE)[1]
        FecPar2name <- pl[FecGrep3[1]]
    }
    if (length(FecGrep4) > 0) {
        FecType <- 4
        FecPar1name <- grep("Eggs_intercept_Fem", pl, value = TRUE)[1]
        FecPar2name <- pl[FecGrep4[1]]
    }
    if (length(FecGrep5) > 0) {
        FecType <- 5
        FecPar1name <- grep("Eggs_intercept_Fem", pl, value = TRUE)[1]
        FecPar2name <- pl[FecGrep5[1]]
    }
    if (is.na(lbinspop[1])) {
        lbinspop <- biology$Low[biology$GP == 1]
    }
    returndat$biology <- biology
    returndat$FecType <- FecType
    returndat$FecPar1name <- FecPar1name
    returndat$FecPar2name <- FecPar2name
    returndat$FecPar1 <- parameters$Value[parameters$Label == 
        FecPar1name]
    returndat$FecPar2 <- parameters$Value[parameters$Label == 
        FecPar2name]
    if (length(returndat$FecPar1) > 1) {
        warning("Plots will only show fecundity and related quantities for Growth Pattern 1")
        returndat$FecPar1 <- returndat$FecPar1[1]
        returndat$FecPar2 <- returndat$FecPar2[2]
    }
    returndat$SpawnOutputUnits <- ifelse(!is.na(biology$Fecundity[1]) && 
        all(biology$Wt_len_F == biology$Fecundity), "biomass", 
        "numbers")
    Growth_Parameters <- matchfun2("Growth_Parameters", 1, "Growth_Parameters", 
        1 + nrow(morph_indexing), header = TRUE)
    for (icol in 1:ncol(Growth_Parameters)) {
        Growth_Parameters[, icol] <- as.numeric(Growth_Parameters[, 
            icol])
    }
    returndat$Growth_Parameters <- Growth_Parameters
    Seas_Effects <- matchfun2("Seas_Effects", 1, "Biology_at_age_in_endyr", 
        -1, header = TRUE)
    if (Seas_Effects[[1]][1] != "absent") {
        for (i in 1:ncol(Seas_Effects)) Seas_Effects[, i] <- as.numeric(Seas_Effects[, 
            i])
    }
    else {
        Seas_Effects <- NA
    }
    returndat$Seas_Effects <- Seas_Effects
    growthCVtype <- matchfun2("Biology_at_age", 0, "Biology_at_age", 
        0, header = FALSE)
    if (nchar(growthCVtype) > 31) {
        returndat$growthCVtype <- substring(growthCVtype, 30)
    }
    else {
        returndat$growthCVtype <- "unknown"
    }
    growdat <- matchfun2("Biology_at_age", 1, "MEAN_BODY_WT(begin)", 
        -1, header = TRUE)
    growdat <- df.rename(growdat, oldnames = c("Gender"), newnames = c("Sex"))
    for (i in 1:ncol(growdat)) {
        growdat[, i] <- as.numeric(growdat[, i])
    }
    nmorphs <- max(growdat$Morph)
    midmorphs <- c(c(0, nmorphs/nsexes) + ceiling(nmorphs/nsexes/2))
    returndat$endgrowth <- growdat
    test <- matchfun2("MEAN_BODY_WT(begin)", 0, "MEAN_BODY_WT(begin)", 
        0, header = FALSE)
    wtatage_switch <- length(grep("wtatage.ss", test)) > 0
    returndat$wtatage_switch <- wtatage_switch
    mean_body_wt <- matchfun2("MEAN_BODY_WT(begin)", 1, "MEAN_SIZE_TIMESERIES", 
        -1, header = TRUE)
    for (i in 1:ncol(mean_body_wt)) mean_body_wt[, i] <- as.numeric(mean_body_wt[, 
        i])
    returndat$mean_body_wt <- mean_body_wt
    rawgrow <- matchfun2("MEAN_SIZE_TIMESERIES", 1, "mean_size_Jan_1", 
        -1, cols = 1:(4 + accuage + 1))
    growthvaries <- FALSE
    if (length(rawgrow) > 1) {
        names(rawgrow) <- rawgrow[1, ]
        growdat <- rawgrow[-1, ]
        for (i in 1:ncol(growdat)) growdat[, i] <- as.numeric(growdat[, 
            i])
        if (SS_versionNumeric < 3.3) {
            growdat <- growdat[growdat$Beg == 1 & growdat$Yr >= 
                startyr & growdat$Yr < endyr, ]
        }
        else {
            growdat <- growdat[growdat$SubSeas == 1 & growdat$Yr >= 
                startyr & growdat$Yr < endyr, ]
        }
        if (nseasons > 1) {
            growdat <- growdat[growdat$Seas == 1, ]
        }
        if (length(unique(growdat$Yr)) > 1) {
            growthvaries <- TRUE
        }
        returndat$growthseries <- growdat
        returndat$growthvaries <- growthvaries
    }
    if (!forecast) 
        selex <- selex[selex$Yr <= endyr, ]
    returndat$sizeselex <- selex
    if (!is.na(matchfun("ENVIRONMENTAL_DATA"))) {
        ageselex <- matchfun2("AGE_SELEX", 4, "ENVIRONMENTAL_DATA", 
            -1, header = TRUE)
    }
    else if (!is.na(matchfun("TAG_Recapture"))) {
        ageselex <- matchfun2("AGE_SELEX", 4, "TAG_Recapture", 
            -1, header = TRUE)
    }
    else if (!is.na(matchfun("NUMBERS_AT_AGE")) && matchfun("NUMBERS_AT_AGE") < 
        matchfun("BIOLOGY")) {
        ageselex <- matchfun2("AGE_SELEX", 4, "NUMBERS_AT_AGE", 
            -1, header = TRUE)
    }
    else {
        ageselex <- matchfun2("AGE_SELEX", 4, "BIOLOGY", -1, 
            header = TRUE)
    }
    ageselex <- df.rename(ageselex, oldnames = c("fleet", "year", 
        "seas", "gender", "morph", "label", "factor"), newnames = c("Fleet", 
        "Yr", "Seas", "Sex", "Morph", "Label", "Factor"))
    if (!forecast) 
        ageselex <- ageselex[ageselex$Yr <= endyr, ]
    for (icol in (1:ncol(ageselex))[!(names(ageselex) %in% c("Factor", 
        "Label"))]) ageselex[, icol] <- as.numeric(ageselex[, 
        icol])
    returndat$ageselex <- ageselex
    exploitation <- matchfun2("EXPLOITATION", 5, "CATCH", -1, 
        header = TRUE)
    exploitation[exploitation == "_"] <- NA
    exploitation$Yr[exploitation$Yr == "init_yr"] <- startyr - 
        1
    for (icol in 1:ncol(exploitation)) {
        exploitation[, icol] <- as.numeric(exploitation[, icol])
    }
    returndat$exploitation <- exploitation
    catch <- matchfun2("CATCH", 1, "TIME_SERIES", -1, substr1 = FALSE, 
        header = TRUE)
    if (catch[[1]][1] != "absent") {
        catch$Like[catch$Like == "-1.#IND"] <- NA
        catch$Yr[catch$Yr == "init"] <- startyr - 1
        for (icol in (1:ncol(catch))[!(names(catch) %in% c("Name"))]) {
            catch[, icol] <- as.numeric(catch[, icol])
        }
    }
    else {
        catch <- NULL
    }
    returndat$catch <- catch
    timeseries <- matchfun2("TIME_SERIES", 1, "SPR_series", -1, 
        header = TRUE)
    timeseries <- timeseries[timeseries$Seas != "recruits", ]
    timeseries[timeseries == "_"] <- NA
    for (i in (1:ncol(timeseries))[names(timeseries) != "Era"]) timeseries[, 
        i] = as.numeric(timeseries[, i])
    returndat$timeseries <- timeseries
    spawnseas <- unique(timeseries$Seas[!is.na(timeseries$SpawnBio)])
    if (length(spawnseas) == 0) {
        spawnseas <- NA
    }
    returndat$spawnseas <- spawnseas
    if ("recruit_dist_endyr" %in% names(recruitment_dist)) {
        rd <- recruitment_dist$recruit_dist_endyr
    }
    else {
        rd <- recruitment_dist
    }
    if (SS_versionNumeric >= 3.3) {
        temp <- morph_indexing[morph_indexing$BirthSeason == 
            min(rd$Seas[rd$"Frac/sex" > 0]) & morph_indexing$Platoon_Dist == 
            max(morph_indexing$Platoon_Dist), ]
        mainmorphs <- min(temp$Index[temp$Sex == 1])
        if (nsexes == 2) {
            mainmorphs <- c(mainmorphs, min(temp$Index[temp$Sex == 
                2]))
        }
    }
    if (SS_versionNumeric < 3.3) {
        temp <- morph_indexing[morph_indexing$BirthSeason == 
            min(rd$Seas[rd$Value > 0]) & morph_indexing$Sub_Morph_Dist == 
            max(morph_indexing$Sub_Morph_Dist), ]
        mainmorphs <- min(temp$Index[temp$Sex == 1])
        if (nsexes == 2) {
            mainmorphs <- c(mainmorphs, min(temp$Index[temp$Sex == 
                2]))
        }
    }
    if (length(mainmorphs) == 0) {
        cat("!Error with morph indexing in SS_output function.\n")
    }
    returndat$mainmorphs <- mainmorphs
    birthseas <- sort(unique(timeseries$Seas[timeseries$Recruit_0 > 
        0]))
    if (length(birthseas) == 0) {
        birthseas <- sort(unique(morph_indexing$BirthSeason))
    }
    returndat$birthseas <- birthseas
    timeseries$Yr <- timeseries$Yr + (timeseries$Seas - 1)/nseasons
    ts <- timeseries[timeseries$Yr <= endyr + 1, ]
    tsyears <- ts$Yr[ts$Seas == 1]
    tsspaw_bio <- ts$SpawnBio[ts$Seas == spawnseas & ts$Area == 
        1]
    if (nareas > 1) {
        for (a in 2:nareas) {
            tsspaw_bio <- tsspaw_bio + ts$SpawnBio[ts$Seas == 
                spawnseas & ts$Area == a]
        }
    }
    if (nsexes == 1) {
        tsspaw_bio <- tsspaw_bio/2
    }
    depletionseries <- tsspaw_bio/tsspaw_bio[1]
    stats$SBzero <- tsspaw_bio[1]
    stats$current_depletion <- depletionseries[length(depletionseries)]
    ls <- nrow(ts) - 1
    totretainedmat <- as.matrix(ts[, substr(names(ts), 1, nchar("retain(B)")) == 
        "retain(B)"])
    ts$totretained <- 0
    ts$totretained[3:ls] <- rowSums(totretainedmat)[3:ls]
    totcatchmat <- as.matrix(ts[, substr(names(ts), 1, nchar("enc(B)")) == 
        "enc(B)"])
    ts$totcatch <- 0
    ts$totcatch[3:ls] <- rowSums(totcatchmat)[3:ls]
    F_method <- as.numeric(rawrep[matchfun("F_Method"), 2])
    returndat$F_method <- F_method
    if (F_method == 1) {
        stringmatch <- "Hrate:_"
    }
    else {
        stringmatch <- "F:_"
    }
    Hrates <- as.matrix(ts[, substr(names(ts), 1, nchar(stringmatch)) == 
        stringmatch])
    fmax <- max(Hrates)
    depletion_method <- as.numeric(rawrep[matchfun("Depletion_method"), 
        2])
    depletion_basis <- rawrep[matchfun("B_ratio_denominator"), 
        2]
    if (depletion_basis == "no_depletion_basis") {
        depletion_basis <- "none"
    }
    else {
        depletion_basis <- as.numeric(strsplit(depletion_basis, 
            "%*", fixed = TRUE)[[1]][1])/100
    }
    returndat$depletion_method <- depletion_method
    returndat$depletion_basis <- depletion_basis
    if (SS_versionNumeric < 3.2) {
        DF_discard <- rawrep[matchfun("DISCARD_OUTPUT"), 3]
        if (length(grep("T_distribution", DF_discard)) > 0) 
            DF_discard <- as.numeric(strsplit(DF_discard, "=_")[[1]][2])
        if (length(grep("_normal_with_Std_in_as_CV", DF_discard)) > 
            0) 
            DF_discard <- 0
        if (length(grep("_normal_with_Std_in_as_stddev", DF_discard)) > 
            0) 
            DF_discard <- -1
        if (length(grep("_lognormal", DF_discard)) > 0) 
            DF_discard <- -2
        shift <- 2
        discard_spec <- NULL
    }
    else {
        DF_discard <- NA
        shift <- 1
        discard_spec <- matchfun2("DISCARD_SPECIFICATION", 9, 
            "DISCARD_OUTPUT", -2, cols = 1:3, header = TRUE)
        if (length(grep("trunc_normal", names(discard_spec))) > 
            0) {
            discard_spec <- matchfun2("DISCARD_SPECIFICATION", 
                10, "DISCARD_OUTPUT", -2, cols = 1:3, header = TRUE)
        }
        for (icol in 1:3) {
            discard_spec[, icol] <- as.numeric(discard_spec[, 
                icol])
        }
        names(discard_spec)[1] <- "Fleet"
    }
    discard <- matchfun2("DISCARD_OUTPUT", shift, "MEAN_BODY_WT_OUTPUT", 
        -1, header = TRUE)
    if (names(discard)[1] == "MEAN_BODY_WT_OUTPUT") {
        discard <- NA
    }
    if (!is.na(discard) && names(discard)[1] != "Fleet") {
        discard <- matchfun2("DISCARD_OUTPUT", shift, "MEAN_BODY_WT_OUTPUT", 
            -1, header = FALSE)
        names(discard) <- c("Fleet", "Yr", "Seas", "Obs", "Exp", 
            "Std_in", "Std_use", "Dev")
    }
    discard_type <- NA
    if (!is.na(discard) && nrow(discard) > 1) {
        discard[discard == "_"] <- NA
        if (SS_versionNumeric <= 3.23) {
            for (icol in (1:ncol(discard))[!(names(discard) %in% 
                c("Fleet"))]) discard[, icol] <- as.numeric(discard[, 
                icol])
            discard$Fleet <- NA
            if (!"Name" %in% names(discard)) 
                discard$Name <- discard$Fleet
            for (i in 1:nrow(discard)) {
                discard$Fleet[i] <- strsplit(discard$Name[i], 
                  "_")[[1]][1]
                discard$Name[i] <- substring(discard$Name[i], 
                  nchar(discard$Fleet[i]) + 2)
            }
        }
        else {
            for (icol in (1:ncol(discard))[!(names(discard) %in% 
                c("Name", "SuprPer"))]) discard[, icol] <- as.numeric(discard[, 
                icol])
        }
    }
    else {
        discard <- NA
    }
    returndat$discard <- discard
    returndat$discard_type <- discard_type
    returndat$DF_discard <- DF_discard
    returndat$discard_spec <- discard_spec
    DF_mnwgt <- rawrep[matchfun("log(L)_based_on_T_distribution"), 
        1]
    if (!is.na(DF_mnwgt)) {
        DF_mnwgt <- as.numeric(strsplit(DF_mnwgt, "=_")[[1]][2])
        mnwgt <- matchfun2("MEAN_BODY_WT_OUTPUT", 2, "FIT_LEN_COMPS", 
            -1, header = TRUE)
        mnwgt[mnwgt == "_"] <- NA
        if (SS_versionNumeric <= 3.23) {
            for (icol in (1:ncol(mnwgt))[!(names(mnwgt) %in% 
                c("Fleet"))]) mnwgt[, icol] <- as.numeric(mnwgt[, 
                icol])
            mnwgt$Fleet <- NA
            for (i in 1:nrow(mnwgt)) {
                mnwgt$Fleet[i] <- strsplit(mnwgt$Fleet[i], "_")[[1]][1]
                mnwgt$Name[i] <- substring(mnwgt$Fleet[i], nchar(mnwgt$Fleet[i]) + 
                  2)
            }
        }
        else {
            for (icol in (1:ncol(mnwgt))[!(names(mnwgt) %in% 
                c("Name"))]) mnwgt[, icol] <- as.numeric(mnwgt[, 
                icol])
        }
    }
    else {
        DF_mnwgt <- NA
        mnwgt <- NA
    }
    returndat$mnwgt <- mnwgt
    returndat$DF_mnwgt <- DF_mnwgt
    spr <- matchfun2("SPR_series", 5, "SPAWN_RECRUIT", -1, header = TRUE)
    if (length(grep("Kobe_Plot", rawrep[, 1])) != 0) {
        shift <- -3
        if (SS_versionNumeric < 3.23) 
            shift <- -1
        spr <- matchfun2("SPR_series", 5, "Kobe_Plot", shift, 
            header = TRUE)
        Kobe_head <- matchfun2("Kobe_Plot", 0, "Kobe_Plot", 3, 
            header = TRUE)
        shift <- 1
        Kobe_warn <- NA
        Kobe_MSY_basis <- NA
        if (length(grep("_basis_is_not", Kobe_head[1, 1])) > 
            0) {
            shift <- shift + 1
            Kobe_warn <- Kobe_head[1, 1]
        }
        if (length(grep("MSY_basis", Kobe_head[2, 1])) > 0) {
            shift <- shift + 1
            Kobe_MSY_basis <- Kobe_head[2, 1]
        }
        Kobe <- matchfun2("Kobe_Plot", shift, "SPAWN_RECRUIT", 
            -1, header = TRUE)
        Kobe[Kobe == "_"] <- NA
        Kobe[Kobe == "1.#INF"] <- NA
        Kobe[Kobe == "-1.#IND"] <- NA
        names(Kobe) <- gsub("/", ".", names(Kobe), fixed = TRUE)
        for (icol in 1:3) {
            Kobe[, icol] <- as.numeric(Kobe[, icol])
        }
    }
    else {
        Kobe <- NA
        Kobe_warn <- NA
        Kobe_MSY_basis <- NA
    }
    returndat$Kobe_warn <- Kobe_warn
    returndat$Kobe_MSY_basis <- Kobe_MSY_basis
    returndat$Kobe <- Kobe
    names(spr) <- gsub(pattern = "SPB", replacement = "SSB", 
        names(spr))
    spr <- df.rename(spr, oldnames = c("Year", "spawn_bio", "SPR_std", 
        "Y/R", "F_std"), newnames = c("Yr", "SpawnBio", "SPR_report", 
        "YPR", "F_report"))
    spr[spr == "_"] <- NA
    spr[spr == "&"] <- NA
    spr[spr == "-1.#IND"] <- NA
    for (i in (1:ncol(spr))[!(names(spr) %in% c("Actual:", "More_F(by_morph):"))]) {
        spr[, i] <- as.numeric(spr[, i])
    }
    spr$spr <- spr$SPR
    returndat$sprseries <- spr
    stats$last_years_SPR <- spr$spr[nrow(spr)]
    stats$SPRratioLabel <- managementratiolabels[1, 2]
    stats$last_years_SPRratio <- spr$SPR_std[nrow(spr)]
    returndat$managementratiolabels <- managementratiolabels
    returndat$F_report_basis <- managementratiolabels$Label[2]
    returndat$B_ratio_denominator <- as.numeric(strsplit(managementratiolabels$Label[3], 
        "%")[[1]][1])/100
    returndat$sprtarg <- sprtarg
    returndat$btarg <- btarg
    if (!is.na(btarg) & btarg == 0.4 & startyr == 1966 & sprtarg == 
        0.4 & accuage == 20 & wtatage_switch) {
        if (verbose) 
            cat("Setting minimum biomass threshhold to 0.10 because this looks like hake\n", 
                "  (can replace or override in SS_plots by setting 'minbthresh')\n")
        minbthresh <- 0.1
    }
    returndat$minbthresh <- minbthresh
    if (!is.na(yielddat[[1]][1])) {
        returndat$equil_yield <- yielddat
    }
    else {
        if (verbose) 
            cat("You skipped the equilibrium yield data\n")
    }
    flush.console()
    if (ncpue > 0) {
        cpue <- matchfun2("INDEX_2", 1, "INDEX_2", ncpue + 1, 
            header = TRUE)
        cpue[cpue == "_"] <- NA
        cpue <- df.rename(cpue, oldnames = c("Yr.S", "Supr_Per"), 
            newnames = c("Yr.frac", "SuprPer"))
        if (SS_versionNumeric < 3.24) {
            cpue$Name <- NA
            for (i in 1:nrow(cpue)) {
                cpue$Fleet[i] <- strsplit(cpue$Fleet[i], "_")[[1]][1]
                cpue$Name[i] <- substring(cpue$Fleet[i], nchar(cpue$Fleet[i]) + 
                  2)
            }
        }
        for (i in (1:ncol(cpue))[!names(cpue) %in% c("Name", 
            "SuprPer")]) {
            cpue[, i] <- as.numeric(cpue[, i])
        }
    }
    else {
        cpue <- NA
    }
    returndat$cpue <- cpue
    if (SS_versionNumeric >= 3.3) {
        rawnatage <- matchfun2("NUMBERS_AT_AGE", 1, "BIOMASS_AT_AGE", 
            -1, cols = 1:(13 + accuage), substr1 = FALSE)
    }
    else {
        rawnatage <- matchfun2("NUMBERS_AT_AGE", 1, "NUMBERS_AT_LENGTH", 
            -1, cols = 1:(12 + accuage), substr1 = FALSE)
    }
    if (length(rawnatage) > 1) {
        names(rawnatage) <- rawnatage[1, ]
        rawnatage <- rawnatage[-1, ]
        rawnatage <- df.rename(rawnatage, oldnames = c("Gender", 
            "BirthSeas", "SubMorph"), newnames = c("Sex", "BirthSeason", 
            "Platoon"))
        for (i in (1:ncol(rawnatage))[!(names(rawnatage) %in% 
            c("Beg/Mid", "Era"))]) {
            rawnatage[, i] = as.numeric(rawnatage[, i])
        }
        returndat$natage <- rawnatage
    }
    if (SS_versionNumeric >= 3.3) {
        batage <- matchfun2("BIOMASS_AT_AGE", 1, "NUMBERS_AT_LENGTH", 
            -1, cols = 1:(13 + accuage), substr1 = FALSE)
    }
    else {
        batage <- NULL
    }
    if (length(batage) > 1) {
        names(batage) <- batage[1, ]
        batage <- batage[-1, ]
        for (i in (1:ncol(batage))[!(names(batage) %in% c("Beg/Mid", 
            "Era"))]) {
            batage[, i] = as.numeric(batage[, i])
        }
        returndat$batage <- batage
    }
    col.adjust <- 12
    if (SS_versionNumeric < 3.3) {
        col.adjust <- 11
    }
    if (length(grep("BIOMASS_AT_LENGTH", rawrep[, 1])) == 0) {
        rawnatlen <- matchfun2("NUMBERS_AT_LENGTH", 1, "CATCH_AT_AGE", 
            -1, cols = 1:(col.adjust + nlbinspop), substr1 = FALSE)
    }
    else {
        rawnatlen <- matchfun2("NUMBERS_AT_LENGTH", 1, "BIOMASS_AT_LENGTH", 
            -1, cols = 1:(col.adjust + nlbinspop), substr1 = FALSE)
    }
    if (length(rawnatlen) > 1) {
        names(rawnatlen) <- rawnatlen[1, ]
        rawnatlen <- rawnatlen[-1, ]
        rawnatlen <- df.rename(rawnatlen, oldnames = c("Gender", 
            "BirthSeas", "SubMorph"), newnames = c("Sex", "BirthSeason", 
            "Platoon"))
        for (i in (1:ncol(rawnatlen))[!(names(rawnatlen) %in% 
            c("Beg/Mid", "Era"))]) {
            rawnatlen[, i] = as.numeric(rawnatlen[, i])
        }
        returndat$natlen <- rawnatlen
    }
    if (length(grep("BIOMASS_AT_LENGTH", rawrep[, 1])) > 0) {
        rawbatlen <- matchfun2("BIOMASS_AT_LENGTH", 1, "CATCH_AT_AGE", 
            -1, cols = 1:(col.adjust + nlbinspop), substr1 = FALSE)
        if (length(rawbatlen) > 1) {
            names(rawbatlen) <- rawbatlen[1, ]
            rawbatlen <- rawbatlen[-1, ]
            for (i in (1:ncol(rawbatlen))[!(names(rawbatlen) %in% 
                c("Beg/Mid", "Era"))]) {
                rawbatlen[, i] = as.numeric(rawbatlen[, i])
            }
            returndat$batlen <- rawbatlen
        }
    }
    movement <- matchfun2("MOVEMENT", 1, "EXPLOITATION", -1, 
        cols = 1:(7 + accuage), substr1 = FALSE)
    names(movement) <- c(movement[1, 1:6], paste("age", movement[1, 
        -(1:6)], sep = ""))
    movement <- movement[-1, ]
    for (i in 1:ncol(movement)) movement[, i] <- as.numeric(movement[, 
        i])
    returndat$movement <- movement
    tagreportrates <- matchfun2("Reporting_Rates_by_Fishery", 
        1, "See_composition_data_output_for_tag_recapture_details", 
        -1, cols = 1:3)
    if (tagreportrates[[1]][1] != "absent") {
        names(tagreportrates) <- tagreportrates[1, ]
        tagreportrates <- tagreportrates[-1, ]
        for (i in 1:ncol(tagreportrates)) tagreportrates[, i] <- as.numeric(tagreportrates[, 
            i])
        returndat$tagreportrates <- tagreportrates
    }
    else {
        returndat$tagreportrates <- NA
    }
    tagrecap <- matchfun2("TAG_Recapture", 1, "Tags_Alive", -1, 
        cols = 1:10)
    if (tagrecap[[1]][1] != "absent") {
        tagfirstperiod <- as.numeric(tagrecap[1, 1])
        tagaccumperiod <- as.numeric(tagrecap[2, 1])
        names(tagrecap) <- tagrecap[4, ]
        tagrecap <- tagrecap[-(1:4), ]
        for (i in 1:ncol(tagrecap)) tagrecap[, i] <- as.numeric(tagrecap[, 
            i])
        returndat$tagrecap <- tagrecap
        returndat$tagfirstperiod <- tagfirstperiod
        returndat$tagaccumperiod <- tagaccumperiod
    }
    else {
        returndat$tagrecap <- NA
        returndat$tagfirstperiod <- NA
        returndat$tagaccumperiod <- NA
    }
    tagsalive <- matchfun2("Tags_Alive", 1, "Total_recaptures", 
        -1, cols = 1:ncols)
    if (tagsalive[[1]][1] != "absent") {
        tagcols <- max((1:ncols)[apply(tagsalive, 2, function(x) {
            any(x != "")
        })])
        tagsalive <- tagsalive[, 1:tagcols]
        names(tagsalive) <- c("TG", paste("period", 0:(tagcols - 
            2), sep = ""))
        for (i in 1:ncol(tagsalive)) tagsalive[, i] <- as.numeric(tagsalive[, 
            i])
        returndat$tagsalive <- tagsalive
    }
    else {
        returndat$tagsalive <- NA
    }
    tagtotrecap <- matchfun2("Total_recaptures", 1, "Reporting_Rates_by_Fishery", 
        -1, cols = 1:ncols)
    if (tagtotrecap[[1]][1] != "absent") {
        tagcols <- max((1:ncols)[apply(tagtotrecap, 2, function(x) {
            any(x != "")
        })])
        tagtotrecap <- tagtotrecap[, 1:tagcols]
        names(tagtotrecap) <- c("TG", paste("period", 0:(tagcols - 
            2), sep = ""))
        for (i in 1:ncol(tagtotrecap)) tagtotrecap[, i] <- as.numeric(tagtotrecap[, 
            i])
        returndat$tagtotrecap <- tagtotrecap
    }
    else {
        returndat$tagtotrecap <- NA
    }
    rawALK <- matchfun2("AGE_LENGTH_KEY", 4, "AGE_AGE_KEY", -1, 
        cols = 1:(accuage + 2))
    if (length(rawALK) > 1 & rawALK[[1]][1] != "absent" && length(grep("AGE_AGE_KEY", 
        rawALK[, 1])) == 0) {
        morph_col <- 5
        if (SS_versionNumeric < 3.3 & length(grep("Sub_Seas", 
            rawALK[, 3])) == 0) {
            morph_col <- 3
        }
        starts <- grep("Morph:", rawALK[, morph_col]) + 2
        ends <- grep("mean", rawALK[, 1]) - 1
        N_ALKs <- length(starts)
        ALK <- array(NA, c(nlbinspop, accuage + 1, N_ALKs))
        dimnames(ALK) <- list(Length = lbinspop, TrueAge = 0:accuage, 
            Matrix = 1:N_ALKs)
        for (i in 1:N_ALKs) {
            ALKtemp <- rawALK[starts[i]:ends[i], -1]
            for (icol in 1:(accuage + 1)) {
                ALKtemp[, icol] <- as.numeric(ALKtemp[, icol])
            }
            ALK[, , i] <- as.matrix(ALKtemp)
            Matrix.Info <- rawALK[starts[i] - 2, ]
            Matrix.Info <- Matrix.Info[Matrix.Info != ""]
            dimnames(ALK)$Matrix[i] <- paste(Matrix.Info, collapse = " ")
        }
        returndat$ALK <- ALK
    }
    rawAAK <- matchfun2("AGE_AGE_KEY", 1, "SELEX_database", -1, 
        cols = 1:(accuage + 2))
    if (length(rawAAK) > 1) {
        starts <- grep("KEY:", rawAAK[, 1])
        returndat$N_ageerror_defs <- N_ageerror_defs <- length(starts)
        if (N_ageerror_defs > 0) {
            nrowsAAK <- nrow(rawAAK)/N_ageerror_defs - 3
            AAK = array(NA, c(N_ageerror_defs, nrowsAAK, accuage + 
                1))
            age_error_mean <- age_error_sd <- data.frame(age = 0:accuage)
            for (i in 1:N_ageerror_defs) {
                AAKtemp <- rawAAK[starts[i] + 2 + 1:nrowsAAK, 
                  -1]
                rownames.tmp <- rawAAK[starts[i] + 2 + 1:nrowsAAK, 
                  1]
                for (icol in 1:(accuage + 1)) AAKtemp[, icol] <- as.numeric(AAKtemp[, 
                  icol])
                AAK[i, , ] <- as.matrix(AAKtemp)
                age_error_mean[[paste("type", i, sep = "")]] <- as.numeric((rawAAK[starts[i] + 
                  1, -1]))
                age_error_sd[[paste("type", i, sep = "")]] <- as.numeric((rawAAK[starts[i] + 
                  2, -1]))
            }
            if (!is.null(AAK)) {
                dimnames(AAK) <- list(AgeingErrorType = 1:N_ageerror_defs, 
                  ObsAgeBin = rownames.tmp, TrueAge = 0:accuage)
            }
            returndat$AAK <- AAK
            returndat$age_error_mean <- age_error_mean
            returndat$age_error_sd <- age_error_sd
        }
    }
    catage <- matchfun2("CATCH_AT_AGE", 1, "BIOLOGY", -1)
    if (catage[[1]][1] == "absent") {
        catage <- NA
        cat("! Warning: no catch-at-age numbers because 'detailed age-structured reports'\n", 
            "          is turned off in starter file.\n")
    }
    else {
        catage <- catage[, apply(catage, 2, emptytest) < 1]
        names(catage) <- catage[1, ]
        catage <- catage[-1, ]
        for (icol in (1:ncol(catage))[substr(names(catage), 1, 
            2) != "XX" & names(catage) != "Era"]) {
            catage[, icol] <- as.numeric(catage[, icol])
        }
    }
    returndat$catage <- catage
    if (!is.na(matchfun("Z_AT_AGE"))) {
        Z_at_age <- matchfun2("Z_AT_AGE_Annual_2", 1, "Spawning_Biomass_Report_1", 
            -2, header = TRUE)
        M_at_age <- matchfun2("Z_AT_AGE_Annual_1", 1, "-ln(Nt+1", 
            -1, matchcol2 = 5, header = TRUE)
        if (nrow(Z_at_age) > 0) {
            Z_at_age[Z_at_age == "_"] <- NA
            M_at_age[M_at_age == "_"] <- NA
            Z_at_age[Z_at_age == "-1.#INF"] <- NA
            M_at_age[M_at_age == "-1.#INF"] <- NA
            if (Z_at_age[[1]][1] != "absent" && nrow(Z_at_age > 
                0)) {
                for (i in 1:ncol(Z_at_age)) Z_at_age[, i] <- as.numeric(Z_at_age[, 
                  i])
                for (i in 1:ncol(M_at_age)) M_at_age[, i] <- as.numeric(M_at_age[, 
                  i])
            }
            else {
                Z_at_age <- NA
                M_at_age <- NA
            }
        }
    }
    else {
        Z_at_age <- NA
        M_at_age <- NA
    }
    returndat$Z_at_age <- Z_at_age
    returndat$M_at_age <- M_at_age
    Dynamic_Bzero1 <- matchfun2("Spawning_Biomass_Report_2", 
        1, "NUMBERS_AT_AGE_Annual_2", -1)
    Dynamic_Bzero2 <- matchfun2("Spawning_Biomass_Report_1", 
        1, "NUMBERS_AT_AGE_Annual_1", -1)
    if (Dynamic_Bzero1[[1]][1] == "absent") {
        Dynamic_Bzero <- NA
    }
    else {
        Dynamic_Bzero <- cbind(Dynamic_Bzero1, Dynamic_Bzero2[, 
            3])
        names(Dynamic_Bzero) <- c("Yr", "Era", "SPB", "SPB_nofishing")
        if (nareas == 1 & ngpatterns == 1) {
            Dynamic_Bzero <- Dynamic_Bzero[-(1:2), ]
            for (icol in c(1, 3, 4)) Dynamic_Bzero[, icol] <- as.numeric(as.character(Dynamic_Bzero[, 
                icol]))
            names(Dynamic_Bzero) <- c("Yr", "Era", "SPB", "SPB_nofishing")
        }
        if (nareas > 1 & ngpatterns == 1) {
            Dynamic_Bzero <- cbind(Dynamic_Bzero1, Dynamic_Bzero2[, 
                -(1:2)])
            Dynamic_Bzero <- Dynamic_Bzero[-(1:2), ]
            for (icol in (1:ncol(Dynamic_Bzero))[-2]) {
                Dynamic_Bzero[, icol] <- as.numeric(as.character(Dynamic_Bzero[, 
                  icol]))
            }
            names(Dynamic_Bzero) <- c("Yr", "Era", paste0("SPB_area", 
                1:nareas), paste0("SPB_nofishing_area", 1:nareas))
            Dynamic_Bzero$SPB <- apply(Dynamic_Bzero[, 2 + 1:nareas], 
                1, sum)
            Dynamic_Bzero$SPB_nofishing <- apply(Dynamic_Bzero[, 
                2 + nareas + 1:nareas], 1, sum)
        }
    }
    returndat$Dynamic_Bzero <- Dynamic_Bzero
    if (comp) {
        returndat$comp_data_exists <- TRUE
        returndat$lendbase <- lendbase
        returndat$sizedbase <- sizedbase
        returndat$agedbase <- agedbase
        returndat$condbase <- condbase
        returndat$ghostagedbase <- ghostagedbase
        returndat$ghostcondbase <- ghostcondbase
        returndat$ghostlendbase <- ghostlendbase
        returndat$ladbase <- ladbase
        returndat$wadbase <- wadbase
        returndat$tagdbase1 <- tagdbase1
        returndat$tagdbase2 <- tagdbase2
    }
    else {
        returndat$comp_data_exists <- FALSE
    }
    returndat$len_comp_fit_table <- fit_len_comps
    returndat$age_comp_fit_table <- fit_age_comps
    returndat$size_comp_fit_table <- fit_size_comps
    returndat$derived_quants <- der
    returndat$parameters <- parameters
    returndat$FleetNames <- FleetNames
    returndat$repfiletime <- repfiletime
    returndat$SRRtype <- as.numeric(rawrep[matchfun("SPAWN_RECRUIT"), 
        3])
    SPB_final_Label <- paste0("SPB_", endyr + 1)
    if (SPB_final_Label %in% der$Label) {
        SPB_final_EST <- der$Value[der$Label == SPB_final_Label]
        SPB_final_SD <- der$StdDev[der$Label == SPB_final_Label]
        returndat$Pstar_sigma <- sqrt(log((SPB_final_SD/SPB_final_EST)^2 + 
            1))
    }
    else {
        returndat$Pstar_sigma <- NULL
    }
    if (covar) {
        returndat$CoVar <- CoVar
        if (stats$N_estimated_parameters > 1) {
            returndat$highcor <- highcor
            returndat$lowcor <- lowcor
            returndat$corstats <- corstats
        }
        returndat$stdtable <- stdtable
    }
    recdevEarly <- parameters[substring(parameters$Label, 1, 
        13) == "Early_RecrDev", ]
    early_initage <- parameters[substring(parameters$Label, 1, 
        13) == "Early_InitAge", ]
    main_initage <- parameters[substring(parameters$Label, 1, 
        12) == "Main_InitAge", ]
    recdev <- parameters[substring(parameters$Label, 1, 12) == 
        "Main_RecrDev", ]
    recdevFore <- parameters[substring(parameters$Label, 1, 8) == 
        "ForeRecr", ]
    recdevLate <- parameters[substring(parameters$Label, 1, 12) == 
        "Late_RecrDev", ]
    recruitpars <- NULL
    if (nrow(early_initage) > 0) {
        early_initage$type <- "Early_InitAge"
        early_initage$Yr <- startyr - as.numeric(substring(early_initage$Label, 
            15))
        recruitpars <- rbind(recruitpars, early_initage)
    }
    if (nrow(recdevEarly) > 0) {
        recdevEarly$type <- "Early_RecrDev"
        recdevEarly$Yr <- as.numeric(substring(recdevEarly$Label, 
            15))
        recruitpars <- rbind(recruitpars, recdevEarly)
    }
    if (nrow(main_initage) > 0) {
        main_initage$type <- "Main_InitAge"
        main_initage$Yr <- startyr - as.numeric(substring(main_initage$Label, 
            14))
        recruitpars <- rbind(recruitpars, main_initage)
    }
    if (nrow(recdev) > 0) {
        recdev$type <- "Main_RecrDev"
        recdev$Yr <- as.numeric(substring(recdev$Label, 14))
        recruitpars <- rbind(recruitpars, recdev)
    }
    if (nrow(recdevFore) > 0) {
        recdevFore$type <- "ForeRecr"
        recdevFore$Yr <- as.numeric(substring(recdevFore$Label, 
            10))
        recruitpars <- rbind(recruitpars, recdevFore)
    }
    if (nrow(recdevLate) > 0) {
        recdevLate$type <- "Late_RecrDev"
        recdevLate$Yr <- as.numeric(substring(recdevLate$Label, 
            14))
        recruitpars <- rbind(recruitpars, recdevLate)
    }
    if (!is.null(recruitpars)) {
        recruitpars <- recruitpars[order(recruitpars$Yr), c("Value", 
            "Parm_StDev", "type", "Yr")]
    }
    returndat$recruitpars <- recruitpars
    if (is.null(recruitpars)) {
        sigma_R_info <- NULL
    }
    else {
        sigma_R_info <- data.frame(period = c("Main", "Early+Main", 
            "Early+Main+Late"), N_devs = 0, SD_of_devs = NA, 
            Var_of_devs = NA, mean_SE = NA, mean_SEsquared = NA)
        subset <- recruitpars$type %in% c("Main_InitAge", "Main_RecrDev")
        within_period <- sigma_R_info$period == "Main"
        sigma_R_info$N_devs[within_period] <- sum(subset)
        sigma_R_info$SD_of_devs[within_period] <- sd(recruitpars$Value[subset])
        sigma_R_info$mean_SE[within_period] <- mean(recruitpars$Parm_StDev[subset])
        sigma_R_info$mean_SEsquared[within_period] <- mean((recruitpars$Parm_StDev[subset])^2)
        subset <- recruitpars$type %in% c("Early_RecrDev", "Early_InitAge", 
            "Main_InitAge", "Main_RecrDev")
        within_period <- sigma_R_info$period == "Early+Main"
        sigma_R_info$N_devs[within_period] <- sum(subset)
        sigma_R_info$SD_of_devs[within_period] <- sd(recruitpars$Value[subset])
        sigma_R_info$mean_SE[within_period] <- mean(recruitpars$Parm_StDev[subset])
        sigma_R_info$mean_SEsquared[within_period] <- mean((recruitpars$Parm_StDev[subset])^2)
        subset <- recruitpars$type %in% c("Early_RecrDev", "Early_InitAge", 
            "Main_InitAge", "Main_RecrDev", "Late_RecrDev")
        within_period <- sigma_R_info$period == "Early+Main+Late"
        sigma_R_info$N_devs[within_period] <- sum(subset)
        sigma_R_info$SD_of_devs[within_period] <- sd(recruitpars$Value[subset])
        sigma_R_info$mean_SE[within_period] <- mean(recruitpars$Parm_StDev[subset])
        sigma_R_info$mean_SEsquared[within_period] <- mean((recruitpars$Parm_StDev[subset])^2)
        sigma_R_info$Var_of_devs <- sigma_R_info$SD_of_devs^2
        sigma_R_info$sqrt_sum_of_components <- sqrt(sigma_R_info$Var_of_devs + 
            sigma_R_info$mean_SEsquared)
        sigma_R_info$SD_of_devs_over_sigma_R <- sigma_R_info$SD_of_devs/sigma_R_in
        sigma_R_info$sqrt_sum_over_sigma_R <- sigma_R_info$sqrt_sum_of_components/sigma_R_in
        sigma_R_info$alternative_sigma_R <- sigma_R_in * sigma_R_info$sqrt_sum_over_sigma_R
    }
    stats$sigma_R_in <- sigma_R_in
    stats$sigma_R_info <- sigma_R_info
    stats$rmse_table <- rmse_table
    RecrDistpars <- parameters[substring(parameters$Label, 1, 
        8) == "RecrDist", ]
    returndat$RecrDistpars <- RecrDistpars
    returndat$wtatage <- wtatage
    returndat <- c(returndat, stats)
    if (printstats) {
        cat("Statistics shown below (to turn off, change input to printstats=FALSE)\n")
        stats$likelihoods_used <- format(stats$likelihoods_used, 
            scientific = 20)
        stats$estimated_non_dev_parameters <- format(stats$estimated_non_dev_parameters, 
            scientific = 20)
        print(stats)
        if (covar) {
            if (stats$N_estimated_parameters > 1) {
                print(corstats, quote = FALSE)
            }
            else {
                cat("Too few estimated parameters to report correlations.\n")
            }
        }
    }
    returndat$logfile <- logfile
    inputs <- list()
    inputs$dir <- dir
    inputs$model <- model
    inputs$repfile <- repfile
    inputs$forecast <- forecast
    inputs$warn <- warn
    inputs$covar <- covar
    inputs$verbose <- verbose
    returndat$inputs <- inputs
    if (verbose) 
        cat("completed SS_output\n")
    invisible(returndat)
}
