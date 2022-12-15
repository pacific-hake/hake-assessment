make.survey.history.table <- function(dat,
                                      dat.age1,
                                      digits = 3,
                                      xcaption = "default",
                                      xlabel   = "default",
                                      font.size = 9,
                                      space.size = 10){
  ## Returns an xtable of the history of the acoustic survey
  ## The vessel names need to be fixed. They are seperated by spaces, and may
  ##  or may not have dashes in their names. The dashes will be replaced with
  ##  spaces, and the spaces will be replaced by newlines in the output
  ##
  ## dat is a data frame containing the survey history
  ## dat.age1 is a data frame containing the age-1 values (columns year and
  ##  value, in 1000s of age-1 fish), will be displayed as billions
  ## digits - number of decimal places for biomass and CV values, and age-1 index
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table

  dat$biomass[!is.na(dat$biomass)] <- f(dat$biomass[!is.na(dat$biomass)], digits)
  dat$cv[!is.na(dat$cv)] <- f(dat$cv[!is.na(dat$cv)], digits)

  ## Put the vessels in the right format
  lst <- lapply(dat$vessels,
              function(x){p <- strsplit(as.character(x), " +")[[1]]
                q <- sapply(p, function(y){gsub("-", " ", y)})})
  dat$vessels <- sapply(lst, function(x){latex.mlc(x, FALSE)})

  dat <- dplyr::left_join(dat, dat.age1, by = c("year" = "Year"))
  dat$Index <- dat$Index/1e6     # Convert thousands to billions
  dat$Index[!is.na(dat$Index)] <- f(dat$Index[!is.na(dat$Index)], digits)

  dat[is.na(dat)] <- "--"

  colnames(dat) <- c(latex.bold("Year"),
                     latex.mlc(c("Start",
                                 "date")),
                     latex.mlc(c("End",
                                 "date")),
                     latex.bold("Vessels"),
                     latex.mlc(c("Age-2+ biomass",
                                 "index",
                                 "(million t)")),
                     latex.mlc(c("Sampling",
                                 "CV age-2+")),
                     latex.mlc(c("Number of",
                                 "hauls with",
                                 "age samples")),
                     latex.mlc(c("Age-1 index",
                                 "(billions of",
                                 "fish)")),
                     latex.mlc(c("Sampling",
                                 "CV age-1"))
                    )

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(dat,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(dat),
                                 just = "c")),
        caption.placement = "top",
        include.rownames = FALSE,
        table.placement = "H",
        sanitize.text.function = function(x){x},
        size = size.string)
}

make.survey.by.country.table <- function(dat,
                                         digits = 3,
                                         xcaption = "default",
                                         xlabel   = "default",
                                         font.size = 9,
                                         space.size = 10){
  ## Returns an xtable of the values of the survey by country.
  ##
  ## dat is a data frame containing the survey by country data
  ## digits - number of decimal places for biomass and CV values
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table

  dat$total <- dat$total / 1e3
  dat$canada.total <- dat$canada.total / 1e3
  dat$us.total <- dat$total - dat$canada.total
  dat$us.prop <- 100 - dat$canada.prop
  dat$us.cv <- dat$us.cv / 100
  dat$canada.cv <- dat$canada.cv / 100

  dat$us.total <- f(dat$us.total, digits)
  dat$us.cv <- f(dat$us.cv, digits)
  dat$us.prop <- paste0(f(dat$us.prop, 2), "\\%")
  dat$canada.total <- f(dat$canada.total, digits)
  dat$canada.cv <- f(dat$canada.cv, digits)
  dat$canada.prop <- paste0(f(dat$canada.prop, 2), "\\%")

  dat <- as_tibble(dat) %>%
    select("year",
           "us.total",
           "us.cv",
           "us.prop",
           "canada.total",
           "canada.cv",
           "canada.prop",
           )


  ## Put the vessels in the right format
#  lst <- lapply(dat$vessels,
#              function(x){p <- strsplit(as.character(x), " +")[[1]]
#                q <- sapply(p, function(y){gsub("-", " ", y)})})
#  dat$vessels <- sapply(lst, function(x){latex.mlc(x, FALSE)})

#  dat <- dplyr::left_join(dat, dat.age1, by = c("year" = "Year"))
#  dat$Index <- dat$Index/1e6     # Convert thousands to billions
#  dat$Index[!is.na(dat$Index)] <- f(dat$Index[!is.na(dat$Index)], digits)

#  dat[is.na(dat)] <- "--"

  colnames(dat) <- c(latex.bold("Year"),
                     latex.mlc(c("U.S. Age-2+",
                                 "biomass",
                                 "(million t)")),
                     latex.mlc(c("U.S. sampling",
                                 "CV age-2+")),
                     latex.mlc(c("U.S. percentage",
                                 "of biomass")),
                     latex.mlc(c("Canada Age-2+",
                                 "biomass",
                                 "(million t)")),
                     latex.mlc(c("Canada sampling",
                                 "CV age-2+")),
                     latex.mlc(c("Canada",
                                 "percentage",
                                 "of biomass"))
                    )

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(dat,
               caption = xcaption,
               label = xlabel,
#               align = c("l", "r", "c", "r", "c")),
               align = c(get.align(ncol(dat)-1,
                                   just = "c"),
                         "r")),
        caption.placement = "top",
        include.rownames = FALSE,
        table.placement = "H",
        sanitize.text.function = function(x){x},
        size = size.string)
}

make.survey.extrap.table <- function(dat,
                                     digits = 3,
                                     xcaption = "default",
                                     xlabel   = "default",
                                     font.size = 9,
                                     space.size = 10){
  ## Returns an xtable containing the survey extrapolation values
  ##
  ## dat is a data frame containing the survey extrap. values
  ## digits - number of decimal points for biomass and CV values
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table

  ## Remove 2016 columns
  dat <- dat[,-grep("2016", names(dat))]

  ## Format the columns individually, avoiding any NA's
  dat$no.extrap[!is.na(dat$no.extrap)] <-
    f(dat$no.extrap[!is.na(dat$no.extrap)] / 1000, digits)
  dat$with.extrap[!is.na(dat$with.extrap)] <-
    f(dat$with.extrap[!is.na(dat$with.extrap)] / 1000, digits)
  dat$design.based[!is.na(dat$design.based)] <-
    f(dat$design.based[!is.na(dat$design.based)] / 1000, digits)
  dat$cv.no.extrap[!is.na(dat$cv.no.extrap)] <-
    paste0(f(100 * dat$cv.no.extrap[!is.na(dat$cv.no.extrap)], 1), "\\%")
  dat$cv.with.extrap[!is.na(dat$cv.with.extrap)] <-
    paste0(f(100 * dat$cv.with.extrap[!is.na(dat$cv.with.extrap)], 1), "\\%")
  dat[is.na(dat)] <- "--"

  colnames(dat) <- c(latex.bold("Year"),
                     latex.mlc(c("Biomass with",
                                 "extrapolation",
                                 "(million t)")),
                     latex.mlc(c("Sampling CV",
                                 "with",
                                 "extrapolation")),
                     latex.mlc(c("Biomass no",
                                 "extrapolation",
                                 "(million t)")),
                     latex.mlc(c("Sampling CV",
                                 "no",
                                 "extrapolation")),
                     latex.mlc(c("Biomass",
                                 "Design-based",
                                 "(million t)")))

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(dat,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(dat), just = "c")),
        caption.placement = "top",
        include.rownames = FALSE,
        table.placement = "H",
        sanitize.text.function = function(x){x},
        size = size.string)
}

make.survey.summary.table <- function(dat,
                                      digits = 3,
                                      xcaption = "default",
                                      xlabel   = "default",
                                      font.size = 9,
                                      space.size = 10){
  ## Returns an xtable for the survey summary data from the csv file
  ## Remove 2016 assessment columns which show the difference in the 1998
  ## survey estimate (removed for 2018 assessment, see issue #337 on GitHub)
  ##
  ## dat is a data frame containing the survey summary info
  ## digits - number of decimal points for biomass and CV values
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table

  ## Format the columns individually, avoiding any NA's
  dat$biomass.2016[!is.na(dat$biomass.2016)] <-
    f(dat$biomass.2016[!is.na(dat$biomass.2016)] / 1000, digits)
  dat$biomass.2017[!is.na(dat$biomass.2017)] <-
    f(dat$biomass.2017[!is.na(dat$biomass.2017)] / 1000, digits)

  dat$cv.2016[!is.na(dat$cv.2016)] <-
    paste0(f(dat$cv.2016[!is.na(dat$cv.2016)] * 100, 1), "\\%")
  dat$cv.2017[!is.na(dat$cv.2017)] <-
    paste0(f(dat$cv.2017[!is.na(dat$cv.2017)] * 100, 1), "\\%")

  dat[is.na(dat)] <- "--"

  ## Remove 2016 assessment year columns
  dat <- dat[,-c(2,3)]

  ## colnames(dat) <- c(latex.bold("Year"),
  ##                    latex.mlc(c("Biomass estimate",
  ##                                "2016",
  ##                                "(million t)")),
  ##                    latex.mlc(c("Sampling CV",
  ##                                "2016")),
  ##                    latex.mlc(c("Biomass estimate",
  ##                                "2017",
  ##                                "(million t)")),
  ##                    latex.mlc(c("Sampling CV",
  ##                                "2017")))
  colnames(dat) <- c(latex.bold("Year"),
                     latex.mlc(c("Biomass estimate",
                                 "(million t)")),
                     latex.mlc(c("Sampling CV")))

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(dat,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(dat), just = "c")),
        caption.placement = "top",
        include.rownames = FALSE,
        table.placement = "H",
        sanitize.text.function = function(x){x},
        size = size.string)
}

make.survey.biomass.plot <- function(model,
                                     xlab = "Year",
                                     ylab = "Biomass Index Estimate (million t)"){
  ## It is assumed that the data file has been read in correctly.
  ## Assumes that there is only one 'CPUE' index and it is the acoustic survey.
  ## There is no error checking to warn you if there is more than one index.

  oldpar <- par()
  tmp <- model$dat
  numObs <- tmp$N_cpue
  survey.dat <- tmp$CPUE
  ## Remove non-survey years from the data frame
  survey.dat <- survey.dat[survey.dat$index > 0,]

  ests <- as.list(survey.dat)
  ests$lo <- as.numeric(ests$lo)
  ests$hi <- as.numeric(ests$hi)
  ests$obs <- as.numeric(ests$obs)
  ests$se_log<- as.numeric(ests$se_log)

  ## Hard-coded 2009 for Hake survey squid year
  tmpSE <- ests$se_log[ests$year == 2009]
  ests$se_log[ests$year == 2009] <- 0.0682 ## se without squid inflation
  ests$lo <- exp(log(ests$obs) - 1.96 * ests$se_log)
  ests$hi <- exp(log(ests$obs) + 1.96 * ests$se_log)
  ests$value <- ests$obs

  ests2 <- ests
  ests2$se_log <- NA
  ests2$se_log[ests2$year == 2009] <- tmpSE ## se without squid inflation
  ests2$lo <- exp(log(ests2$obs) - 1.96 * ests2$se_log)
  ests2$hi <- exp(log(ests2$obs) + 1.96 * ests2$se_log)
  ests2$value <- ests2$obs
  par(las=1, mar=c(5, 4, 1, 1) + 0.1, cex.axis = 0.9)
  plotBars.fn(as.numeric(ests2$year),
              ests2,
              scale = 1e6,
              ylim = c(0,3),
              pch = 20,
              xlab = xlab,
              ylab = ylab,
              cex = 1.5,
              las = 1,
              gap = 0.05,
              xaxt = "n",
              ciLwd = 3,
              ciCol = rgb(0, 0, 1, 0.6),
              yaxs = 'i')
  plotBars.fn(as.numeric(ests$year),
              ests,
              scale = 1e6,
              ylim = c(0, 3),
              pch = 20,
              add = TRUE,
              cex = 1.5,
              las = 1,
              gap = 0.05,
              xaxt = "n",
              ciLwd = 3,
              ciCol = gray(0.2))
  axis(1, at = ests$year, cex.axis = 0.8)
  par <- oldpar
}

#' Make a plot of the age-1 index overlaid with model fit to that index
#'
#' @param model A model as output by [load_models()]
#' @param age1index A vector of the age 1 index values
#' @param legendloc See [legend()]
#'
#' @return A plot of the age-1 index fit
#' @export
make.survey.age1.plot <- function(model,
                                  age1index,
                                  legendloc = "topleft"){

  oldpar <- par("mar")
  on.exit(par(oldpar))
  par(mar = c(4, 4, 1, 1) + 0.1)

  x <- age1index
  yrs <- x$Year

  recr1 <- model$extra.mcmc$natage_median %>%
    filter(Yr %in% yrs) %>%
    pull(`1`)
  recrAll <- model$extra.mcmc$natage_median %>%
    filter(Yr %in% min(yrs):max(yrs)) %>%
    pull(`1`)

  logAge1 <- log(recr1)
  logIndex <- log(x$Index)
  mn <- mean(logAge1)
  index <- mn * logIndex / mean(logIndex[!is.na(x$Index)])
  plot(min(yrs):max(yrs),
       recrAll / 1e3,
       pch = 4,
       type = "b",
       log = "y",
       ylim = c(0.015, 50),
#       ylim = range(c(recr1, exp(index)),
#                    na.rm = TRUE) * c(0.7, 3) / 1e3,
       lwd = 2,
       xaxt = "n",
       xlab = "Year",
       ylab = "Estimated age-1 fish (billions)",
       las = 1,
       col = gray(0.7),
       cex = 0.8)
  points(yrs,
         exp(index) / 1e3,
         pch = 16,
         col = "blue",
         cex = 1.5)
  points(x$Year[!is.na(x$Index)],
         recr1 / 1e3,
         pch = 4,
         col = "black",
         cex = 1,
         lwd = 2)
  axis(1, at = x$Year)
  legend(legendloc,
         c("Model-estimated age-1 fish",
           "Scaled acoustic survey age-1 index"),
         col = c("black", "blue"),
         pch = c(4,16),
         lty = NA,
         lwd = 2,
         bty = "o")
}

make.survey.biomass.extrap.plot <- function(dat){
  ## dat - data.frame of different indices
  ## show - vector of which values to show
  oldpar <- par("mar", "las", "cex.axis")
  on.exit(par(oldpar))

  ## Remove non-data years from the data frame no longer needed because we're
  ##  comparing cases with different range of years
  ## dat <- dat[complete.cases(dat),]

  # values with extrapolation used in base model
  ests <- data.frame(year = dat$year,
                     obs = dat$with.extrap,
                     se_log = dat$cv.with.extrap)
  ests$lo <- exp(log(ests$obs) - 1.96 * ests$se_log)
  ests$hi <- exp(log(ests$obs) + 1.96 * ests$se_log)
  ests$value <- ests$obs

  # 2009 w/o squid inflation
  ests_squid <- ests[ests$year == 2009, ]
  ests_squid[, "se_log"] <- 0.0682 ## se without squid inflation
  ests_squid[, c("lo", "hi")] <- exp(log(ests_squid$obs) + c(-1.96, 1.96) * ests_squid$se_log)
  par(las = 1, mar = c(5, 4, 1, 1) + 0.1, cex.axis = 0.9)

  plotBars.fn(ests$year,
              ests,
              scale = 1e3,
              ylim = c(0, 3),
              yaxs = 'i',
              pch = 20,
              xlab="Year",
              ylab = "Biomass index (million t)",
              cex = 1.5,
              las = 1,
              gap = 0.05,
              xaxt = "n",
              ciLwd = 3,
              ciCol = rgb(0, 0, 0, 0.5))
  plotBars.fn(ests_squid$year,
              ests_squid,
              scale = 1e3,
              pch = 20,
              add = TRUE,
              cex = 1.5,
              las = 1,
              gap = 0.05,
              ciLwd = 3.25,
              ciCol = rgb(0, 0, 0, 1))
  axis(1, at = ests$year, cex.axis = 0.8)
}

# Plot the age-1 index data only (see make.survey.age1.plot for results)
make.survey.age1.plot.data <- function(dat,
                                       log.scale = TRUE,
                                       yLim = c(10, 26)){
  ## dat - data.frame of age 1 index (saved as age.1.index)
  ## log - whether to show log scale or not
  ## yLim - ylim value, default is for log.scale = TRUE in 2022
  oldpar <- par("mar", "las", "cex.axis")
  on.exit(par(oldpar))

  par(las = 1, mar = c(5, 4, 1, 1) + 0.1, cex.axis = 0.9)
  # values with extrapolation used in base model
  ests <- data.frame(year = dat$Year,
                     obs = dat$Index,
                     se_log = dat$CV)

  # convert to 1000s of fish for log plot, billions for linear plot:
  if(log.scale){
    ests$obs <- ests$obs*1e3} else {
    ests$obs <- ests$obs/1e6}

  ests$lo <- exp(log(ests$obs) - 1.96 * ests$se_log)
  ests$hi <- exp(log(ests$obs) + 1.96 * ests$se_log)
  ests$value <- ests$obs

  if(log.scale){
    plotBars.fn(ests$year,
                log(ests),
                scale = 1,
                ylim = yLim,
                yaxs = 'i',
                pch = 20,
                xlab="Year",
                ylab = "Relative age-1 index estimate (log(fish))",
                cex = 1.5,
                las = 1,
                gap = 0.05,
                xaxt = "n",
                ciLwd = 3,
                ciCol = rgb(0, 0, 0, 0.5))
  }

  if(!log.scale){
    plotBars.fn(ests$year,
                ests,
                scale = 1,
                ylim = yLim,
                yaxs = 'i',
                pch = 20,
                xlab="Year",
                ylab = "Relative age-1 index (billions of fish)",
                cex = 1.5,
                las = 1,
                gap = 0.05,
                xaxt = "n",
                ciLwd = 3,
                ciCol = rgb(0, 0, 0, 0.5))
  }
  axis(1, at = ests$year, cex.axis = 0.8)
}

make.kriging.parameters.table <- function(krig.pars = kriging.pars,
                                          xcaption = "default",
                                          xlabel   = "default",
                                          font.size = 9,
                                          space.size = 10,
                                          placement = "H"){
  ## Returns an xtable in the proper format for the kriging parameters
  ##
  ## krig.pars - The kriging paramaters from the csv file as a data frame
  ## digits - number of decimal points for biomass and CV values
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for where to place the table in the document

  krig.pars[1:10,"SearchRadius"] <-
    f(as.numeric(krig.pars[1:10,"SearchRadius"]), 2)
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- 0
  addtorow$pos[[3]] <- 10
  addtorow$pos[[4]] <- nrow(krig.pars)
  addtorow$command <- c(paste0("\\toprule ",
                               "Year",
                               latex.amp(),
                               "Search radius",
                               latex.amp(),
                               "$k$\\subscr{min}",
                               latex.amp(),
                               "$k$\\subscr{max}",
                               latex.nline,
                               "\\midrule"),
                        paste0(latex.bold(last.assess_yr), latex.nline),
                        paste0(latex.bold(assess_yr), latex.nline),
                        "\\bottomrule")

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(krig.pars,
               caption = xcaption,
               label = xlabel,
               align = c("l", "r", "c", "r", "c")),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = placement,
        hline.after = NULL,
        booktabs = TRUE)
}
