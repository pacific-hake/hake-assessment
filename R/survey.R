#' An xtable of the history of the acoustic survey
#'
#' @details
#' The vessel names need to be fixed. They are seperated by spaces, and may
#' or may not have dashes in their names. The dashes will be replaced with
#' spaces, and the spaces will be replaced by newlines in the output
#'
#' @param dat A data frame containing the survey history.
#' @param CPUE A data frame containing the obs and se_log by year and index.
#'  Observations are in 1000s but will be displayed as billions.
#' @param digits Number of decimal places.
#' @param xcaption Caption to appear in the calling document.
#' @param xlabel The label used to reference the table in latex.
#' @param font.size Size of the font for the table.
#' @param space.size Size of the vertical spaces for the table.
make.survey.history.table <- function(dat,
                                      CPUE,
                                      digits = 3,
                                      xcaption = "default",
                                      xlabel   = "default",
                                      font.size = 9,
                                      space.size = 10){
  dat <- dplyr::full_join(
    x = dat %>%
      # Put vessels in the right format
      dplyr::mutate(
        vessels = purrr::map(
          .x = strsplit(survey_history_df$vessels," +"),
          .f = ~ latex.mlc(
            gsub(pattern = "-", replacement = " ", x = .),
            FALSE
          )
        )
      ),
    # Massage the CPUE data from the data file into wide format
    y = CPUE %>%
      dplyr::filter(index > 0) %>%
      dplyr::mutate(
        obs = f(obs / 1e6, digits),
        se_log = f(se_log, digits)
      ) %>%
      tidyr::pivot_wider(
        names_from = index,
        values_from = c("obs", "se_log"),
        values_fill = "--"
      ) %>%
      dplyr::select(-seas),
    by = "year"
  ) %>%
  dplyr::relocate(se_log_2, hauls.with.samples, .after = obs_2)

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

#' Make a plot of the age-1 index overlaid with model fit to that index
#'
#' @param model A model as output by [load_models()].
#' @param legendloc See [legend()].
#' @param ylim The limits of the y axis as a range.
#'
#' @return A plot of the age-1 index fit
#' @export
make.survey.age1.plot <- function(model,
                                  legendloc = "topleft",
                                  ylim = c(0.015, 50)){

  oldpar <- par("mar")
  on.exit(par(mar = oldpar))
  par(mar = c(4, 4, 1, 1) + 0.1)

  x <- dplyr::filter(
    model[["dat"]][["CPUE"]],
    index == 3
  )
  yrs <- x$year

  recr1 <- model$extra.mcmc$natage_median %>%
    filter(Yr %in% yrs) %>%
    pull(`1`)
  recrAll <- model$extra.mcmc$natage_median %>%
    filter(Yr %in% min(yrs):max(yrs)) %>%
    pull(`1`)

  logAge1 <- log(recr1)
  logIndex <- log(x$obs)
  mn <- mean(logAge1)
  index <- mn * logIndex / mean(logIndex[!is.na(x$obs)])
  plot(min(yrs):max(yrs),
       recrAll / 1e3,
       pch = 4,
       type = "b",
       log = "y",
       ylim = ylim,
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
  points(x$year[!is.na(x$obs)],
         recr1 / 1e3,
         pch = 4,
         col = "black",
         cex = 1,
         lwd = 2)
  axis(1, at = x$year)
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

  # values with extrapolation used in base model
  ests <- data.frame(year = dat$year,
                     obs = dat$obs / 1000,
                     se_log = dat$se_log)
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
  ## dat - data.frame of age 1 index (from base_model$dat$CPUE)
  ## log - whether to show log scale or not
  ## yLim - ylim value, default is for log.scale = TRUE in 2022
  oldpar <- par("mar", "las", "cex.axis")
  on.exit(par(oldpar))

  par(las = 1, mar = c(5, 4, 1, 1) + 0.1, cex.axis = 0.9)
  # values with extrapolation used in base model
  ests <- dat[, c("year", "obs", "se_log")]

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
