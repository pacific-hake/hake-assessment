#' Weight-At-Age Heatmap Including Extrapolated Years
#'
#' Produce a matrix of colors and values of weight-at-age information
#' by year (y-axis) and age (x-axis). The plot uses output from a
#' Stock Synthesis model.
#'
#' @details
#' Ages zero through the maximum age modeled in the data are shown
#' for each year plotted. This maximum-age shown is found by removing ages
#' that are the duplicate of the previous age, no user input is needed.
#'
#' Users have many options to control what years are included in the plot.
#' The beginning years of data can be removed using \code{first.year}.
#' Users do not have control over what are noted as projection years, which
#' are all years after the end of the data included in the assessment model
#' up to last year included in the weight-at-age matrix.
#' These projection years are noted using a horizontal line but if the
#' demarcation is not wanted, then they can change the line width to zero.
#' Finally, users can calculate the colors using a range of data specified
#' with \code{first.year} and the last year of the data in the model. Then,
#' the resulting plot can be truncated to a specified year range using
#' \code{print.years}. The truncation is helpful to facilitate plots that fit
#' on a single page or on a slide with readable values. Using this truncation,
#' the colours would show trends across all plots made rather than just the
#' truncated data.
#'
#' @param model An list of results read in from an SS model using
#' \code{\link{load_ss_files}}.
#' @param fleet An integer value specifying which fleet you want plotted.
#' Fleet -2 will plot fecundity information.
#' Fleet -1 will plot population weight-at-age for the middle of the year.
#' Fleet 0 will plot population weight-at-age for the beginning of the year.
#' Positive values for fleet will link to a modeled fleet.
#' @param proj.line.color Line color to separate projection years.
#' @param proj.line.width Line width to separate projection years.
#' @param proj.line.yr Year to separate projection years from the data period,
#' where if \code{NULL}, then the last year of the data in the assessment will
#' be used to specify when the data end and projections begin.
#' @param first.year The first year to plot because users may want to truncate
#' the data to only years for which were informed by data and not all years
#' in the weight-at-age file.
#' @param extrap.mask A list of vectors of which years are extrapolations so
#' that they can be colored accordingly. 1's means they are extrapolations
#' and 0's means they are not.
#' @param longterm.mean.ages A vector of mean weight-at-age values
#' per ages zero to the maximum age of fish in the data.
#' If \code{NULL}, the first year will be assumed to be the mean  of all years.
#' @param font.size Font size of the values printed in each box.
#' @param axis.font.size Font size for axis labels.
#' @param samplesize A logical value specifying if the heat map should be of
#' input sample size used to generate the data rather than the weights-at-age.
#' @param print.years A vector of years that will be included in the output,
#' but users should note that all data from \code{first.year} to the terminal
#' year of the data in the model will be used to calculate means and colors.
#' This parameter facilitates splitting the plot into two separate plots when
#' there are a number of years.
#' @param colour A character string of \code{"both"}, \code{"all"},
#' or \code{"age"}. If the default of \code{"both"}, then the colors of the
#' boxes will be based on all of the data and the transparency of the colors
#' will strictly be a function of each individual age in turn. That is, the
#' darkness of the colors used across years for age zero specifies which years
#' have the largest age-0 fish. \code{"all"} specifies colors based on the min
#' and max values of all ages without transparency. \code{"age"} colors the min
#' and max color specific for each age without transparency.
#' @param start_yr Start year
#' @param end_yr End year
#'
#' @return A [ggplot2::ggplot()] object
#' @export
weight.at.age.heatmap <- function(model,
                                  fleet = 1,
                                  proj.line.color = "royalblue",
                                  proj.line.width = 1,
                                  proj.line.yr = NULL,
                                  first.year = 1975,
                                  start_yr = 1966,
                                  end_yr = assess_yr - 1,
                                  extrap.mask = NULL,
                                  longterm.mean.ages = NULL,
                                  font.size = 4,
                                  axis.font.size = 10,
                                  samplesize = FALSE,
                                  print.years = NULL,
                                  colour = c("all", "age", "both")){

  colour <- match.arg(colour)

  stopifnot(!is.null(extrap.mask))

  ## Toggle data frame for which values are extrapolated values
  last_data_yr <- model$endyr
  input.yrs <-  first.year:last_data_yr
  if (is.null(proj.line.yr)) proj.line.yr <- last_data_yr

  wa <- as_tibble(model$wtatage[, !grepl("comment", colnames(model$wtatage))]) %>%
    filter(Fleet == fleet) %>%
    select(-c(Seas, Sex, Bio_Pattern, BirthSeas, Fleet)) %>%
    filter(Yr > 0)
  wa <- wa[,1:which(apply(wa, 1, duplicated)[, 1])[1]-1]

  min_yr <- min(wa$Yr)
  if(min_yr > start_yr){
    # Copy first row, modify it, and bind it on at the start
    #row <- wa[1, ]
    # Change to mean of all years
    mean_yrs <- first.year:end_yr
    mean_dat <- wa %>%
      filter(Yr %in% mean_yrs) %>%
      select(-Yr) %>%
      summarize_all(~{mean(.x)}) %>%
      unlist()
    # Make new single row for earliest year, which contains means of all years in the data
    row <- c(start_yr, mean_dat) %>% t() %>% as.data.frame() %>% as_tibble()
    names(row)[1] <- "Yr"
    rows <- row
    for(yr in (start_yr + 1):(min_yr - 1)){
      # Bind a new row for all years between the earliest year and the year before the start year of data
      row[1, 1] <- yr
      rows <- bind_rows(rows, row)
    }
    wa <- rows %>% bind_rows(wa)
    wa <- wa[order(wa$Yr), ]
  }

  last.yr <- max(wa$Yr)

  w <- melt(wa, id.vars = "Yr")

  ages <- as.numeric(levels(unique(w$variable)))
  nage <- length(ages)
  if (!is.null(longterm.mean.ages)) {
  	stopifnot(length(longterm.mean.ages) == nage)
  }

  colors <- colorRampPalette(c("red",
                               "yellow",
                               "green",
                               "dodgerblue"))(nage - 1)
  if (is.null(longterm.mean.ages)) {
    longterm.mean.ages <- unlist(wa[wa$Yr == min(wa$Yr), -1])
  }
  avg <- data.frame(Yr = min(wa$Yr) - 2,
                    variable = ages,
                    value = longterm.mean.ages)
  w <- as.data.frame(rbind(w, avg))
  w$Yr <- as.integer(w$Yr)
  w$value <- as.numeric(w$value)
  w$age <- utils::type.convert(w$variable, as.is = TRUE)

  nn <- pivot_longer(extrap.mask, cols = starts_with("a"),
    values_to = "a", names_to = "age", names_prefix = "a") %>%
    arrange(as.numeric(age, fleet, Yr)) %>%
    mutate(Yr = if_else(Yr < 0, min(w$Yr), Yr))

  valswithmask <- merge(w, nn, by = c("Yr", "age"), all = TRUE)
  valswithmask[is.na(valswithmask$a), "a"] <- 0
  valswithmask <- valswithmask %>%
    group_by(Yr) %>% mutate(N = sum(a)) %>% ungroup()
  bycolumn <- valswithmask %>%
    group_by(age) %>%
    mutate(rescale = scales::rescale(value)) %>%
    ggplot(., aes(x = factor(age), y = Yr,
      fontface = ifelse(a > 0, "plain", "bold"))) +
    scale_fill_gradientn(colors = colors, guide = FALSE) +
    scale_alpha(range = c(0.1, 1)) +
    theme(legend.position = "none")
    if (colour == "age") {
      g <- bycolumn + geom_tile(aes(fill = rescale))
    }
    if (colour == "both") {
      g <- bycolumn +
        geom_tile(aes(alpha = rescale, fill = value))
    }

  if (colour == "all") {
  g <- ggplot(valswithmask,
    aes(y = Yr, fontface = ifelse(a > 0, "plain", "bold")))+
    geom_tile(aes(x = variable, fill = value)) +
    scale_fill_gradientn(colors = colors, guide = FALSE)
  }
  g <- g +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(size = axis.font.size),
          axis.text.y = element_text(size = axis.font.size)) +
    scale_y_continuous(breaks = seq(min(w$Yr), max(w$Yr), 1),
                       labels = c(ifelse(samplesize, "sum", "mean"),
                                  "",
                                  seq(min(w$Yr),
                                      max(w$Yr),
                                      1)[-c(1,2)])) +
    ylab("Year") +
    xlab("Age") +
    geom_hline(yintercept = input.yrs[1] - 0.5,
               color = proj.line.color,
               size = proj.line.width) +
    coord_cartesian(expand = FALSE, ylim = print.years)

  if(samplesize) {
    g <- g +
      geom_text(
        data = g$data %>% dplyr::filter(Yr != min(w$Yr)),
        aes(x = as.numeric(factor(age)) + 0.3, label = a),
        hjust = "right",
        size = font.size) +
      geom_text(
        data = g$data %>% dplyr::filter(Yr == min(w$Yr)),
        aes(x = as.numeric(factor(age)) + 0.3, y = Yr + .5, label = a),
        size = font.size, angle = 20) +
      coord_cartesian(expand = FALSE, ylim = print.years,
        xlim = c(0.5, model$nage + 2.5)) +
      geom_text(data = subset(valswithmask, age == 15 & Yr > min(w$Yr)),
        hjust = "right",
                x = model$nage + 2.5, aes(label = N, col = rescale(N))) +
      geom_text(
        data = g$data %>% dplyr::filter(Yr == min(w$Yr), age == model$nage),
        label = "sum",
        x = model$nage + 2,
        y = min(w$Yr)) +
    scale_colour_gradientn(colors = colors, guide = FALSE)
    # Code below adds squiggle line of mean weight-at-age down each column
    # geom_path(
    #   data = valswithmask %>% dplyr::filter(Yr > 1965, Yr <= 2020) %>%
    #     arrange(Yr) %>%
    #     group_by(age) %>%
    #     mutate(scaling = rescale(value,to = c(-0.25,0.25))) %>%
    #     ungroup(),
    #   aes(x = age+1 + scaling, y = Yr, group = age))
  } else {
    g <- g +
      geom_text(aes(x = variable, label = sprintf('%0.2f', value)), size = font.size)
  }

  if(last.yr > last_data_yr){
    ## Add line separating projections
    g <- g + geom_hline(yintercept = proj.line.yr + 0.5,
                        color = proj.line.color,
                        size = proj.line.width)
  }
  g
}

barfun <- function(x, y, x.pos="left", plot=1, ...){
  #make barplot-like shape which is really a polygon
  if(any(sort(x)!=x)){
    stop("x must be a vector of strictly increasing values")
  }
  if(length(x)!=length(y) | any(!is.numeric(x), !is.numeric(y))){
    stop("x and y must be numeric vectors of the same length")
  }
  n <- length(x)

  if(x.pos=="left"){
    # x-values represent left-hand sides of each bin
    # make final value as the last in the sequence plus diff from previous
    x.vec <- c(x, tail(x,1) + diff(tail(x,2)))
  }
  if(x.pos=="right"){
    # x-values represent right-hand sides of each bin
    # make final value as the last in the sequence plus diff from previous
    x.vec <- c(x[1], x[1] - diff(x[1:2]))
  }
  if(x.pos=="center"){
    # x-values represent right-hand sides of each bin
    # make final value as the last in the sequence plus diff from previous
    diff <- diff(head(x,2))
    x.vec <- c(x[1] - diff/2, x + diff/2)
  }
  x.double <- sort(c(x.vec, x.vec))
  y.double <- c(0, y[sort(rep(1:n, 2))], 0)
  if(plot){
    polygon(x.double, y.double, ...)
  }
  return(data.frame(x=x.double, y=y.double))
}

# source atsea.ages as noted in /hake-data/Rcode/AtSeaComps.R
make.size.at.age.plot <- function(df, type='len'){
  # df is a data.frame with columns for month, length, and weight.

  df.early <- df[df$month<8,] # up through July
  df.late <- df[df$month>=8,] # August and after
  #par(mfrow=c(8,1), mar=rep(0,4), oma=c(4,4,1,1))
  par(mar=c(4.1,4.1,1,1))

  if(type=="len"){
    ylim <- c(20, 56)
    breaks <- seq(0,85,1)
    y.axis.ticks <- seq(0, 85, 2)
    y.text <- 25
    scale <- 2.5
    ylab <- "Length (cm)"
  }
  if(type=="wt"){
    ylim <- c(0, 1.2)
    breaks <- seq(0, 7.0, 0.05)
    y.axis.ticks <- seq(0, 2.0, 0.1)
    y.text <- 0.1
    scale <- 0.08
    ylab <- "Weight (kg)"
  }

  plot(0, type='n', xlim=c(2,12), ylim=ylim,
       xaxs='i', yaxs='i', xlab="Age", ylab=ylab, axes=FALSE)
  axis(1, at=1:(round(par()$usr[2])-1))
  axis(2, las=1, at=y.axis.ticks)
  #for(period in c("all","early","late")){
  fillvec <- c(rgb(.6, .6, .6, 1),
               rgb(1, 0, 0, .1),
               rgb(0, 0, 1, .1))
  bordervec <- c(rgb(.6, .6, .6, 1),
                 rgb(.7, 0, 0, .8),
                 rgb(0, 0, .7, .8))

  periods <- 2:3
  for(period in periods){
    if(period==1) df.tmp <- df       # all months
    if(period==2) df.tmp <- df.early # up to month 7
    if(period==3) df.tmp <- df.late  # month 8 and onward

    if(type=='len'){
      variable <- df.tmp$length
    }
    if(type=='wt'){
      variable <- df.tmp$weight
    }

    # loop over ages
    for(age in 2:20){
      hist.info <- hist(variable[df.tmp$age==age],
                        breaks=breaks, plot=FALSE)
      # use barfun function defined at the top of this file
      polygon.outline <- barfun(x=hist.info$breaks[-length(hist.info$breaks)],
                                y=hist.info$density)
      polygon(x=age + scale*polygon.outline$y, y=polygon.outline$x,
              col=fillvec[period], border=bordervec[period])
      n <- sum(df.tmp$age==age, na.rm=TRUE)
      text(x=age, y=y.text - .025*diff(ylim)*period,
           labels=paste0("n=",n), col=bordervec[period], cex=.8, pos=4)
    }
  }
  if(type=='len'){
    abline(h=y.axis.ticks, lty=3, col='grey40')
    abline(h=seq(0, 80, 10), lty=1, col='grey40', lwd=1)
  }
  if(type=='wt'){
    abline(h=y.axis.ticks, lty=3, col='grey40')
    abline(h=seq(0, 1.2, .2), lty=1, col='grey40', lwd=1)
  }
  abline(v=0:10)
  legend('topleft', fill=fillvec[periods], border=bordervec[periods],
         bg='white',
         legend=c("May-November", "May-July", "August-November")[periods])
}

if(FALSE){
  # load atsea.ages containing fish sample information from U.S. at-sea fishery
  # (contains other confidential information not saved here)
  df <- atsea.ages
  df$month <- as.numeric(substring(df$HAUL_OFFLOAD_DATE, 6, 7))
  df$year <- as.numeric(substring(df$HAUL_OFFLOAD_DATE, 1, 4))
  # subset for 2016 only and then look at Spring and Fall values
  df <- df[df$year==2016,]
  df$length=df$LENGTH
  df$weight=df$WEIGHT

  setwd('C:/github/hake-assessment/doc/r')
  dir.SRG.management <- "../../beamer/SRG/Management/"
  cairo_ps(filename = file.path(dir.SRG.management,
                       "Figures/length-at-age-U.S.-atsea-2016.eps"),
                   width = 6, height = 5, pointsize = 10)
  make.size.at.age.plot(df, type='len')
  dev.off()
  cairo_ps(filename = file.path(dir.SRG.management,
                       "Figures/weight-at-age-U.S.-atsea-2016.eps"),
                   width = 6, height = 5, pointsize = 10)
  make.size.at.age.plot(df, type='wt')
  dev.off()

  # load bds.fish.worked containing fish sample information from U.S. shore-based fishery
  # (contains other confidential information not saved here)
  df2 <- bds.fish.worked
  df2 <- df2[df2$SAMPLE_YEAR==2016,]
  df2$month <- df2$SAMPLE_MONTH
  df2$length <- df2$FISH_LENGTH/10
  df2$weight <- df2$FISH_WEIGHT
  df2$age <- df2$AGE
  make.size.at.age.plot(df2, type='len')
  make.size.at.age.plot(df2, type='wt')
}
