#' Make an age composition bubble plot
#'
#' @param model A model object as returned from [load.ss.files()]
#' @param subplot 1 for fishery, 2 for survey
#' @param ... Additional parameters passed to [plot_bubbles()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
make_age_comp_bubble_plot <- function(model,
                                      subplot = 1,
                                      ...){

  dat <- model$dat$agecomp %>%
    dplyr::filter(FltSvy == subplot) %>%
    select(Yr, starts_with("a", ignore.case = FALSE)) %>%
    setNames(gsub("a", "", names(.))) %>%
    rename(Year = Yr) %>%
    mutate(n = rowSums(.[-1])) %>%
    mutate_at(vars(-Year), ~(./n)) %>%
    select(-n) %>%
    melt(id.var = "Year") %>%
    as_tibble() %>%
    rename(Age = variable, Proportion = value)

  g <- plot_bubbles(dat, ...)

  g
}

#' Make a bubble plot from the given data
#'
#' @param d a [tibble::tibble()] of the data in long format with column
#' names `Year`, `Age`, and `Proportion`
#' @param clines An optional vector of years to draw cohort lines through
#' @param mean_age A two-column tibble with columnn names `Year` and `Age` where
#' each row containns a year and `Age` represents the mean age for each year
#' @param mean_age_line_color The line color for the mean age line
#' @param mean_age_line_size The line thickness for the mean age line
#' @param mean_age_line_type The line type for the mean age line
#' @param yrs A vector of 2, for the years to show on the plot
#' @param by How many years between year labels on the x-axis
#' @param legend.position See [ggplot2::theme(legend.position)]
#' @param alpha See [ggplot2::geom_point()]
#' @param xlim Limits for the x-axis
#' @param ... Additional parameters passed to [ggplot2::geom_point()],
#' [ggplot2::geom_segment()] and [ggplot2::theme()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_bubbles <- function(d,
                         clines = c(1980, 1984, 1999, 2010, 2014, 2016),
                         mean_age = NULL,
                         mean_age_line_color = "red",
                         mean_age_line_size = 1.5,
                         mean_age_line_type = "solid",
                         yrs = NULL,
                         by = 5,
                         legend.position = "none",
                         alpha = 0.3,
                         xlim = c(1975, year(Sys.Date())),
                         legend.title = "Proportion",
                         ...){

  if(!is.null(xlim[1])){
    d <- d |>
      filter(Year %in% xlim[1]:xlim[2])
  }

  g <- ggplot(d, aes(x = Year, y = Age, size = Proportion)) +
    geom_point(alpha = alpha, ...) +
    scale_x_continuous(breaks = seq(from = xlim[1], to = xlim[2], by = by),
                       expand = c(0.025, 0)) +
    coord_cartesian(xlim) +
    expand_limits(x = xlim[1]:xlim[2]) +
    scale_size_continuous(range = c(0.5, 10))
  if(!is.null(clines)){
    clines <- tibble(year = clines,
                     y = 0,
                     xend = clines + max(as.numeric(d$Age)),
                     yend = max(as.numeric(d$Age)))
    g <- g +
      geom_segment(data = clines,
                   x = clines$year,
                   y = clines$y,
                   aes(xend = clines$xend,
                       yend = clines$yend),
                   size = 1,
                   color = "red",
                   ...)
  }

  if(!is.null(mean_age)){
    g <- g +
      geom_line(data = mean_age,
                aes(x = Year, y = Age),
                inherit.aes = FALSE,
                color = mean_age_line_color,
                size = mean_age_line_size,
                linetype = mean_age_line_type)
  }

  g <- g +
    theme(legend.position = legend.position, ...) +
    guides(size = guide_legend(title = legend.title))

  g
}

#' Make a bubble plot from the given data
#'
#' @param d a [tibble::tibble()] of the data in long format with column names `Year`, `Age`, and `Proportion`
#' @param type 1 = Fishery, any other value = Survey
#' @param clines An optional vector of years to draw cohort lines through
#' @param yrs A vector of 2, for the years to show on the plot
#' @param by How many years between year labels on the x-axis
#' @param legend.position See [ggplot2::theme(legend.position)]
#' @param alpha See [ggplot2::geom_point()]
#' @param xlim Limits for the x-axis
#' @param ... Additional parameters passed to [ggplot2::geom_point()],
#' [ggplot2::geom_segment()] and [ggplot2::theme()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_pearson_bubbles <- function(model,
                                 type = 1,
                                 clines = c(1980, 1984, 1999, 2010, 2014, 2016),
                                 by = 5,
                                 legend.position = "none",
                                 alpha = 0.3,
                                 xlim = c(1975, year(Sys.Date())),
                                 ...){

  if(type == 1){
    d <- model$extra.mcmc$comp_fishery_median
  }else{
    d <- model$extra.mcmc$comp_survey_median
  }

  g <- ggplot(d, aes(x = Yr,
                     y = Age,
                     size = abs(Pearson),
                     fill = factor(sign(as.numeric(Pearson))))) +
    geom_point(pch = 21, alpha = alpha, ...) +
    scale_x_continuous(breaks = seq(from = xlim[1], to = xlim[2], by = by),
                       expand = c(0.025, 0)) +
    coord_cartesian(xlim) +
    expand_limits(x = xlim[1]:xlim[2]) +
    scale_fill_manual(values = c("white", "black"), guide = FALSE) +
    scale_size_continuous(breaks = c(1, 1, 2, 2, 3, 3),
                          labels = c(-8, -4, -0.1, 0.1, 4, 8),
                          range = c(0.1, 8))

  if(!is.null(clines)){
    clines <- tibble(year = clines,
                     y = 0,
                     xend = clines + max(as.numeric(d$Age)),
                     yend = max(as.numeric(d$Age)))
    g <- g +
      geom_segment(data = clines,
                   x = clines$year,
                   y = clines$y,
                   aes(xend = clines$xend,
                       yend = clines$yend),
                   size = 1,
                   color = "red",
                   inherit.aes = FALSE,
                   ...)
  }
  g <- g +
    theme(legend.position = legend.position, ...) +
    guides(size = guide_legend(title = "Residuals",
                               nrow = ifelse(legend.position == "right" |
                                               legend.position == "left", 10, 1),
                               override.aes =
                                 list(fill = c("white", "white", "white",
                                               "black", "black", "black"),
                                      size = c(8, 4, 0.1,
                                               0.1, 4, 8))))

  g
}

#' Get the start and end year of the age comp data, and maximum proportion overall with its year and age
#'
#' @param model A model as returnded by [load.ss.files()]
#' @param type 1 for Fishery and 2 for Survey
#'
#' @return A vector of 5 elements as described above
#' @export
get_age_comp_limits <- function(model, type = 1){

  dat <- model$dat$agecomp %>%
    filter(FltSvy == type) %>%
    select(Yr, starts_with("a", ignore.case = FALSE)) %>%
    setNames(gsub("a", "", names(.))) %>%
    rename(Year = Yr)

  subdat <- dat %>% select(-Year)
  max_row_col <- which(subdat == max(subdat), arr.ind = TRUE)
  max_prop_yr <- dat[max_row_col[1],]$Year
  max_prop_age <- names(subdat)[max_row_col[2]]
  ret_vec <- c(as.integer(min(dat$Year)),
               as.integer(max(dat$Year)),
               max(subdat),
               max_prop_yr,
               max_prop_age)

  names(ret_vec) <- c("start_yr", "end_yr", "max.prop", "max.prop.yr", "max.prop.age")

  ret_vec
}

#' Plot the numbers-at-age bubble plot with mean age line
#'
#' @param model A model object as output from [load_ss_files()]
#' @param scale Number to scale the numbers-at-age by so the legend values are nicer
#' @param ... Additional parameters passed to [plot_bubbles()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
#' @importFrom tibble as_tibble
#' @importFrom dplyr select rename filter mutate mutate_at pull do rowwise rowSums vars
#' @importFrom reshape2 melt
#' @importFrom purrr map2_dfc
make_numbers_at_age_plot <- function(model,
                                     scale = 1e3,
                                     ...){
  natage <- model$extra.mcmc$natage_median %>%
    as_tibble() %>%
    rename(Year = Yr)

  dat <- natage %>%
    melt(id.var = "Year") %>%
    as_tibble() %>%
    rename(Age = variable, Proportion = value) %>%
    mutate(Proportion = Proportion / scale)

  # Mean age algorithm (from [r4ss::SSplotNumbers()])
  # For each year, multiply the numbers-at-age by the age then
  # Add them, so that there is a total sum for each year then
  # divide that by the sum of the numbers-at-age for the year
  ages <- as.numeric(names(natage)[-1])
  years <- natage$Year
  natage <- natage %>% select(-Year)
  sums <- natage %>%
    rowwise() %>%
    do( (.) %>% as.data.frame %>% mutate(sum = sum(.))) %>%
    pull(sum)
  natage <- map2_dfc(natage, ages, `*`)
  natage <- natage %>%
    mutate(sumprod = rowSums(.))
  natage <- cbind(years, natage, sums) %>%
    as_tibble() %>%
    mutate(Age = sumprod / sums) %>%
    rename(Year = years)

  mean_age <- natage %>%
    select(Year, Age) %>%
    mutate(Age = Age + 1) # To offset the 0 in the plot

  g <- plot_bubbles(dat, mean_age = mean_age, clines = NULL, ...)

  g
}

make.age.comp.compare.bubble.plot <- function(model,                  ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                              start_yr = min(d1$Yr, d2$Yr), ## First year for age comps - default from the data frame
                                              end_yr = max(d1$Yr, d2$Yr),   ## Last year for age comps - default from the data frame
                                              show.key = FALSE,       ## Show some sample bubbles at the top with sizes
                                              key.yrs = NULL,         ## Vector of 4 years for locations to put the key if show.key == TRUE
                                              inches = 0.12,
                                              opacity = 80            ## Allows for transparency
                                              ){
  ## Plot the age compositions for fishery and survey overlaid
  oldpar <- par()
  if(show.key){
    if(is.null(key.yrs)){
      stop("make.age.comp.bubble.plot: Error - you must supply a key.yrs vector of 4 years when specifying show.key = TRUE.\n")
    }else{
      if(length(key.yrs) != 4){
        stop("make.age.comp.bubble.plot: Error - key.yrs must be a vector of exactly 4 years when specifying show.key = TRUE.\n")
      }
    }
    par(mar = c(2.1, 4.1, 2.1, 4.1), oma = c(1.1, 1.1, 0, 0), cex.axis = 0.9)
  }else{
    par(mar = c(2.1, 4.1, 1.1, 4.1), oma = c(1.1, 1.1, 0, 0), cex.axis = 0.9)
  }
  d1 <- model$dat$agecomp[model$dat$agecomp$FltSvy == 1,]
  d2 <- model$dat$agecomp[model$dat$agecomp$FltSvy == 2,]
  survey.yrs <- d2$Yr
  if(end_yr < start_yr){
    stop("make.age.comp.bubble.plot: Error - end_yr cannot be less than start_yr\n")
  }
  for(i in 2:1){
    dat <- model$dat$agecomp[model$dat$agecomp$FltSvy == i,]
    if(i == 1){
      dat <- dat[dat$Yr %in% survey.yrs,]
    }
    ages.str <- names(dat)[grep("^a[0-9]+$", names(dat))]
    ages <- as.numeric(gsub("a", "", ages.str))
    min.age <- min(ages)
    max.age <- max(ages)
    ## Get the maximum proportion and its location within the data
    age.df <- dat[,names(dat) %in% ages.str]
    max.prop <- max(age.df)
    which.max.prop <- which(age.df == max(age.df), arr.ind = TRUE)
    ## Convert the locations to year and age for return statement
    which.max.prop <- c(dat$Yr[which.max.prop[1]], ages[which.max.prop[2]])

    x <- data.frame(expand.grid(dat$Yr, min.age:max.age),
                    prop = unlist(dat[,ages.str]))
    names(x) <- c("Yr", "Age", "prop")
    symbols(c(x[,1], -1),
            c(x[,2], -1),
            circles = sqrt(c(x[,3], max.prop)),
            inches = inches,
            ylim = c(min.age, max.age),
            xlim = c(start_yr, end_yr),
            xlab = "",
            ylab = "",
            xaxt = "n",
            add = if(i == 2) FALSE else TRUE,
            fg = if(i == 2) get.shade("darkblue", opacity + 10) else get.shade("darkred", opacity),
            bg = if(i == 2) get.shade("blue", opacity + 10) else get.shade("red", opacity))
    if(i == 2 && show.key){
      symbols(0.2 + c(key.yrs, -1),
              c(16.2, 16.2, 16.2, 16.2, -1),
              circles = sqrt(c(1, 10, 25, 50, max.prop)),
              inches = inches,
              add = TRUE,
              xpd = NA,
              fg = get.shade("darkblue", opacity + 10),
              bg = get.shade("darkred", opacity))
      text(key.yrs + 1.18, c(16.2,16.2,16.2,16.2), c("0.01", "0.1", "0.25", "0.5"), xpd = NA, cex = 0.8)
      ## Fishery dot
      symbols(2009.2,
              16.2,
              circles = 0.05,
              inches = inches,
              add = TRUE,
              xpd = NA,
              fg = "darkred",
              bg = "red")
      text(2009.6 + 1.2, 16.2, "Fishery", xpd = NA, cex = 0.8)
      ## Survey dot
      symbols(2013.2,
              16.2,
              circles = 0.05,
              inches = inches,
              add = TRUE,
              xpd = NA,
              fg = "darkblue",
              bg = "blue")
      text(2013.6 + 1.2, 16.2, "Survey", xpd = NA, cex = 0.8)
    }
  }
  axis(1, at = survey.yrs, labels = survey.yrs, mgp = c(1, 0.5, 0))
  axis(4)
  mtext("Year", side = 1, outer = TRUE)
  mtext("Age", side = 2, line = -1, outer = TRUE, las = 0)
  par <- oldpar
}

#' @param model A model list with MCMC output included. This is typically
#' `base_model` for the hake assessment. Information included in the list
#' will be typical information and output from [r4ss::SSgetMCMC].
#' @param subplot An integer specifying which fishery include in the figure.
#' @param fill A logical specifying if missing years should be
#' included in the figure such that the time series is a complete one.
#' The new default as of 2021 is `TRUE`, which was suggested by Trevor
#' Branch during the 2020 SRG Review.
make.age.comp.fit.plot <- function(model,
                                   subplot = 1,
                                   fill = TRUE){

  oldpar <- par()
  if(subplot == 1){
    ncol <- 4
    f <- 1
    label <- "Fishery age composition"
  }else if(subplot == 2){
    ncol <- ifelse(fill, 4, 1)
    f <- 2
    label <- "Survey age composition"
  }else{
    cat("make.age.comp.fit.plot: Error - subplot must be either 1 or 2.\n\n")
  }
  age.fits(model,
           ncol = ncol,
           f = f,
           uncertainty = TRUE,
           title.text = label,
           legend = FALSE,
           start.color = 1)
  par <- oldpar
}

age.fits <- function(model,
                     case_label = "",
                     f = 1,
                     ncol = 3,
                     start.color = 1,
                     title.text = "Fishery age composition data",
                     legend = TRUE,
                     uncertainty = FALSE,
                     verbose = FALSE) {

  if(f == 1){
    ages <- c(1, 15) #age range
    d <- model$extra.mcmc$comp_fishery
  }else{
    ages <- c(2, 15) #age range
    d <- model$extra.mcmc$comp_survey
  }
  ages.list <- ages[1]:ages[2]

  first.year <- min(d$Yr)
  subtle.color <- "gray40"
  years <- sort(unique(d$Yr))
  obs.data <- NULL
  pred.data <- NULL
  pred.data.025 <- NULL
  pred.data.975 <- NULL
  for(iyr in years){
    j <- d %>% filter(Yr == iyr)
    obs.data <- rbind(obs.data, j %>% pull(Obs_med))
    pred.data <- rbind(pred.data, j %>% pull(Exp_med))
    if(uncertainty){
      pred.data.025 <- rbind(pred.data.025, j %>% pull(Exp_lower))
      pred.data.975 <- rbind(pred.data.975, j %>% pull(Exp_upper))
    }
  }

  colnames(obs.data) <- ages.list
  colnames(pred.data) <- ages.list
  colnames(pred.data.025) <- ages.list
  colnames(pred.data.975) <- ages.list
  # Current year
  cyear <- as.numeric(format(Sys.Date(), "%Y"))
  years1 <- seq(first.year, cyear)
  nyears <- length(years)
  nyears1 <- cyear - first.year

  nages <- length(ages.list)
  mfcol <- c(ceiling(nyears / ncol), ncol)
  mfcol1<- c(ceiling(nyears1 / ncol), ncol)
  par(mfcol = mfcol, oma = c(3.5, 4.5, 3.5, 1), mar = c(0,0,0,0))
  # Use hideous rainbow colors because they loop more gracefully than rich.colors
  # overriding cohort.color setting above to make it constant across fleets
  cohort.color <- rainbow(22)[-c(1:2)]
  if(start.color > length(cohort.color)){
    stop("start.color should be less than ", length(cohort.color),
         call. = FALSE)
  }
  cohort.color <- cohort.color[c(start.color:length(cohort.color), 1:(start.color - 1))]
  cohort.colors <- matrix(ncol=nyears1,nrow=length(cohort.color))
  cohort.colors <- data.frame(cohort.colors)
  ncolors <- length(cohort.color)
  if(verbose){
    cat("ncolors: ", ncolors, "\n")
  }
  for(i in 1:nyears1){
    cohort.color <- c(cohort.color[ncolors], cohort.color[ -1 * ncolors])
    cohort.colors[,i] <- cohort.color
  }

  ylim <- c(0, 1.05 * max(obs.data, pred.data, na.rm = TRUE))
  if(uncertainty){
    ylim <- c(0, 1.05 * max(obs.data, pred.data.975, na.rm = TRUE))
  }
  for(yr in 1:nyears){
    year1 <- years[yr]
    names.arg <- rep("", nages)
    x <- barplot(unlist(obs.data[yr,]),
                 space = 0.2,
                 ylim = ylim,
                 las = 1,
                 names.arg = names.arg,
                 cex.names=0.5,
                 xaxs = "i",
                 yaxs = "i",
                 border = subtle.color,
                 col = cohort.colors[1:nages, year1 - first.year + 1],
                 axes = FALSE,
                 ylab = "",
                 xlab = "")
    if(yr %% mfcol[1] == 0){
      axis(side = 1,
           at = x,
           lab = ages.list,
           line = -0.1,
           col.axis = subtle.color,
           col = subtle.color,
           lwd = 0.5,
           lwd.ticks = 0.5)  #just use for the labels, to allow more control than names.arg
      axis(side = 1,
           at = x[ages.list %% 2 == 0],
           lab = ages.list[ages.list %% 2 == 0],
           line = -0.1,
           col.axis = subtle.color,
           col = subtle.color,
           lwd = 0.5,
           lwd.ticks = 0.5)  #just use for the labels, to allow more control than names.arg
    }
    if(yr <= mfcol[1]){
      axis(2,
           las = 1,
           at = c(0, 0.5),
           col = subtle.color,
           col.axis = subtle.color,
           lwd = 0.5)
    }
    par(new = TRUE)
    plot(x = x,
         y = pred.data[yr, ],
         ylim = ylim,
         xlim = par("usr")[1:2],
         las = 1,
         xaxs = "i",
         yaxs = "i",
         bg = "white",
         fg = "brown",
         type = 'n',
         axes = FALSE,
         ylab = "",
         xlab = "")
    if(legend & par()$mfg[2] == par()$mfg[4] & par()$mfg[1] == 1){
      par(xpd = NA)
      legend(x = 0,
             y = 1.6 * ylim[2],
             legend = c("Observed proportion", "Expected proportion with 95% interval"),
             pch = c(22, 23),
             pt.cex = c(2, 1),
             col = c(subtle.color, 1),
             pt.bg = c(cohort.color[1], "white"), bty = 'n')
      par(xpd = FALSE)
    }
    #segments(x0=x-.3, x1=x+.3, y0=pred.data[yr,], y1=pred.data[yr,], lwd=3)
    #segments(x0=x, x1=x, y0=0, y1=pred.data[yr,], lwd=1)
    ## rect(xleft=x-.2, xright=x+.2, ybottom=0, ytop=pred.data[yr,], lwd=1,
    ##      col=gray(1,alpha=.5))
    if(uncertainty){
      arrows(x0 = x,
             x1 = x,
             y0 = pred.data.025[yr,],
             y1 = pred.data.975[yr,],
             #y0=0, y1=pred.data,
             angle = 90,
             length = 0.02,
             code = 3)
    }
    points(x = x,
           y = pred.data[yr, ],
           pch = 23,
           cex = .8,
           bg = "white",
           lwd = 1)
    box(col = subtle.color,
        lwd = 0.5)
    x.pos <- par("usr")[1] + 0.85*diff(par("usr")[1:2])   #par("usr") spits out the current coordinates of the plot window
    y.pos <- par("usr")[3] + 0.75*diff(par("usr")[3:4])   #par("usr") spits out the current coordinates of the plot window
    text(x = x.pos,
         y = y.pos,
         years[yr],
         cex = 1.2,
         col = subtle.color)
    par(xpd = TRUE)
    ## rect(xleft=x-.2, xright=x+.2, ybottom=0, ytop=pred.data[yr,], lwd=1,
    ##     col=gray(1,alpha=.5))
    abline(h = 0,
           col = subtle.color)
  }
  mtext(side = 1, outer = TRUE, "Age", line = 2)
  mtext(side = 2, outer = TRUE, "Proportion", line = 3.2)
  mtext(side = 3, outer = TRUE, line = 1.2, title.text)
  if(nchar(case_label) > 0){
    mtext(side = 3,
          outer = TRUE,
          line = 0.2,
          paste0("(", case_label, ")"), cex = 0.6)
  }
}
