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
