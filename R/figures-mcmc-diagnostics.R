#' Make a grid of posterior plots with priors and initial values
#'
#' @param model The model output from Stock Synthesis as loaded by
#'   [create_rds_file()].
#' @param posterior_regex A vector of regular expressions that can be matched
#'   to parameter names. Order the vector in the same order as you want the
#'   figures to be plotted but make sure you also have `titles` ordered
#'   appropriately as well otherwise the names will not line up.
#' @param titles A vector of titles for the plots. The titles can be used as is
#'   or they can be transformed via a labeller passed to
#'   [ggplot2::facet_wrap()] via `...`. The default value of `NULL` leads to
#'   each panel being labelled with a lower-case letter enclosed in open
#'   brackets, e.g., `(a)`. If you truly do not want a label, just pass the
#'   argument a vector of empty strings, e.g., `c("", "", ...)`, with the same
#'   length as the result of parameters that are found via `posterior_regex`.
#' @param x_range A string specifying the method to limit the range of each
#'   x axis in the panels. Typically, all geoms used in the figure dictate the
#'   range. But, with the use of [ggh4x::facetted_pos_scales()] we can specify
#'   which data set should specify the range limits. Setting this argument to
#'   `"posterior"`, instead of the default `"prior"`, will limit the ranges to
#'   only the realized values in the posterior. This can be helpful when the
#'   prior is quite vague and the posterior only covers a small range of the
#'   parameter space.
#' @param ... Parameters to be passed to [ggplot2::facet_wrap()]. For example,
#'   `labeller = label_parsed_space`, which is available in this package, will
#'   remove the spaces from the strings and implement
#'   [ggplot2::label_parsed()]. And, `ncol` and `nrow` can be used to direct
#'   the output or you can let {ggplot2} figure it out.
#'
#' @return A {ggplot2} object.
#' @export
plot_priors_vs_posts <- function(model,
                                 posterior_regex,
                                 titles = NULL,
                                 x_range = c("posterior", "prior"),
                                 ...){
  x_range <- match.arg(x_range)
  titles <- if(is.null(titles)){
    glue("({letters[seq_along(posts)]})")
  }else{
    titles
  }

  priors <- get_prior_data(model, posterior_regex)
  posts <- get_posterior_data(model, posterior_regex)
  names(posts) <- titles
  names(priors) <- titles

  # Make long data frames for ggplot2
  posteriors_long <- enframe(posts) |>
    unnest(cols = "value") |>
    mutate(parameter = factor(name, levels = titles))
  posteriors_ranges <- map(
    posts, ~ scale_x_continuous(limits = range(.x)))
  priors_long <- map(priors, "prior_random") |>
    enframe() |>
    unnest(cols = "value") |>
    mutate(parameter = factor(name, levels = titles)) |>
    filter(!is.na(value))
  priors_init <- map(priors, "initval") |>
    unlist() |>
    enframe() |>
    mutate(parameter = factor(name, levels = titles))
  gg <- ggplot() +
    geom_histogram(data = posteriors_long,
                   mapping = aes(value, after_stat(density)),
                   fill = "gray60",
                   bins = 30) +
    geom_density(data = priors_long,
                 mapping = aes(value, after_stat(density)),
                 col = "black",
                 linewidth = 1.2) +
    geom_point(data = priors_init,
               mapping = aes(x = value, y = 0),
               col = "red",
               pch = 17) +
    geom_vline(data = group_by(posteriors_long, parameter) |>
                 summarize(median = median(value)),
               mapping = aes(xintercept = median),
               linetype = 2,
               col = rgb(0, 0, 0, 0.5)) +
    facet_wrap("parameter", scales = "free", ...) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    xlab("") +
    ylab("")

  # xlim for each panel will be based on both the posterior and the prior
  # unless `ggh4x` is called b/c `ggplot2` doesn't allow for manipulation
  # of the axes by panel only across all panels with scales
  if (x_range == "posterior") {
    gg <- gg +
      ggh4x::facetted_pos_scales(x = posteriors_ranges)
  }

  return(gg)
}

#' Generate the correlation values as text for the panels
#'
#' @param x The x values for the correlations
#' @param y The y values for the correlations
#' @param digits The number of decimal places to round to in the panels
#' @param prefix A vector of text to paste to the beginning of the
#' correlation values
#' @param cex.cor the font size for the correlation text
panel.cor <- function(x,
                      y,
                      digits = 2,
                      prefix = "",
                      cex.cor){

  usr <- par("usr")
  on.exit(par(usr))

  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8 / strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * sqrt(r))
}

#' Plot pairs for MCMC posteriors
#'
#' @param model A model object as output by [load_ss_files()]
#' @param inc.key.params If TRUE, use the arguments `key_posteriors_regex`
#' and `key_posteriors_names`
#' @param key_posteriors_regex  A vector of regular expressions representing
#' key posteriors
#' @param key_posteriors_names  A vector of names to show for the key
#' posteriors
#' @param recr A vector of recruitment deviation years to include if
#' `inc.key.params` is `FALSE`
#' @param bratio A vector of Bratio years to include if `inc.key.params` is
#' `FALSE`
#' @param forecatch A vector of forecast catch years to include if
#' `inc.key.params` is `FALSE`
#'
#' @return A base R plot
#' @export
make.mcmc.diag.pairs.plot <- function(model,
                                      inc.key.params = TRUE,
                                      key_posteriors_regex = NULL,
                                      key_posteriors_names = NULL,
                                      recr = NULL,
                                      bratio = NULL,
                                      forecatch = NULL){

  m <- model$mcmc

  if(inc.key.params){
    lst <- lapply(seq_along(key_posteriors_regex), function(x){
      select(m, matches(key_posteriors_regex[x]))
    })
    par_estimated <- vector()    # check which key_posteriors are estimated
    par_estimated_number <- vector()    # check which key_posteriors end up with
                                        # two columns due to regex
    for(i in 1:length(key_posteriors_regex)){
      par_estimated[i] <- ncol(lst[[i]]) > 0
      par_estimated_number[i] <- ncol(lst[[i]])
    }

    # Manually fix the two columns issue when including Age1 Survey
    if(max(par_estimated_number) > 1){
      dupl <- which.max(par_estimated_number)   # which list containts duplicates
      if(sum(par_estimated_number == dupl)){
        stop("Multiple repeated column names, need to manually decide which to include.")
      }

      if("Q_extraSD_Age1_Survey(3)" %in%
         names(lst[[dupl]])){
        key_posteriors_names <- c(key_posteriors_names[1:dupl],
                                  "Age-1 survey extra SD",
                                  key_posteriors_names[(dupl+1):length(key_posteriors_names)])
      } else {
        stop(paste0("Need to decide which variables to show in
             make.mcmc.diag.pairs.plot() for model run ",
             model$path))
      }
    }

    obj_func_col <- select(m, contains("Objective_function"))
    df <- bind_cols(obj_func_col, lst)
    labels <- c("Objective\nFunction", key_posteriors_names[par_estimated])
    if(length(names(df)) != length(labels)){
      stop("Number of parameters estimated from key posteriors is wrong.")
    }
  }else{
    if(is.null(recr)){
      stop("If inc.key.params is FALSE, recr must not be null.",
           call. = FALSE)
    }
    rnames <- rownames(model$recruitpars) %>%
      enframe(name = NULL)
    recs <- bind_cols(rnames, as_tibble(model$recruitpars)) %>%
      filter(Yr %in% recr) %>%
      rename(Name = value)
    lst <- lapply(seq_along(recs$Name), function(x){
      select(m, contains(recs$Name[x]))
    })
    df <- bind_cols(select(m, contains("R0")), lst)
    labels <- c("Equilibrium\nrecruitment\nlog(R0)",
                paste("Recruit\ndev.", recr))
  }
  if(!is.null(recr) & inc.key.params){
    str <- paste0("Recr_", recr)
    lst <- lapply(seq_along(str), function(x){
      select(m, starts_with(str[x]))
    })
    df <- bind_cols(df, lst)
    labels <- c(labels, paste0("Recruitment\n", recr))
  }
  if(!is.null(bratio) & inc.key.params){
    str <- paste0("Bratio_", bratio)
    lst <- lapply(seq_along(str), function(x){
      select(m, contains(str[x]))
    })
    df <- bind_cols(df, lst)
    labels <- c(labels, paste0("Relative\nspawning\nbiomass\n", bratio))
  }
  if(!is.null(forecatch) & inc.key.params){
    str <- paste0("ForeCatch_", forecatch)
    lst <- lapply(seq_along(str), function(x){
      select(m, contains(str[x]))
    })
    df <- bind_cols(df, lst)
    labels <- c(labels, paste0("Default\nharvest ", forecatch))
  }
  labels <- gsub(" +", "\n", labels)
  labels <- gsub("-", "-\n", labels)
  pairs(df,
        labels = labels,
        pch = ".",
        cex.labels = 1.2,
        xaxt = "n",
        yaxt = "n",
        las = 1,
        gap = 0.5,
        oma = c(0,0,0,0),
        lower.panel = panel.cor)
}

# MCMC trace plots for either age-2+ biomass or age-1 index
make.mcmc.survey.fit.plot <- function(model,         ## model is a model with an mcmc run which has the output of the
                                                     ##  r4ss package's function SSgetMCMC and has the extra_mcmc member
                                      start_yr,      ## Year to start the plot
                                      end_yr,        ## Year to end the plot
                                      surv_yrs,      ## Years in which the survey took place
                                      probs = c(0.025, 0.975), ## Confidence interval values lower and upper
                                      y.max = 5.5e6, ## maximum value for the y-axis
                                      samples = 1000, ## how many lines to show
                                      leg.cex = 1,    ## Legend tect size
                                      survey.type = "biomass", # or age1 for age-1 survey
                                      survey.col = "black"  # colour for survey points
                                      ){
  stopifnot(survey.type %in% c("biomass", "age1"))
  if(survey.type == "biomass"){
    y.lab = "Biomass index (million t)"
    survey.index = 2
    legend.text = c("Observed survey biomass with\ninput (wide) and estimated (narrow) 95% intervals",
                    "Median MCMC estimate of expected survey biomass",
                    paste0("A subset (", samples, ") of the MCMC estimates of expected survey biomass"))
    addedSE <- model$mcmc %>% summarise(across(starts_with("Q_extraSD_Acoustic"), ~median(.x)))
  } else {
    y.lab = "Relative age-1 index (billions of fish)"
    survey.index = 3
    legend.text = c("Observed age-1 index with\ninput (wide) and estimated (narrow) 95% intervals",
                    "Median MCMC estimate of scaled age-1 numbers",
                    paste0("A subset (", samples, ") of the MCMC estimates of scaled age-1 numbers"))
    addedSE <- model$mcmc %>% summarise(across(starts_with("Q_extraSD_Age1"), ~median(.x)))
  }

  ## Plot the fit of the model to the acoustic survey with 95% C.I.
  oldpar <- par()
  par(las = 1, mar = c(5, 4, 1, 1) + 0.1, cex.axis = 0.9)
  plot(0,
       type = 'n',
       xlim = c(start_yr-0.5, end_yr+0.5),
       xaxs = 'i',
       ylim = c(0, y.max),
       yaxs = 'i',
       axes = FALSE,
       xlab = "Year",
       ylab = y.lab)
  dat <- model$dat
  cpue <- dat$CPUE[dat$CPUE$index == survey.index,]
  segments(x0 = as.numeric(cpue$year),
           y0 = qlnorm(probs[1], meanlog = log(as.numeric(cpue$obs)), sdlog = as.numeric(cpue$se_log)),
           y1 = qlnorm(probs[2], meanlog = log(as.numeric(cpue$obs)), sdlog = as.numeric(cpue$se_log)),
           lwd = 3,
           lend = 1,
           col = survey.col)

  # subsamble to help the lines be more visible
  nsamp <- ncol(model$extra_mcmc$cpue.table) # total samples
  subsample <- floor(seq(1, nsamp, length.out=samples)) # subset (floor to get integers)

  # Need to do biomass and age1 surveys, this was what it was when only biomass,
  #  but the first rows happend to be that index:
  # y = model$extra_mcmc$cpue.table[1:length(start_yr:end_yr), subsample],
  # model$extra_mcmc$cpue.table is 54x8015, each row presumably corresponding to
  #  a row of model$dat$CPUE. -survey.index here corresponds to non-survey years:
  y.vals.to.plot <- model$extra_mcmc$cpue.table[model$dat$CPUE$index %in%
                                                c(survey.index, -survey.index) &
                                                model$dat$CPUE$year %in%
                                                start_yr:end_yr,
                                                subsample]

  # If doesn't work then try results from (may have to transpose, or reshape):
  #  recr1 <- model$extra_mcmc$natage %>% dplyr::select("Yr", "1") %>%
  #    dplyr::filter(Yr %in% 1995:2021)

  # lines showing expected survey values include in-between years
  # where no survey took place and therefore are not included in surv_yrs
  matplot(x = start_yr:end_yr,
          y = y.vals.to.plot,
          col = rgb(0, 0, 1, 0.03),
          type = 'l',
          add=TRUE,
          lty = 1)
  lines(x = start_yr:end_yr,
        # y = model$extra_mcmc$cpue.median[1:length(start_yr:end_yr)],
        y = model$extra_mcmc$cpue.median[model$dat$CPUE$index %in%
                                         c(survey.index, -survey.index) &
                                         model$dat$CPUE$year %in%
                                         start_yr:end_yr],
        col = rgb(0, 0, 0.5, 0.7),
        lty = 1,
        lwd = 3)
  legend('topleft',
         legend = legend.text,
         lwd = c(NA,3,1),
         pch = c(21, NA, NA),
         bg = 'white',
         text.col = gray(0.6),
         col = c(survey.col,
                 rgb(0, 0, 0.5, 0.7),
                 rgb(0, 0, 1, 0.4)),
         cex = leg.cex,
         bty = 'n')
  # Estimated interval with added uncertainty
  arrows(
    x0 = as.numeric(cpue$year),
    x1 = as.numeric(cpue$year),
    y0 = qlnorm(probs[1], meanlog = log(as.numeric(cpue$obs)), sdlog = as.numeric(cpue$se_log) + addedSE[1,1]),
    y1 = qlnorm(probs[2], meanlog = log(as.numeric(cpue$obs)), sdlog = as.numeric(cpue$se_log) + addedSE[1,1]),
   length = 0.03, angle = 90, code = 3, col = survey.col
  )
  # Observed points
  points(
    x = cpue$year,  # model$cpue$Yr[model$cpue$Use==1],
    y = cpue$obs,   #model$cpue$Obs[model$cpue$Use == 1],
    pch = 1,
    col = survey.col)
  axis(1, at = cpue$year, cex.axis = 0.8, tcl = -0.6)
  axis(1,
       at = (start_yr-4):(end_yr+7),
       lab = rep("", length((start_yr-4):(end_yr+7))),
       cex.axis = 0.8,
       tcl = -0.3)
  box()
  axis(2, at = (0:20)*1e6, lab = 0:20, las = 1)
  par <- oldpar
}

#' Make a bar plot of the catchability parameter for a survey
#'
#' @param model The model to plot bars for
#' @param model2 A model to draw a median line for
#' @param type Either "age2plus" or "age1" for those two surveys
#' @param hist_color Bar color
#' @param hist_alpha Bar transparency
#' @param med_color Median line color
#' @param model2_med_color Model 2 median line color
#' @param model2_mle_color Model 2 median line transparency
#' @export
make.mcmc.catchability.plot <- function(model,
                                        model2 = NULL,
                                        type = "age2plus",
                                        hist_color = "grey60",
                                        hist_alpha = 0.5,
                                        med_color = "royalblue",
                                        #mle_color = "royalblue",
                                        model2_med_color = "red",
                                        model2_mle_color = "red"){
  hist_color <- get.shade(hist_color, (1 - hist_alpha) * 100)
  par(mar = c(3, 3, 1, 1))
  if(type == "age2plus"){
    vec <- model$extra_mcmc$Q_vector
    if(!is.null(model2)){
      vec2 <- model2$extra_mcmc$Q_vector
    }
  }else if(type == "age1"){
    vec <- model$extra_mcmc$Q_vector_age1
    if(!is.null(model2)){
      vec2 <- model2$extra_mcmc$Q_vector_age1
    }
  }else{
    stop("type must be either 'age2plus' or 'age1'")
  }
  hist(vec,
       breaks = seq(0, 1.1 * max(vec), 0.05),
       xlab = "Acoustic survey catchability (Q)",
       col = hist_color,
       border = hist_color,
       xaxs = 'i',
       yaxs = 'i',
       main = "")
  abline(v = median(vec),
         col = med_color,
         lwd = 2,
         lty = 1)
  #abline(v = model$cpue$Calc_Q[1],
  #       col = mle_color,
  #       lwd = 2,
  #       lty = 2)
  if(!is.null(model2)){
    abline(v = median(vec2),
           col = model2_med_color,
           lwd = 2,
           lty = 1)
    #abline(v = model2$cpue$Calc_Q[1],
    #       col = model2_mle_color,
    #       lwd = 2,
    #       lty = 2)
  }
  abline(h = 0)
}

## adapting from make.mcmc.depletion.plot, and values for make.recruitment.plot
## Plot of MCMC samples of recruitment, to help for Issue #836.
##
## make.mcmc.recruitment.plot(base_model, start_yr = start_yr, equil.yr = unfished_eq_yr)
## make.mcmc.recruitment.plot(base_model, start_yr = 2006, equil.yr = unfished_eq_yr, samples = 100)
## make.mcmc.recruitment.plot(base_model, start_yr = 2006, equil.yr = unfished_eq_yr, samples = 100, rescale = TRUE)
## make.mcmc.recruitment.plot(base_model, start_yr = 1966, equil.yr = unfished_eq_yr, samples = NULL, rescale = TRUE, traceplot = FALSE)
make.mcmc.recruitment.plot <- function(model,         ## model is a model with an mcmc run which has the output of the
                                                    ##  r4ss package's function
                                                    ##  SSgetMCMC and has the extra_mcmc member
                                       equil.yr,
                                       start_yr,      ## Year to start the plot
                                       end_yr = 2023, ## Year to end the plot
                                       plot_R0 = TRUE,## Show R0 (only tested on
                                                      ## 2023 fig)
                                       plot_rescale_yr = TRUE,  ## Line at 1.0
                                       plot_rescale_yr_col = "darkgreen",
                                       y.max = NULL,  ## maximum value for the y-axis
                                       samples = 1000,## how many lines to show
                                       rescale = FALSE, ## whether to rescale by
                                                        ## a certain year's recruitment
                                       rescale.yr = 2010,  # year to rescale by
                                         # if rescaling, not fully functional
                                         # yet though
                                       traceplot = TRUE,   # do a traceplot of
                                         # MCMC samples else a median and 95%
                                       # interval plot
                                       highlight = FALSE,  # whether to
                                         # just highlight some colour-code
                                         # samples instead of many same colour
                                         # ones (superimposing is too busy)
                                       highlight.num = 5,   # how many to
                                         # highlight, then highlight them
                                         # equally spaced out (in order of 2010
                                         # recruitment), plus the 2nd and
                                         # penultimate samples.
                                       highlight.cols = c("darkgreen",
                                                          "red",
                                                          "pink",
                                                          "black",
                                                          "orange",
                                                          "gold",
                                                          "brown",
                                                          "darkgrey",
                                                          "violet",
                                                          "black",
                                                          "lightgrey"),
                                       highlight.lwd = 2,
                                       auto.xaxis = TRUE,  # label normally, else
                                       # use bigticks
                                       col.intervals.not.rescale = "blue", # intervals col
                                             # when not rescaling
                                       col.intervals.rescale = "red", # col when
                                       # rescaling
                                       trace.col.not.rescale = rgb(0, 0, 1,
                                                                   0.2), # paleblue
                                       trace.col.rescale = rgb(1, 0, 0, 0.2)
                                       ){
  oldpar <- par()

  par(las = 1, mar = c(5, 4, 1, 1) + 0.1, cex.axis = 0.9)

  dat <- as_tibble(model$mcmc) %>%
    select(c("Recr_Virgin",
             paste0("Recr_", start_yr):paste0("Recr_", end_yr))) / 1e6  # convert
                                        # from thousands to billions

  if(rescale){
    dat <- dat / dat$"Recr_2010"      # generalise ***
    if(is.null(y.max)){
      y.max <- 1.2                    # so will miss some super high ones, but
                                      # easier to see others
    }

    dat_R0 <- select(dat, "Recr_Virgin")  # Just R0
    dat <- select(dat, -"Recr_Virgin")

    col.intervals <- col.intervals.rescale
    trace.col  <- trace.col.rescale
    yLab <- paste0("Age-0 recruits relative to those in ", rescale.yr)
  } else {
    yLab <- "Age-0 recruits (billions)"
    col.intervals <- col.intervals.not.rescale
    trace.col  <- trace.col.not.rescale
  }

  # subsamble to help the lines be more visible
  nsamp <- nrow(dat)    # total samples
  if(!is.null(samples)){
    subsample <- floor(seq(1, nsamp, length.out=samples)) # subset (floor to get
                                                          # integers)
    dat_sub <- dat[subsample, ]
  } else {                                                # no subsampling
    dat_sub <- dat
  }

  if(is.null(y.max)){
    y.max <- max(max(dat_sub) * 0.8)  # so will miss some super high ones, but
                                      # easier to see others
  }

  plot(0,
       type = 'n',
       xlim = c(start_yr-0.5, end_yr+0.5),
       ylim = c(0, y.max),
       axes = FALSE,
       xlab = "Year",
       ylab = yLab)

  abline(h = 0, col = "lightgrey")
  # paleblue <- rgb(0, 0, 1, 0.2)

  if(traceplot){
    if(!highlight){
      matplot(x = start_yr:end_yr,
            y = t(dat_sub),
            col = trace.col,
            type = 'l',
            add=TRUE,
            lty = 1)
    }
    if(highlight){
      highlight.rows <- c(2,
                          round(1:(highlight.num-1) * (samples/(highlight.num -
                                                                1))))
      highlight.rows[length(highlight.rows)] <-
        highlight.rows[length(highlight.rows)] - 1 # so penultimate
      dat_sub_highlight <- as_tibble(dat_sub) %>%
        arrange(Recr_2010)
      dat_sub_highlight <- dat_sub_highlight[highlight.rows, ]
      matplot(x = start_yr:end_yr,
              y = t(dat_sub_highlight),
              col = highlight.cols,
              type = 'l',
              add=TRUE,
              lty = 1,
              lwd = highlight.lwd)
    }
  } else {        # medians and intervals
    if(plot_R0){
      R0_0.025 <- apply(dat_R0, 2, quantile, probs = 0.025)
      R0_med <- apply(dat_R0, 2, median)
      R0_0.975 <- apply(dat_R0, 2, quantile, probs = 0.975)
      rect(start_yr - 5,
           R0_0.025,
           end_yr + 5,
           R0_0.975,
           col = "lightgrey",
           border = NA)
      abline(h = R0_0.025, lty = 3)
      abline(h = R0_med, lty = 2)
      abline(h = R0_0.975, lty = 3)
    }

    # Plot horizontal line at 1:
    if(plot_rescale_yr){
      abline(h = 1,
             col = plot_rescale_yr_col,
             lty = 2)
    }

    lower <- apply(dat_sub, 2, quantile, probs = 0.025)
    medians <- apply(dat_sub, 2, median)
    upper <- apply(dat_sub, 2, quantile, probs = 0.975)


    yrs <- start_yr:end_yr
    points(yrs, medians, pch = 20)
    segments(x0 = yrs,
             y0 = lower,
             x1 = yrs,
             y1 = upper,
             col = col.intervals)
  }

  box()
  if(auto.xaxis){
    axis(1, at = start_yr:end_yr) #cex.axis = 0.8, tcl = -0.6)
    } else {
    axis(1, at = big_ticks)
    axis(1,
         at = small_ticks,
         lab = rep("",length(small_ticks)), tcl = -0.3)
    }
  axis(2)  #, at = 0:5, las = 1)
  par <- oldpar
}
