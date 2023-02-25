#' Plot Autocorrelation, Effective sample size, Geweke statistic, and
#' Heidelberger & Welch statistic histograms. This is a re-coded version of
#' https://github.com/r4ss/r4ss/blob/bioscale/R/mcmc.nuisance.R
#'
#' @param model A model as output by [create_rds_file()]
#' @param col A color to shade the bars
#' @param effn_labels If TRUE, add labels to the top of the bars on the effective sample size plot
#' @param ylim_mult Multiply the maximum ylim value by this (to show labels on bars better if `effn_labels` is `TRUE`)
#' @param show_ro_loc If TRUE, place the text "R0" over the bar in which the R0 parameter falls
#' @param ro_yloc If `show_ro_loc` is `TRUE`, use this value as a y value for the text
#' @param ro_yloc_arr_start If `show_ro_loc` is `TRUE`, use this value as a y value for the start of the arrow
#' pointing to the bar where R0 is
#' @param ro_yloc_arr_end If `show_ro_loc` is `TRUE`, use this value as a y value for the end of the arrow
#' pointing to the bar where R0 is
#'
#' @return A [barplot()]
#' @export
plot_mcmc_param_stats <- function(model,
                                  col = get.shade(color = "blue", opacity = 30),
                                  effn_labels = FALSE,
                                  ylim_mult = 1,
                                  show_ro_loc = FALSE,
                                  ro_yloc = 50,
                                  ro_yloc_arr_start = 60,
                                  ro_yloc_arr_end = 30){

  oldpar <- par("mar", "oma")
  on.exit(par(oldpar))
  par(mar = c(5, 4, 0, 0.5),
      oma = c(0, 0, 0.5, 0.5))

  parm_nm <- model$post_names
  parm_nm <- parm_nm[parm_nm != ""]
  mc <- model$mcmc %>%
    select(all_of(parm_nm))

  draws <- nrow(mc)
  stats <- bind_cols(enframe(rep(0, length(parm_nm)), name = NULL, value = "autocor"),
                     enframe(rep(0, length(parm_nm)), name = NULL, value = "geweke"),
                     enframe(rep(0, length(parm_nm)), name = NULL, value = "effn"),
                     enframe(rep(0, length(parm_nm)), name = NULL, value = "heidelwelsch"),
                     enframe(parm_nm, name = NULL, value = "label"))

  hwsums <- c(0, 0, 0)

  ro_loc <- NULL
  map2(mc, seq_along(mc), ~{
    acftemp <- acf(.x, lag.max = 1, type = "correlation", plot = FALSE)
    acoruse <- round(acftemp$acf[2], 3)
    stats$autocor[.y] <<- acoruse

    # Geweke statistic
    if(acoruse > 0.4){
      gewuse <- 3
    }else{
      geweke <- coda::geweke.diag(coda::mcmc(.x), frac1 = 0.1, frac2 = 0.5)
      gewuse <- round(geweke$z, 3)
    }
    if(gewuse > 3){
      gewuse <- 3
    }else if(gewuse < -3){
      gewuse <- -2.9
    }
    stats$geweke[.y] <<- gewuse

    # Effective sample size
    x <- enframe(.x, name = NULL)
    spec <- coda::spectrum0.ar(x)$spec
    effsize <- round(ifelse(spec == 0, 0, nrow(x) * var(x) / spec), 0)
    stats$effn[.y] <<- min(effsize, draws)
    if(names(mc)[.y] == "SR_LN(R0)"){
      ro_loc <<- stats$effn[.y]
    }
    # Heidelberger and Welch statistic
    if(acoruse > 0.4){
      hwuse <- "No test"
      hwsums[1] <<- hwsums[1] + 1
    }else{
      hw <- as.list(coda::heidel.diag(coda::mcmc(.x), pvalue = 0.05))
      if(hw[1] == 0){
        hwuse <- "Failed"
        hwsums[2] <<- hwsums[2] + 1
      }else if(hw[1] == 1){
        hwuse <- "Passed"
        hwsums[3] <<- hwsums[3] + 1
      }
    }
    stats$heidelwelsch[.y] <<- hwuse
    NULL
  })

  # Plotting section
  par(new = FALSE,
      mfrow = c(2, 2))

  hist(stats$autocor,
       main = "",
       col = col,
       breaks = c(seq(-1, 1, by = 0.1)),
       xlim = c(-1, 1),
       xlab = "Autocorrelation")
  mtext("Summary of nuisance parameters",
        side = 3,
        adj = 0,
        line = 2,
        font = 2,
        cex = 1.5)

  j <- hist(stats$effn,
            breaks = c(seq(0, draws, by = (draws / 10))),
            plot = FALSE)

  hist(stats$effn,
       main = "",
       ylab = "",
       xlab = "Effective sample size",
       breaks = c(seq(0, draws, by = (draws / 10))),
       xlim = c(0, draws),
       ylim = c(0, max(j$counts) * ylim_mult),
       labels = effn_labels,
       col = col)
  if(show_ro_loc && !is.null(ro_loc)){
    freq_ind <- first(which(ro_loc < j$breaks)) - 1
    x_loc <- (j$breaks[freq_ind] + j$breaks[freq_ind + 1]) / 2
    arrows(x0 = x_loc, y0 = ro_yloc_arr_start, x1 = x_loc, y1 = ro_yloc_arr_end, length = 0.05)
    text(x = x_loc,
         y = ro_yloc,
         paste0("R0 (", ro_loc, ")"))
  }

  hist(stats$geweke,
       main = "",
       xlab = "Geweke statistic",
       breaks = c(seq(-5, 5, by = 0.25)),
       xlim = c(-3, 3),
       right = TRUE,
       col = col)

  barplot(hwsums,
          space = 0,
          ylab = "",
          col = col,
          xlab = "Heidelberger and Welch statistic",
          names.arg = c("No test", "Failed", "Passed"))
}

#' Make the caption for [plot_mcmc_param_stats]
#' @param modelname A character string to represent the model name that
#' will be pasted into the caption.
plot_mcmc_param_stats_makecaption <- function(modelname){
  paste("Summary histograms of MCMC diagnostics for all", modelname, "model parameters.",
        "The level of autocorrelation in the chain",
        "(distribution across lag times, i.e., distance between samples in the chain, shown in the top left panel)",
        "influences the effective sample size (top right panel)",
        "used to estimate posterior distributions.",
        "The Geweke statistic (lower left panel) tests for equality between",
        "means located in the first part of the chain against means in the last part of the chain.",
        "The Heidelberger and Welch statistic (lower right panel)",
        "tests if the sampled values come from a stationary distribution by comparing different sections of the chain.")
}
