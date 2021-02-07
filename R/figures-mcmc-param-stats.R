#' Plot Autocorrelation, Effective sample size, Geweke statistic, and
#' Heidelberger & Welch statistic histograms. This is a re-coded version of
#' https://github.com/r4ss/r4ss/blob/bioscale/R/mcmc.nuisance.R
#'
#' @param model A model as output by [load_models()]
#' @param col A color to shade the bars
#'
#' @return A [barplot()]
#' @export
plot_mcmc_param_stats <- function(model,
                                  col = get.shade(color = "blue", opacity = 30)){

  oldpar <- par("mar", "oma")
  on.exit(par(oldpar))
  par(mar = c(5, 4, 0, 0.5),
      oma = c(0, 0, 0.5, 0.5))

  parm_nm <- model$post_names
  parm_nm <- parm_nm[parm_nm != ""]
  mc <- model$mcmc %>%
    select(tidyselect::all_of(parm_nm))

  draws <- nrow(mc)
  stats <- dplyr::bind_cols(tibble::enframe(rep(0, length(parm_nm)), name = NULL, value = "autocor"),
                 enframe(rep(0, length(parm_nm)), name = NULL, value = "geweke"),
                 enframe(rep(0, length(parm_nm)), name = NULL, value = "effn"),
                 enframe(rep(0, length(parm_nm)), name = NULL, value = "heidelwelsch"),
                 enframe(parm_nm, name = NULL, value = "label"))

  hwsums <- c(0, 0, 0)

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
    effsize <- coda::effectiveSize(coda::mcmc(.x))
    effnuse <- round(effsize, 0)
    stats$effn[.y] <<- min(effnuse, draws)

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

  hist(stats$effn,
       main = "",
       ylab = "",
       xlab = "Effective sample size",
       breaks = c(seq(0, draws, by = (draws / 10))),
       xlim = c(0, draws),
       col = col)

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
