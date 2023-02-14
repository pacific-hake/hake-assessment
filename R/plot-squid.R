#' Make the retrospective recruitment deviations plot (AKA 'squid plot')
#'
#' @param model Model with retrospectives
#' @param subplot 1 = Recruitment deviations, 2 = Rec dev strength relative to most recent estimate
#' @param cohorts Vector of cohort years to plot (for labels)
#' @param plot_mcmc If `TRUE` plot the MCMC version based on medians. If `FALSE` plot MLE version.
#' @param getdevs A logical value specifying if the plot should be of
#' recruitment deviations, which is the default. If `FALSE`, then
#' the squid plot will be made using absolute recruitment instead of
#' deviations.
#'
#' @export
plot_squid <- function(model,
                       subplot = 1,
                       cohorts,
                       plot_mcmc = TRUE,
                       getdevs = TRUE){

  oldpar <- par()
  on.exit(par(oldpar))
  mdl_list <- list(model)
  retro_list <- map(model$retros, ~{.x})
  models <- c(mdl_list, retro_list)
  # End years are different for all the models, which messes up the plot
  # so set them all to the base model end year
  end_yr <- models[[1]]$endyr
  models <- map(models, function(mdl){
    mdl$endyr <- end_yr
    mdl
  })
  compare_summary <- SSsummarize(models, SpawnOutputUnits = "biomass", verbose = FALSE)
  neg_yr_vec <- seq(0, -(length(models) - 1), -1)
  endyrvec <- compare_summary$endyrs + 1 + neg_yr_vec
  plot_retro_recruits(compare_summary,
                      endyrvec = endyrvec,
                      cohorts = cohorts,
                      ylim = NULL,
                      uncertainty = FALSE,
                      main = "",
                      mcmcVec = plot_mcmc,
                      devs = getdevs,
                      relative = ifelse(subplot == 1, FALSE, TRUE),
                      labelyears = TRUE,
                      legend = FALSE,
                      leg.ncols = 4)
}
