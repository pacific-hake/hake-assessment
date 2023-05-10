#' Create plots during the loading phase, and return a list of them to be
#' attached to the model object for saving in a model RDS file
#'
#' @details
#' Uses the [probs] vector which is included in the package data for
#' this package
#'
#' @param model A model, created by [create_rds_file()]
#'
#' @return A list of [ggplot2::ggplot()] objects
plot_during_loading <- function(model){

  out <- list()

  # Plot the survey fit with many MCMC posterior lines
  # This is done MUST be made here because the MCMC object is not stored
  # in the RDS files
  out$survey_fit <- plot_survey_fit_mcmc(model,
                                         type = "acoustic",
                                         n_posts = 1000)

  out$age1_index_fit <- plot_survey_fit_mcmc(model,
                                             type = "age1",
                                             n_posts = 1000,
                                             ylim = c(0, 10))

  # out$prior_posterior <- plot_priors_vs_posts(base_model,
  #                                             key_posteriors,
  #                                             titles = key_posteriors_titles,
  #                                             x_range = "prior",
  #                                             ncol = 2,
  #                                             nrow = 3,
  #                                             labeller = label_parsed_space)

  out
}
