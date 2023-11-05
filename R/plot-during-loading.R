#' Create plots during the loading phase, and return a list of them to be
#' attached to the model object for saving in a model RDS file
#'
#' @details
#' Uses the [probs] vector which is included in the package data for
#' this package
#' Primarily used for plots that directly access the `mcmc` output because
#' that is not included in the model object to reduce model object size
#'
#' @param model A model, created by [create_rds_file()]
#'
#' @return A list of [ggplot2::ggplot()] objects
plot_during_loading <- function(model){

  # Turned off for now. ggplot uses a LOT of memory as it stores the entire
  # environment for some reason, soi this attempt did not pan out
  #out <- list()
  out <- NULL

  # Plot the survey fit with many MCMC posterior lines
  # This is done MUST be made here because the MCMC object is not stored
  # in the RDS files
  # out$survey_fit <- plot_survey_fit_mcmc(model,
  #                                        type = "acoustic",
  #                                        n_posts = 1000,
  #                                        glow = TRUE)
  #
  # out$age1_index_fit <- plot_survey_fit_mcmc(model,
  #                                            type = "age1",
  #                                            n_posts = 1000,
  #                                            ylim = c(0, 10),
  #                                            glow = TRUE)
  #
  # out$selex_posteriors <- {
  #   plist <- list()
  #   plist[[1]] <- plot_selex_posteriors(model,
  #                                       type = "survey",
  #                                       probs = probs,
  #                                       age_range = c(1, 8),
  #                                       show_xlab = FALSE,
  #                                       post_med_line_color = "red3",
  #                                       unc_line_color = "red3",
  #                                       glow = TRUE)
  #   plist[[2]] <- plot_selex_posteriors(model,
  #                                       type = "fishery",
  #                                       probs = probs,
  #                                       age_range = c(1, 8),
  #                                       glow = TRUE)
  #   plot_grid(plotlist = plist, nrow = 2, ncol = 1)
  # }
  #
  #
  # out$stock_recr <- plot_stock_recruitment(model,
  #                                          probs = probs)
  #
  # out$catch_fore_density <- plot_catch_forecast_density(base_model,
  #                                                       yr = end_yr)

  # out$prior_posterior <- plot_priors_vs_posts(base_model,
  #                                             key_posteriors,
  #                                             titles = key_posteriors_titles,
  #                                             x_range = "prior",
  #                                             ncol = 2,
  #                                             nrow = 3,
  #                                             labeller = label_parsed_space)

  out
}
