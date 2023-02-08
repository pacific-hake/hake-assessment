# 2.	Plot residuals of age-1 index (compared to estimated recruitment) against
# the predicted age-1 index (or the magnitude of the (actual) recruitments). Could
# be based on absolute values or in log scale; preferably both. 

joined <- dplyr::left_join(
  base_model$dat$CPUE %>%
    dplyr::filter(index == 3) %>%
    dplyr::select(year, obs),
  base_model$extra.mcmc$index.med %>%
    dplyr::filter(Fleet == 3) %>%
    dplyr::ungroup() %>%
    dplyr::select(Yr, value),
  by = c(year = "Yr")
) %>%
  dplyr::mutate(
    residual = value - obs,
    log_predicted = log(value),
    log_residual = log(value) - log(obs)
  )

png("test.png", width = 10, height = 10, units = "in", res = 600)
gg <- ggplot2::ggplot(
  data = joined,
  ggplot2::aes(value, residual)
) +
  ggplot2::geom_point() +
  ggplot2::xlab("Predicted age-1 index") +
  ggplot2::ylab("Residual = predicted - observed")
dev.off()

png("test_log.png", width = 10, height = 10, units = "in", res = 600)
gg <- ggplot2::ggplot(
  data = joined,
  ggplot2::aes(log_predicted, log_residual)
) +
  ggplot2::geom_point() +
  ggplot2::xlab("Log of predicted age-1 index") +
  ggplot2::ylab("Log of predicted - log of observed")
gg
dev.off()
