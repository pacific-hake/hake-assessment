#' Create a plot with running quantiles across the posterior for a parameter
#'
#' @param model A model list, created by [create_rds_file()]
#' @param post_regex A regular expression that matches one parameter name
#' and nothing else
#' @param probs A vector of 3 values for the lower, median, and upper
#' quantiles
#' @param fill The fill color for the credible interval ribbon
#' @param alpha The alpha value for the credible interval ribbon
#' @param y_lim The limits for the y-axis. A vector of 2
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_running_quants <- function(model,
                                post_regex,
                                y_lim = NULL,
                                fill = "royalblue",
                                alpha = 0.3){

  mtch <- grep(post_regex, names(model$mcmc))

  if(!length(mtch)){
    stop("The regex `", post_regex, "` did not match any names in the mcmc ",
         "data frame")
  }

  if(length(mtch) > 1){
    stop("The regex `", post_regex, "` matched more than one name in the ",
         "mcmc data frame")
  }

  if(length(probs) != 3){
    stop("`probs` must be a vector of 3 values")
  }

  mc <- model$mcmc |>
    select(matches(post_regex)) |>
    as_tibble(rownames = "Iteration") %>%
    transmute(Iteration = as.numeric(Iteration),
              Value = .[[2]])

  quant_fun <- function(x, prob){
    quantile(x, probs = prob, names = FALSE)
  }

  draws <- nrow(mc)
  quant_df <- map(probs, ~{
    running(mc$Value,
            fun = quant_fun,
            prob = .x,
            allow.fewer = TRUE,
            width = draws)
  }) |>
    setNames(c("lower", "med", "upper")) |>
    map_dfc(~{.x})

  mc <- mc |>
    bind_cols(quant_df) |>
    select(-Value)

  lowest <- min(mc$lower)
  highest <- max(mc$upper)

  g <- ggplot(mc, aes(x = Iteration,
                      y = med,
                      ymin = lower,
                      ymax = upper)) +
    geom_line() +
    geom_ribbon(fill = fill,
                alpha = alpha) +
    scale_x_continuous(labels = comma) +
    ylab("")

  if(is.null(y_lim)){
    y_lim <- c(lowest, highest)
  }

  g <- g +
    ylim(y_lim)

  g
}
