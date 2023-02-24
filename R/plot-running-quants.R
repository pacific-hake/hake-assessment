#' Create a plot with running quantiles across the posterior for a parameter
#'
#' @param model A model list, created by [create_rds_file()]
#' @param post_regex A regular expression that matches one parameter name
#' and nothing else
#' @param probs A vector of 3 values for the lower, median, and upper
#' quantiles
#' @param rib_alpha The alpha value for the credible interval ribbon
#' @param y_lim The limits for the y-axis. A vector of 2
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_running_quants <- function(model,
                                post_regex,
                                probs = c(0.025, 0.5, 0.975),
                                rib_alpha = 0.2,
                                y_lim = NULL){

  mtch <- grep(post_regex, names(model$mcmc))

  if(!length(mtch)){
    stop("The regex `", post_regex, "` did not match any names in the mcmc ",
         "data frame",
         call. = FALSE)
  }

  if(length(mtch) > 1){
    stop("The regex `", post_regex, "` matched more than one name in the ",
         "mcmc data frame",
         call. = FALSE)
  }

  if(length(probs) != 3){
    stop("`probs` must be a vector of 3 values", call. = FALSE)
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
    geom_ribbon(alpha = rib_alpha) +
    ylab("")

  if(is.null(y_lim)){
    y_lim <- c(lowest, highest)
  }

  g <- g +
    ylim(y_lim)

  g
}
