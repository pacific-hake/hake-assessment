#' Create a plot of survey biomass as points with error   bars
#'
#' @details
#' Extra SD is shown for the 2009 "squid year"
#'
#' @param model A model, created by [create_rds_file()]
#' @param index The index type to plot
#' @param y_lim A vector of two for the minimum and maximum values
#' for the y-axis on the plot
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_survey_biomass <- function(model,
                                index = c("age1", "age2"),
                                y_lim = c(0, 3)){

  index <- match.arg(index)
  y_label <- ifelse(index == "age1",
                    "Relative age-1 index (billions of fish)",
                    "Biomass index (Mt)")
  brk_interval <- `if`(index == "age2", 0.5, 2)
  index <- `if`(index == "age2", 2, 3)

  ests <- model$dat$CPUE |>
    filter(index == !!index) |>
    transmute(year,
              obs = obs / 1e6,
              se_log,
              lo = exp(log(obs) - 1.96 * se_log),
              hi = exp(log(obs) + 1.96 * se_log))

  ests_squid <- ests |>
    filter(year == 2009) |>
    mutate(se_log = 0.0682) |>
    mutate(lo = exp(log(obs) - 1.96 * se_log),
           hi = exp(log(obs) + 1.96 * se_log))

  g <- ggplot(ests,
              aes(x = year,
                  xend = year,
                  y = lo,
                  yend = hi,)) +
    geom_segment(col = "grey",
                 linewidth = 1.5,
                 lineend = "round")

  if(index == 2){
    g <- g +
      geom_segment(data = ests_squid,
                   linewidth = 1.5,
                   lineend = "round")

  }

  g <- g +
    geom_point(aes(y = obs), size = 2) +
    scale_x_continuous(breaks = ests$year) +
    scale_y_continuous(breaks = seq(y_lim[1], y_lim[2], brk_interval),
                       expand = c(0, 0)) +
    coord_cartesian(ylim = y_lim) +
    ylab(y_label) +
    xlab("Year") +
    theme(# plot.margin: top, right,bottom, left
          # Needed to avoid tick labels cutting off
          plot.margin = margin(12, 12, 0, 0))

  g
}
