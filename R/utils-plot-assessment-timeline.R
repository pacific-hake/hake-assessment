#' Create a plot showing the relative numbers of days we have had to
#' produce the assessment each year since 2017
#'
#' @param yrs A vector of years to plot
#' @param days A vector of days required each year to produce the
#'  assessment. Must be the same length as `yrs`
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_assessment_timeline <- function(yrs = c(2017, 2018, 2019, 2020,
                                             2021, 2022, 2023),
                                     days = c(17, 26, 23, 25,
                                              23, 18, 36)){

  if(length(yrs) != length(days)){
    stop("`yrs` aand `days` must be the same length")
  }

  d <- tibble(yrs = yrs,
              days = days) |>
    mutate(rel_days = round((days / max(days)) * 100, 0)) |>
    mutate(yrs = as.numeric(yrs))
  d_odd <- d |>
    dplyr::filter(yrs %% 2 == 1)
  d_even <- d |>
    dplyr::filter(yrs %% 2 == 0)

  ggplot(d,
         aes(x = yrs,
             y = days)) +
    geom_path(linewidth = 1) +
    geom_point(data = d,
               size = 7,
               color = "white") +
    geom_label_repel(data = d_odd,
                     aes(label = paste0(rel_days, "%")),
                     nudge_y = -5, ) +
    geom_label_repel(data = d_even,
                     aes(label = paste0(rel_days, "%")),
                     nudge_y = -5,
                     color = "red") +
    geom_point(data = d,
               size = 3,
               color = "white") +
    geom_point(data = d_odd,
               shape = 1,
               size = 2,
               stroke = 1.5) +
    geom_point(data = d_even,
               shape = 1,
               size = 2,
               stroke = 1.5,
               color = "red") +
    geom_hline(yintercept = max(days),
               linetype = "dotted") +
    scale_x_continuous(breaks = seq(min(yrs), max(yrs), 1),
                       limits = c(min(yrs), max(yrs))) +
    scale_y_continuous(breaks = seq(0, 40, 10),
                       limits = c(0, 40)) +
    ylab("Business Days to Complete Draft Assessment") +
    xlab("Year") +
    geom_text(data = tibble(x = median(d$yrs),
                            y = 5,
                            label = "Values indicate the percentage of the maximum"),
              aes(x = x,
                  y = y,
                  label = label),
              inherit.aes = FALSE) +
    geom_text(data = tibble(x = median(d$yrs),
                            y = 3,
                            label = "(years with new survey data)"),
              aes(x = x,
                  y = y,
                  label = label),
              inherit.aes = FALSE,
              color = "red") +
    theme(axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12))
}