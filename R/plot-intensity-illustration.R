#' Create a plot illustrating the fishing intensity as a function of SPR
#'
#' @param target The target value for SPR as a proportion between 0 and 1
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_intensity_illustration <- function(target = 0.4){

  spr <- seq(0, 1, length = 100)
  intensity = (1 - spr) / (1 - target)
  d <- tibble(spr = spr,
              intensity = intensity) |>
    mutate(spr_1 = 1 - spr)
  max_segment_ind <- min(which(d$intensity <= 1))
  max_segment <- d |>
    slice(max_segment_ind)

  ggplot(d,
         aes(x = spr_1,
             y = intensity)) +
    geom_line(linewidth = 2,
              color = "royalblue",
              lineend = "round") +
    geom_vline(xintercept = 0.6,
               linetype = "dashed") +
    geom_segment(aes(x = 0,
                     xend = max_segment$spr_1,
                     y = 1,
                     yend = 1),
                 linetype = "dashed") +
    annotate("text",
             x = max_segment$spr_1 - 0.05,
             y = 1.35,
             label = "Target SPR",
             angle = 90,
             size = 5) +
    scale_x_continuous(breaks = seq(0, 1, 0.2),
                       sec.axis = sec_axis(~ .,
                                           breaks = seq(0, 1, 0.2),
                                           labels = c("1.0",
                                                      "0.8",
                                                      "0.6",
                                                      "0.4",
                                                      "0.2",
                                                      "0.0"),
                                           name = "SPR")) +
    scale_y_continuous(breaks = seq(0, 1.5, 0.5)) +
    xlab("1 - SPR") +
    ylab("Fishing intensity") +
    theme(plot.margin = margin(c(20, 6, 6, 6)))
}