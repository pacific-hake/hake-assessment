#' Plot weight-at-age data
#'
#' @param data A data frame of weight-at-age values.
#' @param maxage The maximum age in the modelled data.
#' @return A {ggplot2} object.
#' @author Kelli F. Johnson
#' @export
plot_weight_at_age_simple <- function(data, maxage) {
  ggplot2::ggplot(
    data = data %>%
      dplyr::mutate(
        cat = ifelse(
          test = grepl("acoustic", Source, ignore.case = TRUE),
          "Survey",
          "Fishery"
        )
      ),
    ggplot2::aes(
      x = Year,
      y = Weight_kg,
      colour = factor(Age_yrs),
      group = Age_yrs
    )
  ) +
    ggplot2::stat_summary(fun = mean, geom = "line") +
    ggplot2::stat_summary(fun = mean, geom = "point") +
    ggplot2::ylab("Mean weight-at-age (kg)") +
    ggplot2::labs(col = "Age\n(years)") +
    ggplot2::scale_color_manual(values = rich.colors.short(maxage + 5)) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"))
}
