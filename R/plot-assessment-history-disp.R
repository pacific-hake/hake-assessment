#' Create Interquartile range or quartile coefficient of dispersion plots
#' for the assessment history spawning biomass estimates
#'
#' @param base_model A model object, created by [create_rds_file()]
#' @param history_disp_df A data frame, previously read in from the file
#' `assessment-history-probs.csv`
#' @param type One of 'inter' or 'coeff' for Inter-quartile range or
#' Quartile coefficient of dispersion respectively
#' @param ylim A vector of two representing the minimum and maximum values to
#' show on the y-axis
#' @param x_breaks A vector of the values to show on the x-axis
#' @param point_shape A numeric point shape code
#' @param point_size Size of the points
#' @param line_width Width of the line
#'
#' @return a [ggplot2::ggplot()] object
#' @export
plot_assessment_history_disp <- function(base_model,
                                         history_disp_df,
                                         type = c("inter", "coeff"),
                                         y_lim = c(0, 2000),
                                         x_breaks = seq(2012, 2022, 2),
                                         point_shape = 1,
                                         point_size = 2,
                                         line_width = 1){

  type <- match.arg(type)
  y_lab <- ifelse(type == "inter",
                  "Interquartile Range of Spawning Biomass (kt)",
                  "Quartile Coefficient of Dispersion")

  names(history_disp_df) <- tolower(names(history_disp_df))

  d <- history_disp_df |>
    select(assessment_year,
           ifelse(type == "inter",
                  "interquartilerange",
                  "quartilecoeffdispersion")) |>
    rename(value = 2)

  g <- ggplot(d,
              aes(x = assessment_year,
                  y = value)) +
    geom_line(linewidth = line_width) +
    geom_point(size = point_size,
               shape = point_shape) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(labels = comma,
                       limits = y_lim) +
    xlab("Assessment Year") +
    ylab(y_lab)

  g
}
