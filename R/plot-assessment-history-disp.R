#' Create Interquartile range or quartile coefficient of dispersion plots
#' for the assessment history spawning biomass estimates
#'
#' @details
#' Note that if you expect this function to work correctly outside of the
#' hake repository directories (e.g. in a testing directory you got to by
#' running `gotest()`), you must supply the full-path filename for the
#' 'assessment-history-dispersion.csv' file using the arg `fn` to this
#' function. It will be passed via `...` to `calc_retro_stats()`.
#'
#' @param model A model object, created by [create_rds_file()]
#' @param type One of 'inter' or 'coeff' for Inter-quartile range or
#' Quartile coefficient of dispersion respectively
#' @param ylim A vector of two representing the minimum and maximum values to
#' show on the y-axis
#' @param x_breaks A vector of the values to show on the x-axis. The defaults
#' `start_yr` and `end_yr` are assigned in the first few lines of the
#' function to be the limits of the data
#' @param point_shape A numeric point shape code
#' @param point_size Size of the points
#' @param point_color Color of the points
#' @param line_width Width of the line
#' @param line_color Color of the line
#' @param line_type Type of the line, e.g. "dashed", "solid", "dotted"
#' @param ... Arguments to pass to `calc_retro_stats()` (namely, `yr`)
#'
#' @return a [ggplot2::ggplot()] object
#' @export
plot_assessment_history_disp <- function(model,
                                         type = c("inter", "coeff"),
                                         y_lim = c(0, 2000),
                                         x_breaks = seq(start_yr, end_yr, 2),
                                         point_shape = ts_pointshape,
                                         point_size = ts_pointsize,
                                         point_color = ts_single_model_pointcolor,
                                         line_width = ts_linewidth,
                                         line_color = ts_single_line_color,
                                         line_type = ts_single_model_linetype,
                                         ...){

  type <- match.arg(type)
  y_lab <- ifelse(type == "inter",
                  "Interquartile Range of Spawning Biomass (kt)",
                  "Quartile Coefficient of Dispersion")

  history_disp_df <- calc_retro_stats(model, ...)
  names(history_disp_df) <- tolower(names(history_disp_df))

  start_yr <- min(history_disp_df$assessment_year)
  end_yr <- max(history_disp_df$assessment_year)

  d <- history_disp_df |>
    select(assessment_year,
           ifelse(type == "inter",
                  "interquartilerange",
                  "quartilecoeffdispersion")) |>
    rename(value = 2)

  g <- ggplot(d,
              aes(x = assessment_year,
                  y = value)) +
    geom_line(linewidth = line_width,
              linetype = line_type,
              color = line_color) +
    # Add glow
    geom_point(size = point_size + 1,,
               shape = point_shape,
               color = "white") +
    geom_point(size = point_size,
               shape = point_shape,
               color = point_color) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(labels = comma,
                       limits = y_lim) +
    xlab("Assessment Year") +
    ylab(y_lab)

  g
}
