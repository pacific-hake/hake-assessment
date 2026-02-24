#' Make a time series plot of the management-chosen TAC, assessment
#' estimated TAC, and realized catch
#'
#' @param inc_biomass_ests If `TRUE`, include a line representing the estimated
#' biomass (medians)
#' @param inc_survey If `TRUE`, include the survey biomass estimates as
#' provided by the survey team (not fits from the model)
#' @param line_type Which line type to use for connecting the
#' values with vertical lines
#' @param line_width Width of the connecting lines
#' @param line_alpha Which alpha level (0-1) to use for connecting
#' the values with vertical lines
#' @param leg_pos The legend position as x-y coordinates (vector of 2)
#' @param leg_ncol The number of columns to split the legend into
#' @param leg_font_size The legend font size
#' @param point_size The point size
#' @param y_breaks A vector of values to show on the y-axis
#' @param x_labs_mod Show a year label every Nth tick, This is N. Default 2
#' @param ... Arguments passed to [setup_data_frame_for_past_management_plots()]
#' Includes the catch/tac data frame, model, and arguments to include biomass
#' estimates and diffs (differences between subsequent year proportions)
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_management_catch_vs_tac_diffs <- \(inc_biomass_ests = FALSE,
                                        inc_survey = FALSE,
                                        line_type = "solid",
                                        line_width = 1,
                                        line_alpha = 0.5,
                                        leg_pos = c(0.65, 0.83),
                                        leg_ncol = 1,
                                        leg_font_size = 12,
                                        point_size = 3,
                                        font_size = 10,
                                        right_cols_cm = 1.8,
                                        y_breaks = seq(-1, 1, 0.1),
                                        x_labs_mod = 2,
                                        ...){

  d <- setup_data_frame_for_past_management_plots(inc_biomass_ests = inc_biomass_ests,
                                                  inc_survey = inc_survey,
                                                  diffs = TRUE,
                                                  ...)

  y_min <- min(d$value)
  y_min <- floor(y_min * 10) / 10
  y_max <- max(d$value)
  y_max <- ceiling(y_max * 10) / 10

  x_min <- d$Year |> min()
  x_max <- d$Year |> max()
  x_breaks <- d$Year |> unique() |> sort()
  x_labels <- x_breaks
  x_labels[x_labels %% x_labs_mod == 1] <- ""

  g <- ggplot(d, aes(x = Year,
                     y = value,
                     color = name,
                     shape = name)) +
    # Add a solid horizontal line at 0 for clarity on where 0 is
    geom_hline(yintercept = 0,
               linewidth = 0.3,
               linetype = "solid") +
    geom_hline(yintercept = y_breaks,
               linewidth = 0.1,
               linetype = "dashed") +
    geom_point(size = point_size) +
    geom_line(aes(group = name, color = name),
              linetype = line_type,
              linewidth = line_width,
              alpha = line_alpha) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = leg_font_size),
          # Following make the legend smaller and legend items closer together
          legend.key.size = unit(0.25, "cm"),
          legend.text.align = 0,
          legend.spacing.y = unit(0.01, "cm")) +
    guides(color = guide_legend(byrow = TRUE)) +
    scale_y_continuous(labels = comma,
                       limits = c(y_min, y_max),
                       breaks = y_breaks,
                       expand = expansion(mult = c(0, 0))) +
    scale_x_continuous(labels = x_labels,
                       limits = c(x_min, x_max),
                       breaks = x_breaks) +
    labs(y = "Prop. changed from previous year")

  if(is.null(leg_pos[1]) || is.na(leg_pos[1])){
    g <- g +
      theme(legend.position = "none")
  }else{
    g <- g +
      theme(legend.position = leg_pos) +
      guides(fill = guide_legend(ncol = leg_ncol),
             color = guide_legend(ncol = leg_ncol,
                                  label.hjust = 0),
             shape = guide_legend(label.hjust = 0))
  }

  g
}