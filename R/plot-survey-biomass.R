#' Create a plot of survey biomass as points with error   bars
#'
#' @details
#' Extra SD is shown for the 2009 "squid year"
#'
#' @param model A model, created by [create_rds_file()]
#' @param index The index type to plot
#' @param y_lim A vector of two for the minimum and maximum values
#' for the y-axis on the plot
#' @param tick_prop A value that the length of the major tick marks are
#' multiplied by. This proportion must be set by trial and error. Make sure
#' to change `vjust_x_labels` so the labels are not overlapping the lines or
#' are too far away from the lines
#' @param vjust_x_labels Adjustment to move the x-axis tick labels and label
#' up or down. Negative numbers move down
#' @param remove_yr_labels A vector of years to remove the ,labels for in
#' case they are overlapping
#' @param x_labs_mod How many years between year labels on the x-axis
#' @param alpha The transparency for the error bars (non-squid). Transparency
#' for the squid error bars are 1
#' @param point_size The size of the points (See [ggplot2::geom_point()] for
#' more details)
#' @param point_color The R color for the points (See [ggplot2::geom_point()]
#' for more details)
#' @param point_shape The R shape number for the points (See
#' [ggplot2::geom_point()] for more details)
#' @param point_stroke The stroke value for the points (See
#' [ggplot2::geom_point()] for more details)
#' @param line_width Width of the error bar lines
#' @param line_color R color for the error bar lines
#' @param line_type Line type for the error bar lines
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_survey_biomass <- function(model,
                                index = c("age1", "age2"),
                                x_lim = c(survey_start_yr, survey_end_yr),
                                y_lim = c(0, 3),
                                x_labs_mod = 5,
                                alpha = 0.3,
                                tick_prop = 1,
                                vjust_x_labels = -1.5,
                                remove_yr_labels = NULL,
                                point_size = ts_single_model_pointsize,
                                point_color = ts_single_model_pointcolor,
                                point_shape = ts_single_model_pointshape,
                                point_stroke = ts_single_model_pointstroke,
                                line_width = ts_single_model_linewidth,
                                line_color = ts_single_model_linecolor,
                                line_type = ts_single_model_linetype,
                                ...){

  index <- match.arg(index)
  y_label <- ifelse(index == "age1",
                    "Relative age-1 index (billions of fish)",
                    "Biomass index (Mt)")
  brk_interval <- `if`(index == "age2", 0.5, 2)
  index <- `if`(index == "age2", 2, 3)

  ests <- model$dat$CPUE |>
    dplyr::filter(index == !!index) |>
    transmute(year,
              obs = obs / 1e6,
              se_log,
              lo = exp(log(obs) - 1.96 * se_log),
              hi = exp(log(obs) + 1.96 * se_log))

  ests_squid <- ests |>
    dplyr::filter(year == 2009) |>
    mutate(se_log = 0.0682) |>
    mutate(lo = exp(log(obs) - 1.96 * se_log),
           hi = exp(log(obs) + 1.96 * se_log))

  g <- ggplot(ests,
              aes(x = year,
                  xend = year,
                  y = lo,
                  yend = hi,)) +
    geom_segment(alpha = alpha,
                 linewidth = line_width,
                 color = line_color,
                 linetype = line_type,
                 lineend = "round")

  if(index == 2){
    g <- g +
      geom_segment(data = ests_squid,
                   alpha = 1,
                   linewidth = line_width,
                   color = line_color,
                   linetype = line_type,
                   lineend = "round")
  }

  x_breaks <- x_lim[1]:x_lim[2]
  x_labels <- x_breaks
  if(is.null(x_labs_mod)){
    x_labels[!x_labels %in% unique(ests$year)] <- ""
  }else{
    x_labels[x_labels %% x_labs_mod != 0] <- ""
  }
  if(!is.null(remove_yr_labels)){
    x_labels[x_labels %in% remove_yr_labels] <- ""
  }

  y_breaks <- seq(y_lim[1], y_lim[2], brk_interval)

  g <- g +
    geom_point(aes(y = obs),
               shape = point_shape,
               size = point_size,
               color = point_color,
               stroke = point_stroke) +
    scale_x_continuous(breaks = x_breaks,
                       labels = x_labels) +
    scale_y_continuous(breaks = y_breaks,
                       expand = c(0, 0)) +
    coord_cartesian(ylim = y_lim,
                    clip = "off") +
    ylab(y_label) +
    xlab("Year") +
    theme(axis.text.x = element_text(vjust = vjust_x_labels),
          axis.title.x = element_text(vjust = vjust_x_labels))

  # Add a major tick mark every `x_labs_mod` years
  g <- g |>
    add_major_ticks(x_breaks = x_breaks,
                    modulo = x_labs_mod,
                    prop = tick_prop,
                    ...)

  g
}
