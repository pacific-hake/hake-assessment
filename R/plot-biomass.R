#' Plot biomass from MCMC output for one or more models
#'
#' @param model_lst A list of models, each created by [create_rds_file()]
#' @param model_names A vector of model names,the same length as `models_lst`
#' @param show_arrows Logical. If `TRUE`, show arrow that point toward
#' medians lying outside the plotting area. Also allow the lines toward those
#' median points to be plotted outside the plotting area. If `FALSE`, truncate
#' the medians that lie outside the plotting area to the maximum (or minimum)
#' y limit and draw the lines to go through those new values
#' @param xlim The year limits to plot
#' @param x_breaks The year value tick marks to show for the x axis
#' @param x_labs_mod Value for major X-axis tick marks. Every Nth tick
#' will be longer and have a label. The first and last will be shown
#' regardless of what this number is
#' @param tick_prop A value that the length of the major tick marks are
#' multiplied by
#' @param vjust_x_labels A value to move the x-axis tick labels and title
#' by. Negative means move it down, away from the plot, positive is to move
#' it up, closer to the plot
#' @param ylim The depletion limits to plot
#' @param y_breaks The depletion value tick marks to show for the y axis
#' @param y_labels Labels for the tick marks on the Y axis
#' @param y_colors Colors for the tick labels on the Y axis
#' @param alpha The transparency for all ribbons
#' @param leg_pos The position of the legend inside the plot. If `NULL`,
#' `NA`, or `none`, the legend will not be shown
#' @param leg_ncol The number of columns to show in the legend
#' @param leg_font_size The legend font size
#' @param axis_title_font_size Size of the font for the X and Y axis labels
#' @param axis_tick_font_size Size of the font for the X and Y axis tick labels
#' @param axis_label_color Color for the axis labels and tick labels
#' @param point_size Size of all points shown in plot
#' @param line_width Width of all lines on the plot
#' @param single_line_color Line color for the case where there is only
#' one model to plot
#' @param wrap_y_label Logical. If `TRUE`, adds a newline to the y axis
#' label so that it doesn't get cut off
#' @param rev_colors Logical. If `TRUE`, reverse the order of the colors
#' in the plot. Only applies if more than one model is plotted
#' @param d_obj If not `NULL` this is a list which has been
#' pre-processed to contain all models in a format that is ready to plot.
#' Essentially the first steps of this function have been replicated
#' outside the function (The code inside the `if(is.null(d_obj))`)
#' is done to stop the Rmd process from taking forever
#' @param ribbon_line_type Linetype for ribbon edges; use 0 for no line.
#' @param x_expansion Amount to expand the x axis. See the `expand` argument in
#' [ggplot2::scale_x_continuous()]
#' @param point_shape The R shape number for the points
#' @param single_ribbon_color The ribbon color if there is only a single model
#' @param inc_means Logical. If `TRUE` include the mean values in the plot
#' @param minor_tick_length The length of the small x-axis ticks
#' @param survey_type Either `age1` or `age2`
#' @param colors The colors to use for the plot
#'
#' @return a [ggplot2::ggplot()] object
#' @export
plot_biomass <- function(model_lst = NULL,
                         model_names,
                         show_arrows = TRUE,
                         xlim = c(1964,
                                  year(Sys.time())),
                         x_breaks = 1966:year(Sys.time()),
                         x_labs_mod = 5,
                         x_expansion = 3,
                         tick_prop = 1,
                         vjust_x_labels = -0.25,
                         ylim = c(0, 4.5),
                         y_breaks = seq(ylim[1], ylim[2], by = 0.5),
                         alpha = hake::ts_ribbon_alpha,
                         leg_pos = c(0.65, 0.83),
                         leg_ncol = 1,
                         leg_font_size = 12,
                         point_size = hake::ts_pointsize,
                         point_shape = hake::ts_pointshape,
                         line_width = hake::ts_linewidth,
                         single_line_color = hake::ts_single_model_linecolor,
                         single_ribbon_color = hake::ts_ribbon_fill,
                         ribbon_line_type = hake::ts_ribbon_linetype,
                         rev_colors = TRUE,
                         wrap_y_label = FALSE,
                         d_obj = NULL){

  if(is.null(d_obj)){
    if(is.null(model_lst[1]) || is.null(model_names[1])){
      stop("Either `d_obj` or both `model_lst` and `model_names` ",
           "must be supplied. Both are `NULL`",
           call. = FALSE)
    }
    d_obj <- create_group_df_biomass(model_lst, model_names)
  }

  d <- d_obj[[1]]
  bo <- d_obj[[2]]
  is_single_model <- length(unique(d$model)) == 1

  colors <- plot_color(length(unique(d$model)))
  ribbon_colors <- colors
  if(rev_colors){
    colors <- rev(colors)
    ribbon_colors <- rev(ribbon_colors)
  }
  if(is_single_model){
    colors <- single_line_color
    ribbon_colors <- single_ribbon_color
  }

  x_labels <- make_major_tick_labels(x_breaks = x_breaks,
                                     modulo = x_labs_mod)

  # Add "Unfished Equilibrium" to the x break labels
  # with a newline between them
  x_labels <- c(expression(B[0]), x_labels)
  x_breaks = c(bo$year[1], x_breaks)

  d <- d |>
    filter(year <= xlim[2] & year >= ylim[1])

  # Calculate the data outside the range of the y limits and
  # change the CI in the data to cut off at the limits
  yoob_bo <- calc_yoob(bo, ylim, "slower", "smed", "supper", show_arrows)
  yoob <- calc_yoob(d, ylim, "slower", "smed", "supper", show_arrows)

  g <- ggplot(yoob$d,
              aes(x = year,
                  y = smed,
                  ymin = slower,
                  ymax = supper,
                  group = model,
                  color = model,
                  fill = model)) +
    scale_fill_manual(values = ribbon_colors) +
    scale_color_manual(values = colors) +
    coord_cartesian(xlim = xlim,
                    ylim = ylim,
                    clip = "off") +
    geom_ribbon(alpha = alpha,
                linetype = ribbon_line_type) +
    geom_line(linewidth = line_width) +
    geom_point(size = point_size,
               shape = point_shape) +
    scale_x_continuous(expand = c(0, x_expansion),
                       breaks = x_breaks,
                       labels = x_labels) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       labels = y_breaks) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = leg_font_size),
          legend.text.align = 0,
          # These two commands move the x-axis major tick labels and axis
          # title down so that the ticks. tick labels, and axis title don't
          # overlap each other
          axis.text.x = element_text(vjust = vjust_x_labels),
          axis.title.x = element_text(vjust = vjust_x_labels)) +
    labs(x = "Year",
         y = ifelse(wrap_y_label,
                    add_newlines("Female Spawning Biomass+(Mt)"),
                    "Female Spawning Biomass (Mt)"))

  # Add B0 to the plot
  g <- g +
    geom_point(data = yoob_bo$d,
               size = point_size,
               shape = point_shape,
               position = position_dodge(1.5)) +
    geom_errorbar(data = bo,
                  linewidth = line_width,
                  position = position_dodge(1.5),
                  color = ribbon_colors,
                  alpha = alpha)

  # Add arrows to the plot to point toward the out of bounds data points
  g <- g |>
    draw_arrows_yoob(yoob) |>
    draw_arrows_yoob(yoob_bo)

  # Add major tick marks
  g <- g |>
    add_major_ticks(x_breaks = x_breaks,
                    modulo = x_labs_mod,
                    # This proportion must be set by trial and error
                    # Make sure to change `vjust` value above in the `theme()`
                    # call so the labels are not overlapping the lines or
                    # too far away from the lines
                    prop = tick_prop)

  if(is.null(leg_pos[1]) || is.na(leg_pos[1])){
    g <- g +
      theme(legend.position = "none")
  }else{
    g <- g +
      theme(legend.position = leg_pos) +
      guides(fill = guide_legend(ncol = leg_ncol),
             color = guide_legend(ncol = leg_ncol))
  }

  g
}
