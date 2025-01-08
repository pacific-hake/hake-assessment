#' Plot biomass from MCMC output for one or more models. Includes the
#' virgin biomass, B0
#'
#' @param model_lst A list of models, each created by [create_rds_file()]
#' @param model_names A vector of model names,the same length as `models_lst`
#' @param d_obj If not `NULL` this is a list which has been
#' pre-processed to contain all models in a format that is ready to plot.
#' Essentially the first steps of this function have been replicated
#' outside the function (The code inside the `if(is.null(d_obj))`)
#' is done to stop the Rmd process from taking forever
#' @param show_arrows Logical. If `TRUE`, show arrow that point toward
#' medians lying outside the plotting area. Also allow the lines toward those
#' median points to be plotted outside the plotting area. If `FALSE`,
#' truncate the medians that lie outside the plotting area to the maximum
#' (or minimum) y limit and draw the lines to go through those new values
#' @param xlim The year limits to plot
#' @param x_breaks The year value tick marks to show for the x axis
#' @param x_labs_mod Value for major X-axis tick marks. Every Nth tick
#' will be longer and have a label. The first and last will be shown
#' regardless of what this number is
#' @param x_expansion Amount to expand the x axis. See the `expand` argument
#' in [ggplot2::scale_x_continuous()]
#' @param tick_prop A value that the length of the major tick marks are
#' multiplied by. This proportion must be set by trial and error. Make sure
#' to change `vjust_x_labels` so the labels are not overlapping the lines or
#' are too far away from the lines
#' @param vjust_x_labels Adjustment to move the x-axis tick labels and label
#' up or down. Negative numbers move down
#' @param ylim The y-axis minimum and maximum limits for the plot
#' @param y_breaks The tick mark values to show for the y-axis
#' @param y_labels Labels for the tick marks on the y-axis
#' @param y_colors Colors for the tick labels on the y-axis
#' @param alpha The transparency for all ribbons (credible intervals)
#' @param leg_pos The position of the legend inside the plot. If `NULL`,
#' `NA`, or "none", the legend will not be shown
#' @param leg_ncol The number of columns to show in the legend
#' @param leg_font_size The legend font size
#' labels
#' @param wrap_y_label Logical. If `TRUE`, adds a newline to the y-axis
#' label so that it doesn't get cut off
#' @param dodge_bo A value to adjust the spacing between multiple B0 points
#' and errorbars in the case of more than one model being plotted
#' @param rev_colors Logical. If `TRUE`, reverse the order of the colors
#' in the plot. Only applies if more than one model is plotted
#' @param alpha The transparency for the ribbons (credible intervals)
#' @param point_size The size of the points used for the median lines (See
#' [ggplot2::geom_point()] for more details)
#' @param point_color The R color for the points used for the median lines
#' (See [ggplot2::geom_point()] for more details)
#' @param point_shape The R shape number for the points used for the median
#' lines (See [ggplot2::geom_point()] for more details)
#' @param point_stroke The stroke value for the points used for the median
#' lines (See [ggplot2::geom_point()] for more details)
#' @param line_width Width of the median line and errorbar line (for the B0
#' value) (See [ggplot2::geom_path()] for more details)
#' @param line_gap Gap between the connecting lines and points for each line.
#' See the `mult` parameter of [ggh4x::geom_pointpath()]
#' @param single_ribbon_lines_color Line color for the ribbon limit lines
#' (credible interval) for the case where there is only a single model to plot
#' @param single_ribbon_fill The ribbon fill color if there is only a single
#' model
#' @param ribbon_line_type Line type for ribbon edges; use 0 for no line.
#' @param refpt_bo_linecolor The line color for the B0 horizontal line
#' @param refpt_usr_linecolor The line color for the Upper stock reference
#' horizontal line
#' @param refpt_lrp_linecolor The line color for the Limit Reference point
#' horizontal line
#' @param refpt_bo_linewidth The line width for the B0 horizontal line
#' @param refpt_usr_linewidth The line width for the Upper stock reference
#' horizontal line
#' @param refpt_lrp_linewidth The line width for the Limit Reference point
#' horizontal line
#' @param refpt_bo_linetype The line type for the B0 horizontal line
#' @param refpt_usr_linetype The line type for the Upper stock reference
#' horizontal line
#' @param refpt_lrp_linetype The line type for the Limit Reference point
#' horizontal line
#' @param inc_means Logical. If `TRUE` include the mean values in the plot.
#' Used for recruitment plot only
#' @param survey_type Either `age1` or `age2`. Used for survey index plots
#' only
#' @param ax_title_font_size Size of the font for the X and Y axis labels
#' @param ax_tick_font_size Size of the font for the X and Y axis tick labels
#' @param ax_label_color Color of the font for the X and Y axis tick and
#' title labels
#'
#' @return a [ggplot2::ggplot()] object
#' @export
plot_biomass <- function(
    model_lst = NULL,
    model_names,
    d_obj = NULL,
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
    leg_pos = c(0.65, 0.83),
    leg_ncol = 1,
    leg_font_size = 12,
    dodge_bo = 3,
    rev_colors = TRUE,
    wrap_y_label = FALSE,
    alpha = ts_ribbon_alpha,
    point_size = ifelse(is_single_model,
                        ts_single_model_pointsize,
                        ts_pointsize),
    point_color = ts_single_model_pointcolor,
    point_shape = ifelse(is_single_model,
                         ts_single_model_pointshape,
                         ts_pointshape),
    point_stroke = ifelse(is_single_model,
                          ts_single_model_pointstroke,
                          ts_pointstroke),
    line_width = ifelse(is_single_model,
                        ts_single_model_linewidth,
                        ts_linewidth),
    line_gap = ts_linegap,
    single_ribbon_lines_color = ts_single_model_linecolor,
    single_ribbon_fill = ts_single_model_ribbon_fill,
    ribbon_line_type = ifelse(is_single_model,
                              ts_single_model_ribbon_linetype,
                              ts_ribbon_linetype),
    refpt_bo_linecolor = refpt_bo_linecolor,
    refpt_usr_linecolor = refpt_usr_linecolor,
    refpt_lrp_linecolor = refpt_lrp_linecolor,
    refpt_bo_linewidth = refpt_bo_linewidth,
    refpt_usr_linewidth = refpt_usr_linewidth,
    refpt_lrp_linewidth = refpt_lrp_linewidth,
    refpt_bo_linetype = refpt_bo_linetype,
    refpt_usr_linetype = refpt_usr_linetype,
    refpt_lrp_linetype = refpt_lrp_linetype){

  if(is.null(d_obj)){
    if(is.null(model_lst[1]) || is.null(model_names[1])){
      stop("Either `d_obj` or both `model_lst` and `model_names` ",
           "must be supplied. Neither data source has been supplied")
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
  # In case the length is 2, `plot_color()` will return 3 so we must remove one
  # for the `geom_errorbar()` function to work correctly
  length(ribbon_colors) <- length(unique(d$model))

  if(is_single_model){
    colors <- single_ribbon_lines_color
    ribbon_colors <- single_ribbon_fill
  }

  x_labels <- make_major_tick_labels(x_breaks = x_breaks,
                                     modulo = x_labs_mod)

  # Add "B0" to the x break labels with a newline between them
  x_labels <- c(expression(B[0]), x_labels)
  x_breaks = c(bo$year[1], x_breaks)

  d <- d |>
    dplyr::filter(year <= xlim[2] & year >= ylim[1])

  # Calculate the data y-axis out-of-bounds (yoob) and change the credible
  # interval in the data to cut off at the limits (or not if `show_arrows`
  # is `TRUE`)
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
    geom_ribbon(alpha = alpha,
                linetype = ribbon_line_type) +
    scale_x_continuous(expand = c(0, x_expansion),
                       breaks = x_breaks,
                       labels = x_labels) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       labels = y_breaks) +
    scale_fill_manual(values = ribbon_colors) +
    scale_color_manual(values = colors) +
    coord_cartesian(xlim = xlim,
                    ylim = ylim,
                    # `clip` must be "off" or the different length tick
                    #  marks will not work. All tick marks will be the same
                    #  length
                    clip = "off") +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = leg_font_size),
          legend.text.align = 0,
          axis.text.x = element_text(vjust = vjust_x_labels),
          axis.title.x = element_text(vjust = vjust_x_labels),
          axis.title.y = element_text(vjust = 2)) +
    labs(x = "Year",
         y = ifelse(wrap_y_label,
                    add_newlines("Female Spawning Biomass+(Mt)"),
                    "Female Spawning Biomass (Mt)"))

  # Add the median points and connecting lines.
  # Uses `ggh4x::geom_pointpath()`
  # `do.call()` used here to include the `color` argument only if
  # `is_single_model`is `TRUE`
  g <- g +
    do.call(geom_pointpath,
            c(list(linewidth = line_width,
                   size = point_size,
                   shape = point_shape,
                   stroke = point_stroke,
                   mult = line_gap),
              list(color = point_color)[is_single_model]))

  # Add B0 point and credible interval to the plot
  g <- g +
    geom_errorbar(data = yoob_bo$d,
                  linewidth = line_width,
                  position = position_dodge(dodge_bo),
                  color = ribbon_colors,
                  width = 0) +
    geom_point(data = yoob_bo$d,
               size = point_size,
               shape = point_shape,
               color = "white",
               position = position_dodge(dodge_bo)) +
    geom_point(data = yoob_bo$d,
               size = point_size,
               shape = point_shape,
               position = position_dodge(dodge_bo))

  # Add arrows to point toward the out of bounds data points
  # Note no `+` here, but the pipe instead `|>`
  g <- g |>
    draw_arrows_yoob(yoob) |>
    draw_arrows_yoob(yoob_bo)

  # Add a major tick mark every `x_labs_mod` years
  g <- g |>
    add_major_ticks(x_breaks = x_breaks,
                    modulo = x_labs_mod,
                    prop = tick_prop)

  # Add legend if requested
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
