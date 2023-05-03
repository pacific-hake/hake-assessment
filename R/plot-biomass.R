#' Plot biomass from MCMC output for one or more models
#'
#' @param model_lst A list of models, each created by [create_rds_file()]
#' @param model_names A vector of model names,the same length as `models_lst`
#' @param xlim The year limits to plot
#' @param x_breaks The year value tick marks to show for the x axis
#' @param x_labs_mod Value for major X-axis tick marks. Every Nth tick
#' will be longer and have a label. The first and last will be shown
#' regardless of what this number is
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
#' @param clip_cover There is a white rectangle drawn on top of the plot
#' to cover any of the plot that made it outside the plot area. `clip` has to
#' be set to `off` for the major x-axis tick marks to work, So, this is required.
#' If you make the plot in a grid, the rectangle may overwrite some of the plot
#' above it, and this number will have to be changed through trial and error
#' until you cannot see the white rectangle anymore.
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
                         xlim = c(1964,
                                  year(Sys.time())),
                         x_breaks = 1966:year(Sys.time()),
                         x_labs_mod = 5,
                         x_expansion = 3,
                         ylim = c(0, 4.5),
                         y_breaks = seq(ylim[1], ylim[2], by = 0.5),
                         alpha = 0.1,
                         leg_pos = c(0.65, 0.83),
                         leg_ncol = 1,
                         leg_font_size = 12,
                         axis_title_font_size = 14,
                         axis_tick_font_size = 11,
                         point_size = 2.5,
                         point_shape = 16,
                         line_width = 1,
                         clip_cover = 2,
                         single_line_color = "black",
                         single_ribbon_color = "blue",
                         ribbon_line_type = "dotted",
                         rev_colors = FALSE,
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

  # Remove labels for the minor x-axis ticks
  x_labels <- NULL
  for(i in x_breaks){
    if(i %% x_labs_mod == 0){
      x_labels <- c(x_labels, i)
    }else{
      x_labels <- c(x_labels, "")
    }
  }

  # Add "Unfished Equilibrium" to the x break labels
  # with a newline between them
  x_labels <- c(expression(B[0]), x_labels)
  x_breaks = c(bo$year[1], x_breaks)
  # Major tick mark lengths adjusted here
  x_breaks_nth <- x_breaks[x_breaks %% x_labs_mod == 0]
  top_y_pos = ylim[1]
  bot_y_pos = ylim[1] - (ylim[2] - ylim[1]) / 25
  custom_ticks <- tibble(group = x_breaks_nth,
                         y_end = bot_y_pos)

  # Remove projection years
  d <- d |>
    filter(year <= xlim[2])

  g <- ggplot(d,
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
          # plot.margin: top, right,bottom, left
          # Needed to avoid tick labels cutting off
          plot.margin = margin(12, 12, 0, 0)) +
    labs(x = "Year",
         y = ifelse(wrap_y_label,
                    add_newlines("Female Spawning Biomass+(Mt)"),
                    "Female Spawning Biomass (Mt)"))

  # Add B0 to the plot
  g <- g +
    geom_point(data = bo,
               size = point_size,
               shape = point_shape,
               position = position_dodge(1.5)) +
    geom_errorbar(data = bo,
                  linewidth = line_width,
                  position = position_dodge(1.5),
                  color = ribbon_colors,
                  alpha = 0.5)

  # Add major tick marks
  g <- g +
    geom_linerange(data = custom_ticks,
                   aes(x = group,
                       ymax = top_y_pos,
                       ymin = y_end),
                   size = 0.5,
                   inherit.aes = FALSE)

  g <- g +
    theme(axis.text.x = element_text(color = axis_label_color,
                                     size = axis_tick_font_size,
                                     angle = 0,
                                     hjust = 0.5,
                                     vjust = -0.25,
                                     face = "plain"),
          axis.text.y = element_text(color = axis_label_color,
                                     size = axis_tick_font_size,
                                     hjust = 1,
                                     vjust = 0.5,
                                     face = "plain"),
          axis.title.x = element_text(color = axis_label_color,
                                      size = axis_title_font_size,
                                      angle = 0,
                                      vjust = 0,
                                      face = "plain"),
          axis.title.y = element_text(color = axis_label_color,
                                      size = axis_title_font_size,
                                      angle = 90,
                                      face = "plain"),
          axis.ticks.length = unit(0.15, "cm"))

  if(is.null(leg_pos[1]) || is.na(leg_pos[1])){
    g <- g +
      theme(legend.position = "none")
  }else{
    g <- g +
      theme(legend.position = leg_pos) +
      guides(fill = guide_legend(ncol = leg_ncol),
             color = guide_legend(ncol = leg_ncol))
  }

  # Draw a white rectangle over the top of the plot, obscuring any
  # unclipped plot parts. Clipping has to be off to allow different size
  # tick marks. `grid` package used here
  g <- g +
    annotation_custom(grob = rectGrob(gp = gpar(col = NA, fill = "white")),
                      xmin = xlim[1],
                      xmax = xlim[2],
                      ymin = ylim[2],
                      ymax = ylim[2] + clip_cover)

  g
}
