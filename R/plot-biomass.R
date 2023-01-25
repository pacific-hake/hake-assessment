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
#' @param alpha The transparency for all ribbons
#' @param leg_pos The position of the legend inside the plot. If `NULL`,
#' `NA`, or `none`, the legend will not be shown
#' @param leg_ncol The number of columns to show in the legend
#' @param leg_font_size The legend font size
#' @param axis_title_font_size Size of the font for the X and Y axis labels
#' @param axis_tick_font_size Size of the font for the X and Y axis tick labels
#' @param point_size Size of all points shownin plot
#' @param line_width Width of all lines on the plot
#' @param wrap_y_label Logical. If `TRUE`, adds a newline to the y axis
#' label so that it doesn't get cut off
#' @param d_obj If not `NULL` this is a list which has been
#' pre-processed to contain all models in a format that is ready to plot.
#' Essentially the first steps of this function have been replicated
#' outside the function (The code inside the `if(is.null(d_obj))`)
#' is done to stop the Rmd process from taking forever
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
                         single_line_color = "black",
                         single_ribbon_color = "blue",
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
  x_labels <- c(expression(atop("Unfished", "Equilibrium")), x_labels)
  x_breaks = c(bo$year[1], x_breaks)
  # Tick mark lengths adjusted here
  x_breaks_nth <- x_breaks[x_breaks %% x_labs_mod == 0]
  top_y_pos = 0
  bot_y_pos = - (ylim[2] - ylim[1]) / 50
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
                linetype = "dotted") +
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
          # plot.margin: top, right,bottom, left
          # Needed to avoid tick labels cutting off
          plot.margin = margin(12, 12, 0, 0)) +
    labs(x = "Year",
         y = ifelse(wrap_y_label,
                    add_newlines("Female Spawning Biomass+(million t)"),
                    "Female Spawning Biomass (million t)"))

  # Add major tick marks
  g <- g +
    geom_linerange(data = custom_ticks,
                   aes(x = group,
                       ymax = 0,
                       ymin = y_end),
                   size = 0.5,
                   inherit.aes = FALSE)

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

  g <- g +
    theme(axis.text.x = element_text(color = "grey20",
                                     size = axis_tick_font_size,
                                     angle = 0,
                                     hjust = 0.5,
                                     vjust = 0.5,
                                     face = "plain"),
          axis.text.y = element_text(color = "grey20",
                                     size = axis_tick_font_size,
                                     hjust = 1,
                                     vjust = 0.5,
                                     face = "plain"),
          axis.title.x = element_text(color = "grey20",
                                      size = axis_title_font_size,
                                      angle = 0,
                                      face = "plain"),
          axis.title.y = element_text(color = "grey20",
                                      size = axis_title_font_size,
                                      angle = 90,
                                      face = "plain"))

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
                      ymax = ylim[2] + 2)

  g
}
