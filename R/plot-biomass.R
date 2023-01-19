#' Plot biomass from MCMC output for one or more models
#'
#' @param model_lst A list of models, each created by [create_rds_file()]
#' @param model_names A vector of model names,the same length as `models_lst`
#' @param xlim The year limits to plot
#' @param x_breaks The year value tick marks to show for the x axis
#' @param ylim The depletion limits to plot
#' @param y_breaks The depletion value tick marks to show for the y axis
#' @param alpha The transparency for all ribbons
#' @param leg_pos The position of the legend inside the plot. If `NULL`,
#' `NA`, or `none`, the legend will not be shown
#' @param leg_ncol The number of columns to show in the legend
#' @param leg_font_size The legend font size
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
                         x_breaks = c(1966,
                                      seq(1970,
                                          # Current decade, i.e. 2020
                                          round(year(Sys.time()), -1),
                                          by = 5)),
                                      #year(Sys.time())),
                         x_expansion = 3,
                         ylim = c(0, 4.5),
                         y_breaks = seq(ylim[1], ylim[2], by = 0.5),
                         alpha = 0.1,
                         leg_pos = c(0.65, 0.83),
                         leg_ncol = 1,
                         leg_font_size = 12,
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
                    ylim = ylim) +
    geom_ribbon(alpha = alpha,
                linetype = "dotted") +
    geom_line(linewidth = line_width) +
    geom_point(size = point_size,
               shape = point_shape) +
    scale_x_continuous(expand = c(0, x_expansion),
                       breaks = x_breaks,
                       labels = x_breaks) +
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

  # Add B0 to the plot
  g <- g +
    geom_point(data = bo,
               size = point_size,
               position = position_dodge(1.5)) +
    geom_errorbar(data = bo,
                  linewidth = line_width,
                  position = position_dodge(1.5))

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
