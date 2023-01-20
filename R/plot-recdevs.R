#' Plot recruitment deviations from MCMC output for one or more models
#'
#' @param model_lst A list of models, each created by [create_rds_file()]
#' @param model_names A vector of model names,the same length as `models_lst`
#' @param xlim The year limits to plot
#' @param x_breaks The year value tick marks to show for the x axis
#' @param ylim The depletion limits to plot
#' @param y_breaks The depletion value tick marks to show for the y axis
#' @param y_labels The depletion labels to show for the y axis tick marks
#' @param y_colors The color vector for each label for the y axis tick marks
#' @param alpha The transparency for all ribbons
#' @param leg_pos The position of the legend inside the plot. If `NULL`,
#' `NA`, or `none`, the legend will not be shown
#' @param leg_ncol The number of columns to show in the legend
#' @param leg_font_size The legend font size
#' @param point_size Size of all points shownin plot
#' @param line_width Width of all lines on the plot
#' @param crossbar_width The width of the end bars (top and bottom) of the errorbar
#' lines. Default of zero removes them
#' @param dodge_val The amount to offset the lines from each other in the
#' case of multiple models
#' @param d_obj If not `NULL` this is a list which has been
#' pre-processed to contain all models in a format that is ready to plot.
#' Essentially the first steps of this function have been replicated
#' outside the function (The code inside the `if(is.null(d_obj))`)
#' is done to stop the Rmd process from taking forever
#'
#' @return a [ggplot2::ggplot()] object
#' @export
plot_recdevs <- function(model_lst = NULL,
                         model_names,
                         xlim = c(1946,
                                  year(Sys.time())),
                         x_breaks = c(1946,
                                      seq(
                                        round(1946 + 10, -1),
                                        # Current decade, i.e. 2020
                                        round(year(Sys.time()), -1),
                                        by = 10)),
                         x_expansion = 3,
                         ylim = c(-5, 5),
                         y_breaks = seq(ylim[1], ylim[2], by = 1),
                         y_labels = expression("-5", "-4", "-3", "-2", "-1",
                                               "0",
                                               "1", "2", "3", "4", "5"),
                         y_colors = c("black", "black", "black", "black", "black",
                                      "blue",
                                      "black", "black", "black", "black", "black"),
                         alpha = 0.1,
                         leg_pos = c(0.65, 0.83),
                         leg_ncol = 1,
                         leg_font_size = 12,
                         point_size = 1.5,
                         line_width = 0.5,
                         single_point_color = "black",
                         single_line_color = "black",
                         crossbar_width = 0,
                         dodge_val = 0.5,
                         rev_colors = FALSE,
                         d_obj = NULL){

  if(is.null(d_obj)){
    if(is.null(model_lst[1]) || is.null(model_names[1])){
      stop("Either `d_obj` or both `model_lst` and `model_names` ",
           "must be supplied. Both are `NULL`",
           call. = FALSE)
    }
    d_obj <- create_group_df_recr(model_lst, model_names, devs = TRUE)
  }

  d <- d_obj[[1]]
  colors <- plot_color(length(unique(d$model)))
  line_colors <- colors
  if(rev_colors){
    colors <- rev(colors)
    line_colors <- rev(line_colors)
  }
  is_single_model <- length(unique(d$model)) == 1
  if(is_single_model){
    colors <- single_point_color
    line_colors <- single_line_color
  }

  # Remove projection years
  d <- d |>
    filter(year <= xlim[2])

  g <- ggplot(d,
              aes(x = year,
                  y = devmed,
                  ymin = devlower,
                  ymax = devupper,
                  group = model,
                  color = model,
                  fill = model)) +
    scale_color_manual(values = line_colors) +
    coord_cartesian(xlim = xlim,
                    ylim = ylim) +
    geom_hline(yintercept = 0,
               color = "black",
               linetype = "solid",
               size = 0.5) +
    geom_point(size = point_size,
               position = position_dodge(dodge_val)) +
    scale_x_continuous(expand = c(0, x_expansion),
                       breaks = x_breaks,
                       labels = x_breaks) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       labels = y_labels) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = leg_font_size),
          axis.text.y = element_text(color = y_colors),
          # plot.margin: top, right,bottom, left
          # Needed to avoid tick labels cutting off
          plot.margin = margin(12, 12, 0, 0)) +
    xlab("Year") +
    ylab("Recruitment deviations")

  if(is_single_model){
    g <- g +
      geom_point(size = point_size,
                 color = colors) +
      geom_errorbar(size = line_width,
                    position = position_dodge(dodge_val),
                    alpha = 0.5,
                    width = crossbar_width)
  }else{
    g <- g +
      geom_errorbar(size = line_width,
                  position = position_dodge(dodge_val),
                  width = crossbar_width)
  }

  if(is.null(leg_pos[1]) || is.na(leg_pos[1])){
    g <- g +
      theme(legend.position = "none")
  }else{
    g <- g +
      theme(legend.position = leg_pos) +
      guides(color = guide_legend(ncol = leg_ncol))
  }

  g
}