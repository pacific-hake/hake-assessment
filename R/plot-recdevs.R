#' Plot recruitment deviations from MCMC output for one or more models
#'
#' @rdname plot_biomass
#' @param single_point_color Color to use for the points if there is only
#' one model
#' @param crossbar_width The width of the crossbar ends in
#' [ggplot2::geom_errorbar()]
#' @param dodge_val The amount to separate lines between unique models
#' multiple model plots
#'
#' @export
plot_recdevs <- function(model_lst = NULL,
                         model_names,
                         xlim = c(1946, year(Sys.time())),
                         x_breaks = xlim[1]:xlim[2],
                         x_labs_mod = 5,
                         x_expansion = 2,
                         tick_prop = 1,
                         vjust_x_labels = -2,
                         ylim = c(-5, 5),
                         y_breaks = seq(ylim[1], ylim[2], by = 1),
                         y_labels = expression("-5", "-4", "-3", "-2", "-1",
                                               "0",
                                               "1", "2", "3", "4", "5"),
                         alpha = 0.1,
                         leg_pos = c(0.65, 0.83),
                         leg_ncol = 1,
                         leg_font_size = 12,
                         axis_title_font_size = 14,
                         axis_tick_font_size = 11,
                         point_size = 1.5,
                         line_width = 0.5,
                         clip_cover = 2,
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

  x_labels <- make_major_tick_labels(x_breaks = x_breaks,
                                     modulo = x_labs_mod)

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
                    ylim = ylim,
                    clip = "off") +
    geom_hline(yintercept = 0,
               color = "black",
               linetype = "solid",
               size = 0.5) +
    geom_point(size = point_size,
               position = position_dodge(dodge_val)) +
    scale_x_continuous(expand = c(0, x_expansion),
                       breaks = x_breaks,
                       labels = x_labels) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       labels = y_labels) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = leg_font_size),
          legend.text.align = 0,
          # plot.margin: top, right,bottom, left
          # Needed to avoid tick labels cutting off
          # These two commands move the x-axis major tick labels and axis
          # title down so that the ticks. tick labels, and axis title don't
          # overlap each other
          axis.text.x = element_text(vjust = vjust_x_labels),
          axis.title.x = element_text(vjust = vjust_x_labels),
          plot.margin = margin(12, 12, 7, 0)) +
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
      guides(color = guide_legend(ncol = leg_ncol))
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