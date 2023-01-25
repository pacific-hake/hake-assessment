#' Plot relative biomass from MCMC output for one or more models
#'
#' @rdname plot_biomass
#' @export
plot_rel_biomass <- function(model_lst = NULL,
                             model_names,
                             xlim = c(1966, year(Sys.time())),
                             x_breaks = xlim[1]:xlim[2],
                             x_labs_mod = 5,
                             x_expansion = 3,
                             ylim = c(0, 3.5),
                             alpha = 0.1,
                             leg_pos = c(0.65, 0.83),
                             leg_ncol = 1,
                             leg_font_size = 12,
                             axis_title_font_size = 14,
                             axis_tick_font_size = 11,
                             minor_tick_length = 0.1, # centimeters
                             point_size = 2,
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
    d_obj <- create_group_df_biomass(model_lst, model_names, rel = TRUE)
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
  # X-axis tick mark lengths adjusted here
  x_breaks_nth <- x_breaks[x_breaks %% x_labs_mod == 0]
  top_y_pos = ylim[1]
  bot_y_pos = ylim[1] - (ylim[2] - ylim[1]) / 25
  custom_ticks <- tibble(group = x_breaks_nth,
                         y_end = bot_y_pos)

  y_breaks <- c(0, 0.1, 0.4, 0.5, 1.0)
  y_labels <- expression("0", "0.1B"[0], "0.4B"[0], "0.5", "B"[0])
  y_colors <- c("black", "red", "green", "black", "blue")
  if(ylim[2] < 1){
    stop("Relative spawning biomass plot y-axis max must be 1 or greater",
         call. = FALSE)
  }
  if(ylim[2] > 1){
    seq_above_1 <- seq(1.5, ylim[2], by = 0.5)
    y_breaks <- c(y_breaks, seq_above_1)
    y_labels <- c(y_labels, parse(text = as.character(seq_above_1)))
    y_colors <- c(y_colors, rep("black", length(seq_above_1)))
  }

  d <- d_obj[[1]]
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
                  y = dmed,
                  ymin = dlower,
                  ymax = dupper,
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
    geom_hline(yintercept = 0.1,
               linetype = "dashed",
               color = "red",
               linewidth = 0.5) +
    geom_hline(yintercept = 0.4,
               linetype = "dashed",
               color = "green",
               linewidth = 0.5) +
    geom_hline(yintercept = 1,
               linetype = "dashed",
               color = "blue",
               linewidth = 0.52) +
    scale_x_continuous(expand = c(0, x_expansion),
                       breaks = x_breaks,
                       labels = x_labels) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       labels = y_labels) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = leg_font_size),
          axis.text.y = element_text(color = y_colors),
          # plot.margin: top, right,bottom, left
          # Needed to avoid tick labels cutting off
          plot.margin = margin(12, 12, 7, 0)) +
    labs(x = "Year",
         y = ifelse(wrap_y_label,
                    expression(atop("Relative Spawning Biomass",
                                    paste("("~B[t]/B[0]~")"))),
                    expression(paste("Relative Spawning Biomass ("~B[t]/B[0]~")"))))

  # Add major tick marks
  g <- g +
    geom_linerange(data = custom_ticks,
                   aes(x = group,
                       ymax = top_y_pos,
                       ymin = y_end),
                   size = 0.5,
                   inherit.aes = FALSE)

  g <- g +
    theme(axis.text.x = element_text(color = "grey20",
                                     size = axis_tick_font_size,
                                     angle = 0,
                                     hjust = 0.5,
                                     vjust = -3,
                                     face = "plain"),
          axis.text.y = element_text(color = "grey20",
                                     size = axis_tick_font_size,
                                     hjust = 1,
                                     vjust = 0.5,
                                     face = "plain"),
          axis.title.x = element_text(color = "grey20",
                                      size = axis_title_font_size,
                                      angle = 0,
                                      vjust = -2,
                                      face = "plain"),
          axis.title.y = element_text(color = "grey20",
                                      size = axis_title_font_size,
                                      angle = 90,
                                      face = "plain"),
          axis.ticks.length = unit(0.15, "cm"))

  if(is.null(leg_pos) || is.na(leg_pos)){
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