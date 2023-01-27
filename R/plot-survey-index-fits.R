#' Plot survey index fits from MCMC output for one or more models,
#' for one of the acoustic surveys
#'
#' @rdname plot_biomass
#' @export
plot_survey_index_fits <- function(
    model_lst = NULL,
    model_names,
    survey_type = c("age1",
                    "age2"),
    xlim = c(1995, 2021),
    x_breaks = xlim[1]:xlim[2],
    x_labs_mod = 5,
    x_expansion = 3,
    ylim = c(0, 3),
    y_breaks = seq(ylim[1], ylim[2], by = 0.5),
    alpha = 0.1,
    leg_pos = c(0.65, 0.83),
    leg_ncol = 1,
    leg_font_size = 12,
    axis_title_font_size = 14,
    axis_tick_font_size = 11,
    point_size = 1.5,
    line_width = 0.5,
    clip_cover = 2,
    rev_colors = FALSE,
    dodge_val = 0.5,
    d_obj = NULL){

  survey_type <- match.arg(survey_type)
  fleet <- ifelse(survey_type == "age2", 2, 3)

  if(is.null(d_obj)){
    if(is.null(model_lst[1] || is.null(model_names[1]))){
      stop("Either `d_obj` or both `model_lst` and `model_names` ",
           "must be supplied. Both are `NULL`",
           call. = FALSE)
    }
    d_obj <- create_group_df_index(model_lst, model_names, survey_type)
  }

  d <- d_obj[[1]]
  colors <- plot_color(length(unique(d$model)) - 1)
  num_models <- length(unique(d$model)) - 1 # Minus 1 for observed
  linetypes <- c(rep("solid", num_models), "dashed")
  shapes <- c(rep(16, num_models), 17)
  if(rev_colors){
    colors <- rev(colors)
  }
  # Add observed
  colors <- c(colors, "black")


  # Remove labels for the minor x-axis ticks
  x_labels <- NULL
  for(i in x_breaks){
    if(i %% x_labs_mod == 0){
      x_labels <- c(x_labels, i)
    }else{
      x_labels <- c(x_labels, "")
    }
  }
  # Major tick mark lengths adjusted here
  x_breaks_nth <- x_breaks[x_breaks %% x_labs_mod == 0]
  top_y_pos = ylim[1]
  bot_y_pos = ylim[1] - (ylim[2] - ylim[1]) / 25
  custom_ticks <- tibble(group = x_breaks_nth,
                         y_end = bot_y_pos)

  y_title <- ifelse(survey_type == "age1",
                    "Numbers (billions)",
                    "Biomass (million t)")
  g <- ggplot(d,
              aes(x = year,
                  y = index.med,
                  ymin = index.025,
                  ymax = index.975,
                  group = model,
                  color = model,
                  linetype = model,
                  shape = model)) +
    scale_color_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    scale_linetype_manual(values = linetypes) +
    coord_cartesian(xlim = xlim,
                    ylim = ylim,
                    clip = "off") +
    geom_point(size = point_size,
               position = position_dodge(dodge_val)) +
    geom_line(size = line_width,
              position = position_dodge(dodge_val)) +
    geom_errorbar(size = line_width,
                  position = position_dodge(dodge_val)) +
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
          plot.margin = margin(12, 12, 7, 0)) +
    xlab("Year") +
    ylab(y_title)

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