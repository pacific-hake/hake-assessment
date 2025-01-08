#' Plot recruitment from MCMC output for one or more models, relative to
#' a particular year
#'
#' @rdname plot_biomass
#' @export
plot_rel_recruitment <-  function(model_lst = NULL,
                                  model_names,
                                  rel_yr = 2010,
                                  inc_means = FALSE,
                                  xlim = c(1966, year(Sys.time())),
                                  x_breaks = xlim[1]:xlim[2],
                                  x_labs_mod = 5,
                                  x_expansion = 3,
                                  ylim = c(0, 1.2),
                                  y_breaks = seq(ylim[1], ylim[2], by = 0.1),
                                  y_labels = y_breaks,
                                  y_colors = rep("black", length(y_breaks)),
                                  alpha = 0.2,
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
           "must be supplied. Both are `NULL`")
    }
    d_obj <- create_group_df_recr(model_lst, model_names)
  }

  d <- d_obj[[1]]
  colors <- plot_color(length(unique(d$model)))
  line_colors <- colors
  if(rev_colors){
    colors <- rev(colors)
    line_colors <- rev(line_colors)
  }

  rel_row <- d |>
    dplyr::filter(year == rel_yr)

  # Make all rows relative to the chosen year (`rel_yr`)
  d <- d |>
    pmap(~{
      cols <- list(...)
      cols$rlower <- cols$rlower / rel_row$rlower
      cols$rmed <- cols$rmed / rel_row$rmed
      cols$rupper <- cols$rupper / rel_row$rupper
      cols$rmean <- cols$rmean / rel_row$rmean
      vec2df(c(as.character(cols$model),
               cols$year,
               cols$rlower,
               cols$rmed,
               cols$rupper,
               cols$rmean),
             nms = names(cols))
    }) |>
    map_df(~{.x}) |>
    mutate(across(-model, as.numeric)) |>
    mutate(model = factor(model))

  is_single_model <- length(unique(d$model)) == 1
  if(is_single_model){
    colors <- single_point_color
    line_colors <- single_line_color
    ro_vec <- model_lst[[1]]$mcmccalcs$rinit
    yrs <- c(seq(min(d$year) - x_expansion,
                 min(d$year) - 1), d$year)
    ro <- tibble(model = model_names[[1]],
                 year = yrs,
                 rlower = ro_vec[1],
                 rmed = ro_vec[2],
                 rupper = ro_vec[3]) |>
      mutate(model = factor(model))
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

  # Tick mark lengths adjusted here
  x_breaks_nth <- x_breaks[x_breaks %% x_labs_mod == 0]
  top_y_pos = ylim[1]
  bot_y_pos = ylim[1] - (ylim[2] - ylim[1]) / 25
  custom_ticks <- tibble(group = x_breaks_nth,
                         y_end = bot_y_pos)

  # Remove projection years
  d <- d |>
    dplyr::filter(year <= xlim[2])

  g <- ggplot(d,
              aes(x = year,
                  y = rmed,
                  ymin = rlower,
                  ymax = rupper,
                  group = model,
                  color = model,
                  fill = model)) +
    scale_color_manual(values = line_colors) +
    coord_cartesian(xlim = xlim,
                    ylim = ylim,
                    clip = "off")

  if(is_single_model){
    ro_val <- ro$rmed[1]
    below <- which(ro_val > y_breaks)
    above <- which(ro_val < y_breaks)
    tmp <- y_breaks
    y_breaks <- c(tmp[below], ro_val, tmp[above])
    y_labels <- c(tmp[below], expression(R[0]), tmp[above])

    # Expand right side of ro so that ribbon covers whole plot
    ro_row <- ro[nrow(ro),]
    ro_next_yr <- ro_row$year
    for(yr in ro_next_yr:(ro_next_yr + x_expansion)){
      ro_row$year <- yr
      ro <- bind_rows(ro, ro_row)
    }

    g <- g +
      geom_point(size = point_size,
                 color = colors) +
      geom_hline(data = ro,
                 aes(yintercept = rmed),
                 linetype = "dashed") +
      geom_ribbon(data = ro,
                  aes(ymin = rlower,
                      ymax = rupper),
                  alpha = alpha,
                  linetype = "dotted") +
      geom_errorbar(size = line_width,
                    position = position_dodge(dodge_val),
                    width = crossbar_width,
                    alpha = 0.5,
                    color = line_colors) +
      geom_point(size = point_size,
                 position = position_dodge(dodge_val),
                 color = colors)
  }else{
    g <- g +
      geom_errorbar(size = line_width,
                    position = position_dodge(dodge_val),
                    width = crossbar_width) +
      geom_point(size = point_size,
                 position = position_dodge(dodge_val))
  }

  g <- g +
    geom_hline(yintercept = 0,
               color = "black",
               linetype = "solid",
               size = 0.5) +
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
    xlab("Year") +
    ylab("Age-0 recruits (billions)")

  if(inc_means){
    if(is_single_model){
      g <- g +
        geom_point(aes(y = rmean),
                   shape = 4,
                   color = colors)
    }else{
      g <- g +
        geom_point(aes(y = rmean),
                   shape = 4)
    }
  }

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
                                      vjust = -2,
                                      angle = 0,
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