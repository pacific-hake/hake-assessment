#' Plot relative biomass from MCMC output for one or more models
#'
#' @rdname plot_biomass
#'
#' @export
plot_rel_biomass <- function(
    model_lst = NULL,
    model_names = NULL,
    d_obj = NULL,
    show_arrows = TRUE,
    xlim = c(1966, year(Sys.time())),
    x_breaks = xlim[1]:xlim[2],
    x_labs_mod = 5,
    x_expansion = 3,
    tick_prop = 1,
    vjust_x_labels = -2,
    hjust_y_labels = 0,
    ylim = c(0, 3.5),
    y_breaks = seq(ylim[1], ylim[2], by = 0.5),
    leg_pos = c(0.65, 0.83),
    leg_ncol = 1,
    leg_font_size = 12,
    rev_colors = TRUE,
    wrap_y_label = FALSE,
    alpha = ts_ribbon_alpha,
    point_color = ts_single_model_pointcolor,
    point_size = ifelse(is_single_model,
                        ts_single_model_pointsize,
                        ts_pointsize),
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
    refpt_bo_linecolor = ts_refpt_bo_linecolor,
    refpt_usr_linecolor = ts_refpt_usr_linecolor,
    refpt_lrp_linecolor = ts_refpt_lrp_linecolor,
    refpt_bo_linewidth = ts_refpt_bo_linewidth,
    refpt_usr_linewidth = ts_refpt_usr_linewidth,
    refpt_lrp_linewidth = ts_refpt_lrp_linewidth,
    refpt_bo_linetype = ts_refpt_bo_linetype,
    refpt_usr_linetype = ts_refpt_usr_linetype,
    refpt_lrp_linetype = ts_refpt_lrp_linetype,
    ...){

  if(is.null(d_obj)){
    if(is.null(model_lst[1]) || is.null(model_names[1])){
      stop("Either `d_obj` or both `model_lst` and `model_names` ",
           "must be supplied. Neither data source has been supplied")
    }
    d_obj <- create_group_df_biomass(model_lst,
                                     model_names,
                                     rel = TRUE,
                                     ...)
  }

  x_labels <- make_major_tick_labels(x_breaks = x_breaks,
                                     modulo = x_labs_mod)

  # Set up the y breaks, labels, and colors
  y_breaks_input <- y_breaks
  y_breaks_refpts <- c(0.1, 0.4, 1.0)
  y_breaks <- sort(unique((c(y_breaks_input, y_breaks_refpts))))
  wch_refpts <- map_dbl(y_breaks_refpts, ~{which(.x == y_breaks)})
  y_labels <- y_breaks
  y_labels[wch_refpts] <- expression("0.1B"[0], "0.4B"[0], "B"[0])

  # `axis_label_color` is a package data variable
  y_colors <- rep(axis_label_color, length(y_breaks))
  y_colors[wch_refpts] <- c(refpt_lrp_linecolor,
                            refpt_usr_linecolor,
                            refpt_bo_linecolor)

  d <- d_obj[[1]] |>
    dplyr::filter(year <= xlim[2] & year >= xlim[1])

  is_single_model <- length(unique(d$model)) == 1
  colors <- plot_color(length(unique(d$model)))
  ribbon_colors <- colors
  if(rev_colors){
    colors <- rev(colors)
    ribbon_colors <- rev(ribbon_colors)
  }
  if(is_single_model){
    colors <- single_ribbon_lines_color
    ribbon_colors <- single_ribbon_fill
  }

  # Calculate the data outside the range of the y limits and
  # change the CI in the data to cut off at the limits
  yoob <- calc_yoob(d, ylim, "dlower", "dmed", "dupper", show_arrows)

  g <- ggplot(yoob$d,
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
                    # `clip` must be "off" or the different length tick
                    #  marks will not work. All tick marks will be the same
                    #  length
                    clip = "off") +
    geom_ribbon(alpha = alpha,
                linetype = ribbon_line_type)

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

  # Add reference point lines
  g <- g +
    geom_hline(yintercept = 0.1,
               linetype = refpt_lrp_linetype,
               color = refpt_lrp_linecolor,
               linewidth = refpt_lrp_linewidth) +
    geom_hline(yintercept = 0.4,
               linetype = refpt_usr_linetype,
               color = refpt_usr_linecolor,
               linewidth = refpt_usr_linewidth) +
    geom_hline(yintercept = 1,
               linetype = refpt_bo_linetype,
               color = refpt_bo_linecolor,
               linewidth = refpt_bo_linewidth) +
    scale_x_continuous(expand = c(0, x_expansion),
                       breaks = x_breaks,
                       labels = x_labels) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       labels = y_labels) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = leg_font_size),
          legend.text.align = 0,
          # These following two arguments move the x-axis major tick labels
          # and axis title down so that the ticks, tick labels, and axis
          # title don't overlap each other
          axis.text.x = element_text(vjust = vjust_x_labels),
          axis.title.x = element_text(vjust = vjust_x_labels),
          axis.text.y = element_text(color = y_colors,
                                     hjust = hjust_y_labels)) +
    labs(x = "Year",
         y = ifelse(wrap_y_label,
                    expression(atop("Rel. Spawning Biomass",
                                    paste("("~B[t]/B[0]~")"))),
                    expression(paste("Rel. Spawning Biomass ("~B[t]/B[0]~")"))))

  # Add arrows to point toward the out of bounds data points
  # Note no `+` here, but the pipe instead `|>`
  g <- g |>
    draw_arrows_yoob(yoob)

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
