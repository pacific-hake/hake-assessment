#' Plot recruitment deviations from MCMC output for one or more models
#'
#' @rdname plot_biomass
#' @param dodge_val The amount to separate lines between unique models
#' multiple model plots
#' @param ... Arguments passed to [ggplot2::geom_pointrange()]
#'
#' @export
plot_recdevs <- function(
    model_lst = NULL,
    model_names = NULL,
    d_obj = NULL,
    show_arrows = TRUE,
    xlim = c(1946, year(Sys.time())),
    x_breaks = xlim[1]:xlim[2],
    x_labs_mod = 5,
    x_expansion = 2,
    tick_prop = 1,
    vjust_x_labels = -1,
    ylim = c(-5, 5),
    y_breaks = seq(ylim[1], ylim[2], by = 1),
    leg_pos = c(0.65, 0.83),
    leg_ncol = 1,
    leg_font_size = 12,
    alpha = 1,
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
    line_type = ts_single_model_linetype,
    line_color = ts_single_model_linecolor,
    dodge_val = 1,
    rev_colors = FALSE,
    ...){

  if(is.null(d_obj)){
    if(is.null(model_lst[1]) || is.null(model_names[1])){
      stop("Either `d_obj` or both `model_lst` and `model_names` ",
           "must be supplied. Both are `NULL`")
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
    colors <- point_color
    line_colors <- line_color
  }

  x_labels <- make_major_tick_labels(x_breaks = x_breaks,
                                     modulo = x_labs_mod)

  d <- d |>
    dplyr::filter(year <= xlim[2] & year >= xlim[1])

  # Calculate the data outside the range of the y limits and
  # change the CI in the data to cut off at the limits
  yoob <- calc_yoob(d,
                    ylim,
                    "devlower",
                    "devmed",
                    "devupper",
                    show_arrows = show_arrows)

  g <- ggplot(yoob$d,
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
    scale_x_continuous(expand = c(0, x_expansion),
                       breaks = x_breaks,
                       labels = x_labels) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       labels = y_breaks) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = leg_font_size),
          legend.text.align = 0) +
    xlab("Year") +
    ylab("Recruitment deviations")

  # Add the points and error bars
  if(is_single_model){
    g <- g +
      geom_pointrange(size = point_size / .pt,
                      shape = point_shape,
                      color = point_color,
                      linewidth = line_width,
                      linetype = line_type,
                      alpha = alpha,
                      ...)
  }else{
    g <- g +
      geom_pointrange(size = point_size / .pt,
                      shape = point_shape,
                      width = line_width,
                      linewidth = line_width,
                      linetype = line_type,
                      position = position_dodge(dodge_val),
                      alpha = alpha,
                      ...)
  }

  # Add arrows to the plot to point toward the out of bounds data points
  g <- g |>
    draw_arrows_yoob(yoob)

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

  g <- g +
    theme(axis.text.x = element_text(vjust = vjust_x_labels),
          axis.title.x = element_text(vjust = vjust_x_labels))

  g
}