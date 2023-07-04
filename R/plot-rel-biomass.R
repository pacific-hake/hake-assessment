#' Plot relative biomass from MCMC output for one or more models
#'
#' @rdname plot_biomass
#'
#' @export
plot_rel_biomass <- function(
    d_obj = NULL,
    model_lst = NULL,
    model_names = NULL,
    show_arrows = TRUE,
    xlim = c(1966, year(Sys.time())),
    x_breaks = xlim[1]:xlim[2],
    x_labs_mod = 5,
    x_expansion = 3,
    tick_prop = 1,
    vjust_x_labels = -2,
    ylim = c(0, 3.5),
    leg_pos = c(0.65, 0.83),
    leg_ncol = 1,
    leg_font_size = 12,
    rev_colors = FALSE,
    wrap_y_label = FALSE,
    alpha = hake::ts_ribbon_alpha,
    point_size = ifelse(is_single_model,
                        hake::ts_single_model_pointsize,
                        hake::ts_pointsize),
    point_color = hake::ts_single_model_pointcolor,
    point_shape = ifelse(is_single_model,
                         hake::ts_single_model_pointshape,
                         hake::ts_pointshape),
    line_width = ifelse(is_single_model,
                        hake::ts_single_model_linewidth,
                        hake::ts_linewidth),
    single_line_color = hake::ts_single_model_linecolor,
    single_ribbon_color = hake::ts_single_model_ribbon_fill,
    ribbon_line_type = ifelse(is_single_model,
                              hake::ts_single_model_ribbon_linetype,
                              hake::ts_ribbon_linetype),
    refpt_bo_linecolor = hake::refpt_bo_linecolor,
    refpt_usr_linecolor = hake::refpt_usr_linecolor,
    refpt_lrp_linecolor = hake::refpt_lrp_linecolor,
    refpt_bo_linewidth = hake::refpt_bo_linewidth,
    refpt_usr_linewidth = hake::refpt_usr_linewidth,
    refpt_lrp_linewidth = hake::refpt_lrp_linewidth,
    refpt_bo_linetype = hake::refpt_bo_linetype,
    refpt_usr_linetype = hake::refpt_usr_linetype,
    refpt_lrp_linetype = hake::refpt_lrp_linetype){

  if(is.null(d_obj)){
    if(is.null(model_lst[1]) || is.null(model_names[1])){
      stop("Either `d_obj` or both `model_lst` and `model_names` ",
           "must be supplied. Both are `NULL`",
           call. = FALSE)
    }
    d_obj <- create_group_df_biomass(model_lst,
                                     model_names,
                                     rel = TRUE)
  }

  if(ylim[2] %% 0.5 != 0){
    stop("The upper `ylim` value must be divisible by 0.5",
         call. = FALSE)
  }

  x_labels <- make_major_tick_labels(x_breaks = x_breaks,
                                     modulo = x_labs_mod)

  y_breaks <- c(0, 0.1, 0.4, 0.5, 1.0)
  y_labels <- expression("0",
                         "0.1B"[0],
                         "0.4B"[0],
                         "0.5",
                         "B"[0])
  y_colors <- c("black",
                refpt_lrp_linecolor,
                refpt_usr_linecolor,
                "black",
                refpt_bo_linecolor)
  if(ylim[2] < 1){
    stop("Relative spawning biomass plot y-axis max must be 1 or greater",
         call. = FALSE)
  }
  if(ylim[2] > 1){
    seq_above_1 <- seq(1.5,
                       ylim[2],
                       by = 0.5)
    y_breaks <- c(y_breaks,
                  seq_above_1)
    y_labels <- c(y_labels,
                  parse(text = as.character(seq_above_1)))
    y_colors <- c(y_colors,
                  rep("black",
                      length(seq_above_1)))
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
    filter(year <= xlim[2] & year >= xlim[1])

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
                    clip = "off") +
    geom_ribbon(alpha = alpha,
                linetype = ribbon_line_type) +
    geom_line(linewidth = line_width)

  if(is_single_model){
    g <- g +
      geom_point(size = point_size,
                 color = point_color,
                 shape = point_shape,
                 stroke = 1.25)
  }else{
    g <- g +
      geom_point(size = point_size,
                 shape = point_shape)
  }

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
          # These two commands move the x-axis major tick labels and axis
          # title down so that the ticks. tick labels, and axis title don't
          # overlap each other
          axis.text.x = element_text(vjust = vjust_x_labels),
          axis.title.x = element_text(vjust = vjust_x_labels),
          axis.text.y = element_text(color = y_colors),
          # plot.margin: top, right,bottom, left
          # Needed to avoid tick labels cutting off
          plot.margin = margin(12, 12, 7, 0)) +
    labs(x = "Year",
         y = ifelse(wrap_y_label,
                    expression(atop("Rel. Spawning Biomass",
                                    paste("("~B[t]/B[0]~")"))),
                    expression(paste("Rel. Spawning Biomass ("~B[t]/B[0]~")"))))

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
      guides(fill = guide_legend(ncol = leg_ncol),
             color = guide_legend(ncol = leg_ncol))
  }

  g
}
