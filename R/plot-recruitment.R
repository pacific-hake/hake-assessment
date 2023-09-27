#' Plot recruitment from MCMC output for one or more models
#'
#' @rdname plot_biomass
#' @param relative Logical. If `TRUE`, create a relative recruitment plot using
#' the `r_rel_lower`, `r_rel_med`, `r_rel_upper`, `r_rel_mean`, and
#' `r_rel_init` elements of `mcmccalcs` which are created in the function
#' [calc_mcmc()]. In that function, the relative year is set. It is 2010 by
#' default. To change it, edit the year in the [calc_mcmc()] function, and
#' then [create_rds_file()] needs to be run again to create the RDS file for
#' the model
#' @export
plot_recruitment <- function(
    model_lst = NULL,
    model_names = NULL,
    show_arrows = TRUE,
    inc_means = FALSE,
    relative = FALSE,
    xlim = c(1966, year(Sys.time())),
    x_breaks = xlim[1]:xlim[2],
    x_labs_mod = 5,
    x_expansion = 2,
    tick_prop = 1,
    vjust_x_labels = -2,
    ylim = c(0, 40),
    y_breaks = seq(ylim[1], ylim[2], by = 10),
    y_labels = y_breaks,
    y_colors = rep("black", length(y_breaks)),
    alpha = 0.3,
    leg_pos = c(0.65, 0.83),
    leg_ncol = 1,
    leg_font_size = 12,
    point_size = ts_pointsize,
    point_stroke = ts_single_model_pointstroke,
    line_width = ts_linewidth,
    single_point_color = ts_single_model_pointcolor,
    single_line_color = ts_single_model_linecolor,
    crossbar_width = 0,
    dodge_val = 0.5,
    rev_colors = TRUE,
    d_obj = NULL){

  if(is.null(d_obj)){
    if(is.null(model_lst[1]) || is.null(model_names[1])){
      stop("Either `d_obj` or both `model_lst` and `model_names` ",
           "must be supplied. Both are `NULL`",
           call. = FALSE)
    }
    d_obj <- create_group_df_recr(model_lst,
                                  model_names,
                                  relative = relative)
  }

  if(relative){
    rlower <- "r_rel_lower"
    rmed <- "r_rel_med"
    rupper <- "r_rel_upper"
    rmean <- "r_rel_mean"
    rinit <- "r_rel_init"
  }else{
    rlower <- "rlower"
    rmed <- "rmed"
    rupper <- "rupper"
    rmean <- "rmean"
    rinit <- "rinit"
  }
  rlower_sym <- sym(rlower)
  rmed_sym <- sym(rmed)
  rupper_sym <- sym(rupper)
  rmean_sym <- sym(rmean)

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
    ro_vec <- model_lst[[1]]$mcmccalcs[[rinit]]
    yrs <- c(seq(min(d$year) - x_expansion,
                 min(d$year) - 1), d$year)
    ro <- tibble(model = model_names[[1]],
                 year = yrs,
                 !!rlower_sym := ro_vec[1],
                 !!rmed_sym := ro_vec[2],
                 !!rupper_sym := ro_vec[3]) |>
      mutate(model = factor(model))
  }

  x_labels <- make_major_tick_labels(x_breaks = x_breaks,
                                     modulo = x_labs_mod)

  # Remove projection years
  d <- d |>
    filter(year <= xlim[2] & year >= xlim[1])

  # Calculate the data outside the range of the y limits and
  # change the CI in the data to cut off at the limits
  yoob <- calc_yoob(d, ylim, rlower, rmed, rupper, show_arrows)

  g <- ggplot(yoob$d,
              aes(x = year,
                  y = !!rmed_sym,
                  ymin = !!rlower_sym,
                  ymax = !!rupper_sym,
                  group = model,
                  color = model,
                  fill = model)) +
    scale_color_manual(values = line_colors) +
    coord_cartesian(xlim = xlim,
                    ylim = ylim,
                    clip = "off")

  if(is_single_model){
    ro_val <- ro[[rmed_sym]][1]
    below <- which(ro_val > y_breaks)
    above <- which(ro_val < y_breaks)
    tmp <- y_breaks
    y_breaks <- c(tmp[below], ro_val, tmp[above])
    y_labels <- c(tmp[below], expression(R[0]), tmp[above])

    # Expand right side of ro so that ribbon covers whole plot
    ro_row <- ro[nrow(ro), ]
    ro_next_yr <- ro_row$year
    for(yr in ro_next_yr:(ro_next_yr + x_expansion)){
      ro_row$year <- yr
      ro <- bind_rows(ro, ro_row)
    }

    g <- g +
      geom_point(size = point_size,
                 color = colors) +
      geom_hline(data = ro,
                 aes(yintercept = !!rmed_sym),
                 linetype = "dashed") +
      geom_ribbon(data = ro,
                  aes(ymin = !!rlower_sym,
                      ymax = !!rupper_sym),
                  alpha = alpha,
                  fill = "black",
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
          legend.text.align = 0,
          axis.text.y = element_text(color = y_colors),
          # These two commands move the x-axis major tick labels and axis
          # title down so that the ticks. tick labels, and axis title don't
          # overlap each other
          axis.text.x = element_text(vjust = vjust_x_labels),
          axis.title.x = element_text(vjust = vjust_x_labels)) +
    xlab("Year") +
    ylab(ifelse(relative,
                "Age-0 recruits (billions) relative to 2010 values",
                "Age-0 recruits (billions)"))

  if(inc_means){
    if(is_single_model){
      g <- g +
        geom_point(aes(y = !!rmean_sym),
                   shape = 4,
                   color = colors)
    }else{
      g <- g +
        geom_point(aes(y = !!rmean_sym),
                   shape = 4)
    }
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

  g
}