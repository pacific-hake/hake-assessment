#' Plot survey index fits from MCMC output for one or more models,
#' for one of the acoustic surveys
#'
#' @rdname plot_biomass
#' @param ... Arguments passed to [ggplot2::geom_pointrange()]
#' @export
plot_survey_index_fits <- function(
    model_lst = NULL,
    model_names = NULL,
    d_obj = NULL,
    show_arrows = TRUE,
    survey_type = c("age1",
                    "age2"),
    xlim = c(1995, 2021),
    x_breaks = xlim[1]:xlim[2],
    x_labs_mod = 5,
    x_expansion = 3,
    tick_prop = 1,
    ylim = c(0, 3),
    y_breaks = seq(ylim[1], ylim[2], by = 0.5),
    leg_pos = c(0.65, 0.83),
    leg_ncol = 1,
    leg_font_size = 12,
    alpha = 1,
    point_size = ts_pointsize,
    point_color = ts_single_model_pointcolor,
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
    obs_point_shape = 17,
    obs_point_size = ts_pointsize,
    obs_line_type = "dashed",
    obs_color = "black",
    dodge_val = 0.5,
    rev_colors = FALSE,
    ...){

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
  is_single_model <- length(unique(d$model)) == 1

  colors <- plot_color(length(unique(d$model)) - 1)
  num_models <- length(unique(d$model)) - 1 # Minus 1 for observed
  if(rev_colors){
    colors <- rev(colors)
  }

  # Extract observed from data frame, and modify levels accordingly
  obs_str <- "Observed"
  d_obs <- d |>
    filter(model == !!obs_str) |>
    mutate(model = factor(model, levels = obs_str))
  model_nms_no_obs <- as.character(unique(d$model))
  model_nms_no_obs <- model_nms_no_obs[model_nms_no_obs != obs_str]
  d <- d |>
    filter(model != !!obs_str) |>
    mutate(model = factor(model, levels = model_nms_no_obs))

  # Add color for Observed data
  colors <- c(colors, obs_color)
  x_labels <- make_major_tick_labels(x_breaks = x_breaks,
                                     modulo = x_labs_mod)

  y_title <- ifelse(survey_type == "age1",
                    "Numbers (billions)",
                    "Biomass (Mt)")

  d <- d |>
    filter(year <= xlim[2] & year >= ylim[1])
  d_obs <- d_obs |>
    filter(year <= xlim[2] & year >= ylim[1])

  # Calculate the data y-axis out-of-bounds (yoob) and change the credible
  # interval in the data to cut off at the limits (or not if `show_arrows`
  # is `TRUE`)
  yoob <- calc_yoob(d, ylim, "index_lo", "index_med", "index_hi",
                    show_arrows)
  yoob_obs <- calc_yoob(d_obs, ylim, "index_lo", "index_med", "index_hi",
                        show_arrows)

  g <- ggplot(yoob$d,
              aes(x = year,
                  y = index_med,
                  ymin = index_lo,
                  ymax = index_hi,
                  group = model,
                  color = model)) +
    scale_color_manual(values = colors) +
    coord_cartesian(xlim = xlim,
                    ylim = ylim,
                    clip = "off") +
    scale_x_continuous(expand = c(0, x_expansion),
                       breaks = x_breaks,
                       labels = x_labels) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       labels = y_breaks) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = leg_font_size),
          legend.text.align = 0,
          axis.text.x = element_text(vjust = -2),
          axis.title.x = element_text(vjust = -2)) +
    xlab("Year") +
    ylab(y_title)

  # Add the points and error bars
  if(is_single_model){
    g <- g +
      geom_pointrange(fatten = 1,
                      size = point_size,
                      shape = point_shape,
                      stroke = point_stroke,
                      color = point_color,
                      alpha = alpha,
                      ...)
  }else{
    g <- g +
      geom_pointrange(fatten = 1,
                      size = point_size,
                      shape = point_shape,
                      stroke = point_stroke,
                      position = position_dodge(dodge_val),
                      alpha = alpha,
                      ...)
  }

  # Add the lines connecting the points.
  # `do.call()` used here to include the `color` argument only if
  # `is_single_model`is `TRUE`
  g <- g +
    do.call(geom_path,
            c(list(linewidth = line_width),
              list(color = line_color)[is_single_model]))

  # Add the observed line and points
  g <- g +
    geom_pointpath(data = yoob_obs$d,
                   shape = obs_point_shape,
                   size = obs_point_size,
                   stroke = point_stroke,
                   color = obs_color,
                   size = point_size,
                   linetype = obs_line_type)

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