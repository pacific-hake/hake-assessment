#' Plot survey index fits from MCMC output for one or more models,
#' for one of the acoustic surveys
#'
#' @rdname plot_biomass
#' @param model A model list which is used for plotting the 'observed' values.
#' The `model_lst` argument contains the models to plot the fit for
#' @param ... Arguments passed to [ggplot2::geom_pointrange()]
#' @export
plot_survey_index_fits <- function(
    model,
    model_lst = NULL,
    model_names = NULL,
    d_obj = NULL,
    show_arrows = TRUE,
    survey_type = c("age1",
                    "age2",
                    "edna"),
    xlim = c(1995, 2021),
    x_breaks = xlim[1]:xlim[2],
    x_labs_mod = 5,
    x_expansion = 3,
    tick_prop = 1,
    vjust_x_labels = -0.25,
    ylim = c(0, 3),
    y_breaks = seq(ylim[1], ylim[2], by = 0.5),
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
    line_width = ifelse(is_single_model,
                        ts_single_model_linewidth,
                        ts_linewidth),
    line_type = ts_single_model_linetype,
    line_color = ts_single_model_linecolor,
    obs_point_shape = 17,
    obs_point_size = point_size * 2,
    obs_line_type = "dashed",
    obs_line_width = 1.25,
    obs_err_line_type = "solid",
    obs_err_line_width = 1,
    obs_upper_bar_width = 0.25,
    obs_color = "black",
    dodge_val = 0.5,
    rev_colors = FALSE,
    ...){

  survey_type <- match.arg(survey_type)
  survey_index <- ifelse(survey_type == "age2", 2, 3)

  if(is.null(d_obj)){
    if(is.null(model_lst[1]) || is.null(model_names[1])){
      stop("Either `d_obj` or both `model_lst` and `model_names` ",
           "must be supplied. Both are `NULL`")
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

  # Extract observed from data frame, and modify levels accordingly ------------
  obs_str <- "Observed"
  model_nms_no_obs <- as.character(unique(d$model))
  model_nms_no_obs <- model_nms_no_obs[model_nms_no_obs != obs_str]
  d <- d |>
    dplyr::filter(model != !!obs_str) |>
    mutate(model = factor(model, levels = model_nms_no_obs))

  # Add color for Observed data
  colors <- c(colors, obs_color)
  x_labels <- make_major_tick_labels(x_breaks = x_breaks,
                                     modulo = x_labs_mod)

  y_title <- ifelse(survey_type == "age1",
                    "Numbers (billions)",
                    "Biomass (Mt)")

  d <- d |>
    dplyr::filter(year <= xlim[2] & year >= ylim[1])

  # Extract observed index values (errorbars on index observations) ------------
  d_obs <- model$dat$CPUE |>
    as_tibble() |>
    dplyr::filter(index == survey_index)
  lo <- qlnorm(probs[1],
               meanlog = log(as.numeric(d_obs$obs)),
               sdlog = as.numeric(d_obs$se_log))
  hi <- qlnorm(probs[3],
               meanlog = log(as.numeric(d_obs$obs)),
               sdlog = as.numeric(d_obs$se_log))
  d_obs <- d_obs |>
    transmute(yr = year,
              med = obs) |>
    mutate(lo = !!lo,
           hi = !!hi) |>
    mutate(across(-yr, ~{.x / 1e6})) |>
    rename(year = yr,
           index_lo = lo,
           index_med = med,
           index_hi = hi) |>
    mutate(year = as.numeric(year))

  # Calculate the data y-axis out-of-bounds (yoob) and change the credible -----
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
          axis.text.x = element_text(vjust = vjust_x_labels),
          axis.title.x = element_text(vjust = vjust_x_labels)) +
    xlab("Year") +
    ylab(y_title) +
    guides(color = guide_legend(override.aes = list(stroke = 0.1)))

  # Add the points and error bars
  if(is_single_model){
    g <- g +
      geom_pointrange(size = point_size / .pt,
                      shape = point_shape,
                      color = point_color,
                      alpha = alpha,
                      ...)
  }else{
    g <- g +
      geom_pointrange(size = point_size / .pt,
                      shape = point_shape,
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
                   aes(x = year,
                       y = index_med),
                   shape = obs_point_shape,
                   size = obs_point_size,
                   color = obs_color,
                   linetype = obs_line_type,
                   linewidth = line_width,
                   inherit.aes = FALSE) +
    geom_errorbar(data = yoob_obs$d,
                  aes(x = year,
                      ymin = index_lo,
                      ymax = index_hi),
                  linetype = obs_err_line_type,
                  linewidth = obs_err_line_width,
                  color = obs_color,
                  width = obs_upper_bar_width,
                  inherit.aes = FALSE)

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
