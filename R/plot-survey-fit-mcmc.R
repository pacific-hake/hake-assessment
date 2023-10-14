#' Creates a plot of fits to survey biomass including individual posterior
#' lines, median posterior line, observed values, and estimated values
#'
#' @details
#' Uses the [probs] vector which is included in the package data for
#' this package
#'
#' @param model A model object, , created by [create_rds_file()]
#' @param type One of the surveys, either `age1` or `acoustic`
#' @param n_posts The number of posterior lines to plot. The lines are
#' randomly drawn from all posteriors. if this is larger than the number
#' of posteriors, all posterior lines will be shown
#' @param show_legend Logical. If `TRUE`, show the legend
#' @param ylim A vector of two values representing the minimum and maximum
#' values to appear on the y-axis
#' @param post_line_col The color of the thinner individual posterior lines
#' @param post_line_alpha The transparency of the thinner individual
#' posterior lines
#' @param post_med_line_width The width of the posterior median line
#' @param post_med_point_size The size of the posterior median points
#' @param obs_line_color the color for the uncertainty bars for the observed
#' values
#' @param obs_point_color the color for the uncertainty median points for
#' the observed values
#' @param obs_line_width The width of the error bar lines for observed
#' values
#' @param obs_point_size The size of the points on the error bars for the
#' observed values
#' @param extrasd_line_color The color of the error bars for the
#'  extra SD parameter estimates
#' @param extrasd_point_color The color of the median points for the
#'  extra SD parameter estimates
#' @param extrasd_line_width The width of the error bar lines for the
#'  extra SD parameter estimates
#' @param extrasd_point_size The sizeof the median points for the
#' extra SD parameter estimates
#' @param glow Logical. If `TRUE`, add a glow around the lines so that
#' they can more easily be seen in contrast to the background posterior lines
#' @param glow_offset The amount to add to the lines and points to create the
#' @param post_line_width The width of the posterior lines
#' @param post_med_line_color The color of the posterior median line
#' @param post_med_line_alpha The transparency of the posterior median line
#' @param obs_alpha The transparency of the observed lines
#' @param extrasd_alpha The transparency of the extra SD lines
#' @param glow_color The color to use for the "glow" effect around lines
#' @param glow_alpha The transparency to use for the "glow" effect around
#' lines
#' @param post_line_gap The line gap value (separation between points and the
#' line connecting them)
#' @param leg_xmin The bottom left x value for drawing the white box under
#' the legend
#' @param leg_xmax The bottom right x value for drawing the white box under
#' the legend
#' @param leg_ymin The top left y value for drawing the white box under
#' the legend
#' @param leg_ymax The top right y value for drawing the white box under
#' the legend
#'
#' @return a [ggplot2::ggplot()] object
#' @export
plot_survey_fit_mcmc <- function(model,
                                 type = c("age1", "acoustic"),
                                 n_posts = NULL,
                                 show_legend = TRUE,
                                 ylim = c(0, 5),
                                 post_line_width = 0.1,
                                 post_line_col = "black",
                                 post_line_alpha = 0.1,
                                 post_med_line_width = 1,
                                 post_med_line_color = "royalblue",
                                 post_med_line_alpha = main_alpha,
                                 post_med_point_size = 1.5,
                                 post_line_gap = ts_linegap,
                                 obs_line_width = 1,
                                 obs_line_color = "royalblue",
                                 obs_point_color = "black",
                                 obs_point_size = 1.5,
                                 obs_alpha = 1,
                                 extrasd_line_width = 0.5,
                                 extrasd_line_color = "royalblue",
                                 extrasd_point_color = "royalblue",
                                 extrasd_point_size = 2,
                                 extrasd_alpha = 1,
                                 glow = FALSE,
                                 glow_offset = 0.25,
                                 glow_color = "black",
                                 glow_alpha = 1,
                                 leg_xmin = survey_start_yr - 1,
                                 leg_xmax = survey_end_yr - 2,
                                 leg_ymin = ylim[2] - 1.5,
                                 leg_ymax = ylim[2] - 0.25){

  type <- match.arg(type)

  surv_index <- ifelse(type == "age1", 3, 2)

  # Total number of samples available, and the number of posteriors requested
  # which, if `NULL` becomes the number of samples
  n_samp <- model$extra_mcmc$num_posts
  n_posts <- n_posts %||% n_samp
  if(n_samp < n_posts){
    warning("the number of posteriors available (", n_samp, ") is less than ",
            "the requested number of samples to use in the plot (", n_posts,
            "). Using all available samples (", n_samp, ")")
    n_posts <- n_samp
  }
  subsample <- sample(n_samp, n_posts)
  legend_text <- ifelse(n_posts == n_samp,
                        paste0("All (",
                               f(n_posts),
                               ") of the MCMC estimates of ",
                               ifelse(type == "acoustic",
                                      "expected survey biomass",
                                      "scaled age-1 numbers")),
                        paste0("A subset (",
                               f(n_posts),
                               ") of the MCMC estimates of ",
                               ifelse(type == "acoustic",
                                      "expected survey biomass",
                                      "scaled age-1 numbers")))

  # Extract observed index values (thick errorbars) ----
  obs <- model$dat$CPUE |>
    as_tibble() |>
    filter(index == surv_index)
  lo <- qlnorm(probs[1],
               meanlog = log(as.numeric(obs$obs)),
               sdlog = as.numeric(obs$se_log))
  hi <- qlnorm(probs[3],
               meanlog = log(as.numeric(obs$obs)),
               sdlog = as.numeric(obs$se_log))
  obs <- obs |>
    transmute(yr = year,
              med = obs) |>
    mutate(lo = !!lo,
           hi = !!hi) |>
    mutate(across(-yr, ~{.x / 1e6}))

  # Extract the extra SD value for the given survey `type`
  pat <- "Q_extraSD_(Age1|Acoustic)_Survey\\(\\d+\\)"

  extra_sd <- model$mcmc |>
    summarize(across(matches(pat),
                     ~median(.x))) |>
    unlist()
  names(extra_sd) <- tolower(gsub(pat, "\\1", names(extra_sd)))
  # `added_se` is a single numeric value
  extra_sd <- extra_sd[names(extra_sd) == type]

  # Extract observed index values (thin errorbars) ----
  # These have the extra SD added on
  obs_extra_sd <- model$dat$CPUE |>
    as_tibble() |>
    filter(index == surv_index)

  lo <- qlnorm(probs[1],
               meanlog = log(as.numeric(obs_extra_sd$obs)),
               sdlog = as.numeric(obs_extra_sd$se_log) + extra_sd)
  hi <- qlnorm(probs[3],
               meanlog = log(as.numeric(obs_extra_sd$obs)),
               sdlog = as.numeric(obs_extra_sd$se_log) + extra_sd)
  obs_extra_sd <- obs_extra_sd |>
    transmute(yr = year,
              med = obs) |>
    mutate(lo = !!lo,
           hi = !!hi) |>
    mutate(across(-yr, ~{.x / 1e6}))

  # Extract the index fit posterior lines ----
  index_posts <- model$extra_mcmc$index_fit_posts |>
    filter(fleet %in% surv_index) |>
    select(-fleet) |>
    pivot_longer(-yr, names_to = "post", values_to = "med") |>
    mutate(post = as.numeric(post)) |>
    mutate(post = factor(post, levels = sort(unique(post)))) |>
    # Take the random subsample of posterior numbers
    filter(post %in% subsample)

  # Extract index fit median ----
  # index_lo <- model$extra_mcmc$index_lo |>
  #   filter(fleet == surv_index) |>
  #   select(-fleet)
  index_med <- model$extra_mcmc$index_med |>
    filter(fleet == surv_index) |>
    select(-fleet) |>
    rename(med = value)
  # index_hi <- model$extra_mcmc$index_hi |>
  #   filter(fleet == surv_index) |>
  #   select(-fleet)

  x_breaks <- obs$yr
  x_labels <- obs$yr
  x_labels[x_labels == 2012] <- ""
  y_breaks <- seq(min(ylim), max(ylim), by = 1)

  g <- ggplot(obs,
              aes(x = yr,
                  y = med,
                  ymin = lo,
                  ymax = hi)) +
    #Add posteriors lines ----
    geom_line(data = index_posts,
              aes(x = yr,
                  y = med,
                  group = post),
              color = post_line_col,
              linewidth = 0.1,
              alpha = post_line_alpha,
              inherit.aes = FALSE)

  if(glow){
    # Glow 1 - Median posterior point size the same as the "real" point size.
    # This is needed because the lines connecting the points become shorter
    # when the points are larger for the glow effect, so the glow layer
    # will not fully encapsulate the ends on the in-between lines. This
    # fixes the glow at those line ends
    g <- g +
      geom_pointpath(data = index_med,
                     aes(x = yr,
                         y = med),
                     linewidth = post_med_line_width + glow_offset,
                     color = glow_color,
                     alpha = glow_alpha,
                     size = post_med_point_size,
                     mult = post_line_gap,
                     inherit.aes = FALSE) +
      # Glow 2 - Add posterior median points and line
      geom_pointpath(data = index_med,
                     aes(x = yr,
                         y = med),
                     linewidth = post_med_line_width + glow_offset,
                     color = glow_color,
                     alpha = glow_alpha,
                     size = post_med_point_size + glow_offset,
                     mult = post_line_gap,
                     inherit.aes = FALSE)
  }

  # Median posterior line and points
  g <- g +
    geom_pointpath(data = index_med,
                   aes(x = yr,
                       y = med),
                   color = post_med_line_color,
                   size = post_med_point_size,
                   linewidth = post_med_line_width,
                   mult = post_line_gap,
                   inherit.aes = FALSE)

  if(glow){
    # Glow - Extra SD line (Longer than obs)
    g <- g +
      geom_errorbar(data = obs_extra_sd,
                    linewidth = extrasd_line_width + 0.5,
                    width = 0,
                    color = glow_color,
                    alpha = glow_alpha,
                    lineend = "round")
  }

  # Extra SD line (Longer than obs)
  g <- g +
    geom_errorbar(data = obs_extra_sd,
                  linewidth = extrasd_line_width,
                  color = extrasd_line_color,
                  width = 0,
                  lineend = "round")

  if(glow){
    # Glow - Obs line (observed)
    g <- g +
      geom_errorbar(data = obs,
                    width = 0,
                    linewidth = obs_line_width + 0.5,
                    color = glow_color,
                    alpha = glow_alpha,
                    lineend = "round")
  }

  # Obs line (observed)
  g <- g +
    geom_errorbar(data = obs,
                  width = 0,
                  linewidth = obs_line_width,
                  color = obs_line_color,
                  lineend = "round")

  if(glow){
    # Glow - Obs median point
    g <- g +
      geom_point(data = obs,
                 size = obs_point_size + 0.5,
                 color = glow_color,
                 alpha = glow_alpha)
  }

  # Obs median point
  g <- g +
    geom_point(data = obs,
               color = obs_point_color,
               alpha = obs_alpha,
               size = obs_point_size) +
    scale_x_continuous(breaks = x_breaks,
                       labels = x_labels) +
    scale_y_continuous(breaks = y_breaks,
                       expand = c(0, 0)) +
    coord_cartesian(ylim = ylim) +
    xlab("Year") +
    ylab(ifelse(type == "acoustic",
                "Biomass index (Mt)",
                "Relative Age-1 index (billions of fish)"))

  if(show_legend){
    x_rng <- layer_scales(g)$x$range$range
    symbol_x <- x_rng[1]
    symbol_x_start <- symbol_x - 1
    symbol_x_end <- symbol_x + 1
    text_x <- symbol_x_end + 0.25
    type_off <- diff(ylim) / 5
    symbol_y <- max(ylim) - 0.2 * type_off

    g <- g +
      # White rectangle acts as legend background
      geom_rect(aes(xmin = leg_xmin,
                    xmax = leg_xmax,
                    ymin = leg_ymin,
                    ymax = leg_ymax),
                fill = "white") +
      # Error bar symbol construction
      annotate("segment",
               x = symbol_x_start,
               xend = symbol_x_end,
               y = symbol_y,
               yend = symbol_y,
               colour = extrasd_line_color,
               linewidth = extrasd_line_width) +
      annotate("segment",
               x = symbol_x_start + 0.5,
               xend = symbol_x_end - 0.5,
               y = symbol_y,
               yend = symbol_y,
               colour = extrasd_line_color,
               linewidth = obs_line_width) +
      annotate("point",
               x = symbol_x,
               y = symbol_y,
               shape = 19,
               colour = "white",
               size = obs_point_size + glow_offset) +
      annotate("point",
               x = symbol_x,
               y = symbol_y,
               shape = 19,
               colour = obs_point_color,
               size = obs_point_size) +
      # Median posterior survey fit construction
      annotate("segment",
               x = symbol_x_start,
               xend = symbol_x_end,
               y = symbol_y - 0.2 * type_off,
               yend = symbol_y - 0.2 * type_off,
               colour = post_med_line_color,
               linewidth = post_med_line_width + 0.5) +
      annotate("segment",
               x = symbol_x_start,
               xend = symbol_x_end,
               y = symbol_y - 0.2 * type_off,
               yend = symbol_y - 0.2 * type_off,
               colour = post_med_line_color,
               linewidth = post_med_line_width) +
      annotate("point",
               x = symbol_x,
               y = symbol_y - 0.2 * type_off,
               shape = 19,
               colour = "white",
               size = post_med_point_size + 2) +
      annotate("point",
               x = symbol_x,
               y = symbol_y - 0.2 * type_off,
               shape = 19,
               colour = post_med_line_color,
               size = post_med_point_size) +
      # Posterior survey fit individual lines construction
      annotate("segment",
               x = symbol_x_start,
               xend = symbol_x_end,
               y = symbol_y - 0.4 * type_off,
               yend = symbol_y - 0.4 * type_off,
               colour = post_line_col,
               linewidth = 0.2) +
      annotate("segment",
               x = symbol_x_start,
               xend = symbol_x_end,
               y = symbol_y - 0.3 * type_off,
               yend = symbol_y - 0.45 * type_off,
               colour = post_line_col,
               linewidth = 0.2) +
      annotate("segment",
               x = symbol_x_start,
               xend = symbol_x_end,
               y = symbol_y - 0.42 * type_off,
               yend = symbol_y - 0.33 * type_off,
               colour = post_line_col,
               linewidth = 0.2) +
      annotate("text",
               x = text_x,
               y = symbol_y,
               label = paste0("Observed ",
                              ifelse(type == "acoustic",
                                     "survey biomass",
                                     "age-1 index"),
                              " with input (wide) ",
                              "and estimated (narrow) 95% intervals"),
               hjust = 0) +
      annotate("text",
               x = text_x,
               y = symbol_y - 0.2 * type_off,
               label = paste0("Median MCMC estimate of ",
                              ifelse(type == "acoustic",
                                     "expected survey biomass",
                                     "scaled age-1 numbers")),
               hjust = 0) +
      annotate("text",
               x = text_x,
               y = symbol_y - 0.4 * type_off,
               label = legend_text,
               hjust = 0)
  }else{
    g <- g +
      theme(legend.position = "none")
 }

  g
}