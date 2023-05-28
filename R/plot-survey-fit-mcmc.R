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
#' @param axis_title_font_size Size of the font for the X and Y axis labels
#' @param axis_tick_font_size Size of the font for the X and Y axis tick labels
#' @param axis_label_color Color for the axis labels and tick labels
#' @param glow Logical. If `TRUE`, add a white glow around the lines so that
#' they can more easily be seen in contrast to the background posterior lines
#' @param ylim A vector of two values representing the minimum and maximum
#' values to appear on the y-axis
#' @param post_line_col The color of the thinner individual posterior lines
#' @param post_line_alpha The transparency of the thinner individual
#' posterior lines
#' @param post_med_color The color of the posterior median line and points
#' @param post_med_line_width The width of the posterior median line
#' @param post_med_point_size The size of the posterior median points
#' @param err_col The color of the error bars (both narrow and thick)
#' @param err_obs_line_width The width of the error bar lines for observed
#' values
#' @param err_est_line_width The width of the error bar lines for estimated
#' values
#' @param err_obs_point_size The size of the points on the error bars
#' @param glow_col_err The color of the 'glow' on the error bar lines
#' @param glow_col_med The color of the 'glow' on the posterior median line
#' @param glow_alpha_err The transparency of the 'glow' on the error bars
#' and points
#' @param glow_alpha_med The transparency of the 'glow' on the posterior
#' median line
#'
#' @return a [ggplot2::ggplot()] object
#' @export
plot_survey_fit_mcmc <- function(model,
                                 type = c("age1", "acoustic"),
                                 n_posts = NULL,
                                 show_legend = TRUE,
                                 ylim = c(0, 5),
                                 axis_title_font_size = 14,
                                 axis_tick_font_size = 11,
                                 axis_label_color = "black",
                                 post_line_col = "gray40",
                                 post_line_alpha = 1,
                                 post_med_color = "darkblue",
                                 post_med_line_width = 2,
                                 post_med_point_size = 3,
                                 err_col = "blue",
                                 err_obs_line_width = 2,
                                 err_est_line_width = 0.5,
                                 err_obs_point_size = 3,
                                 glow = TRUE,
                                 glow_col_err = "white",
                                 glow_col_med = "white",
                                 glow_alpha_err = 0.7,
                                 glow_alpha_med = 0.7){

  type <- match.arg(type)

  surv_index <- ifelse(type == "age1", 3, 2)

  # Total number of samples available
  n_samp <- model$extra_mcmc$num_posts
  n_posts <- ifelse(n_samp < n_posts, n_samp, n_posts)
  # A random sub-sample of those
  if(is.null(n_posts)){
    subsample <- seq_len(n_samp)
  }else{
    subsample <- sample(n_samp, n_posts)
  }

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
    geom_line(data = index_posts,
              aes(x = yr,
                  y = med,
                  group = post),
              color = post_line_col,
              linewidth = 0.1,
              alpha = post_line_alpha,
              inherit.aes = FALSE)

  if(glow){
    g <- g +
      geom_line(data = index_med,
                aes(x = yr,
                    y = med),
                color = glow_col_med,
                alpha = glow_alpha_med,
                linewidth = post_med_line_width + 0.5,
                inherit.aes = FALSE)
  }

  g <- g +
    geom_line(data = index_med,
              aes(x = yr,
                  y = med),
              color = post_med_color,
              linewidth = post_med_line_width,
              inherit.aes = FALSE)

  if(glow){
    g <- g +
      geom_point(data = index_med,
               aes(x = yr,
                   y = med),
               color = glow_col_med,
               alpha = glow_alpha_med,
               size = post_med_point_size + 1,
               inherit.aes = FALSE)
  }

  g <- g +
    geom_point(data = index_med,
               aes(x = yr,
                   y = med),
               color = post_med_color,
               size = post_med_point_size,
               inherit.aes = FALSE)

  if(glow){
    g <- g +
      geom_errorbar(data = obs_extra_sd,
                    linewidth = err_est_line_width + 0.5,
                    width = 0,
                    color = glow_col_err,
                    alpha = glow_alpha_err,
                    lineend = "round")
  }

  g <- g +
    geom_errorbar(data = obs_extra_sd,
                  linewidth = err_est_line_width,
                  color = err_col,
                  width = 0,
                  lineend = "round")

  if(glow){
    g <- g +
      geom_errorbar(width = 0,
                    linewidth = err_obs_line_width + 0.5,
                    color = glow_col_err,
                    alpha = glow_alpha_err,
                    lineend = "round")
  }

  g <- g +
    geom_errorbar(width = 0,
                  color = err_col,
                  linewidth = err_obs_line_width,
                  lineend = "round")

  if(glow){
    g <- g +
      geom_point(size = err_obs_point_size + 0.5,
                 color = glow_col_err,
                 alpha = glow_alpha_err)
  }

  g <- g +
    geom_point(size = err_obs_point_size) +
    scale_x_continuous(breaks = x_breaks,
                       labels = x_labels) +
    scale_y_continuous(breaks = y_breaks,
                       expand = c(0, 0)) +
    coord_cartesian(ylim = ylim) +
    xlab("Year") +
    ylab(ifelse(type == "acoustic",
                "Biomass index (Mt)",
                "Relative Age-1 index (billions of fish)")) +
    theme(axis.text.x = element_text(color = axis_label_color,
                                     size = axis_tick_font_size,
                                     angle = 0,
                                     hjust = 0.5,
                                     vjust = -3,
                                     face = "plain"),
          axis.text.y = element_text(color = axis_label_color,
                                     size = axis_tick_font_size,
                                     hjust = 1,
                                     vjust = 0.5,
                                     face = "plain"),
          axis.title.x = element_text(color = axis_label_color,
                                      size = axis_title_font_size,
                                      angle = 0,
                                      vjust = -2,
                                      face = "plain"),
          axis.title.y = element_text(color = axis_label_color,
                                      size = axis_title_font_size,
                                      angle = 90,
                                      face = "plain"),
          axis.ticks.length = unit(0.15, "cm"),
          plot.margin = margin(12, 12, 12, 5))

  if(show_legend){
    symbol_x <- 1995
    symbol_x_start <- 1994
    symbol_x_end <- 1996
    text_x <- 1996.25
    type_off <- ifelse(type == "age1", 2, 1)
    symbol_y <- max(ylim) - 0.2 * type_off
    g <- g +
      # Error bar symbol construction
      annotate("segment",
               x = symbol_x_start,
               xend = symbol_x_end,
               y = symbol_y,
               yend = symbol_y,
               colour = err_col,
               linewidth = err_est_line_width) +
      annotate("segment",
               x = symbol_x_start + 0.5,
               xend = symbol_x_end - 0.5,
               y = symbol_y,
               yend = symbol_y,
               colour = err_col,
               linewidth = err_obs_line_width) +
      annotate("point",
               x = symbol_x,
               y = symbol_y,
               shape = 19,
               colour = glow_col_err,
               size = err_obs_point_size + 0.5) +
      annotate("point",
               x = symbol_x,
               y = symbol_y,
               shape = 19,
               colour = "black",
               size = err_obs_point_size) +
      # Median posterior survey fit construction
      annotate("segment",
               x = symbol_x_start,
               xend = symbol_x_end,
               y = symbol_y - 0.2 * type_off,
               yend = symbol_y - 0.2 * type_off,
               colour = post_med_color,
               linewidth = post_med_line_width + 0.5) +
      annotate("segment",
               x = symbol_x_start,
               xend = symbol_x_end,
               y = symbol_y - 0.2 * type_off,
               yend = symbol_y - 0.2 * type_off,
               colour = post_med_color,
               linewidth = post_med_line_width) +
      annotate("point",
               x = symbol_x,
               y = symbol_y - 0.2 * type_off,
               shape = 19,
               colour = glow_col_med,
               size = post_med_point_size + 1) +
      annotate("point",
               x = symbol_x,
               y = symbol_y - 0.2 * type_off,
               shape = 19,
               colour = post_med_color,
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
               label = paste0("A subset (",
                              f(n_posts),
                              ") of the MCMC estimates of ",
                              ifelse(type == "acoustic",
                                     "expected survey biomass",
                                     "scaled age-1 numbers")),
               hjust = 0)
  }else{
    g <- g +
      theme(legend.position = "none")
 }

  g
}