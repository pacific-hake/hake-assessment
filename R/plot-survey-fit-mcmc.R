#' Creates a plot of fits to survey biomass including individual posterior
#' lines and the median posterior line
#'
#' @param model A model object, , created by [create_rds_file()]
#' @param type One of the surveys, either `age1` or `acoustic`
#' @param n_posts The number of posterior lines to plot. The lines are
#' randomly drawn from all posteriors. if this is larger than the number
#' of posteriors, all posterior lines will be shown
#' @param probs A vector of three probabilities for the credible interval,
#' with the middle one being the median (0.5)
#' @param show_legend Logical. If `TRUE`, show the legend
#' @param axis_title_font_size Size of the font for the X and Y axis labels
#' @param axis_tick_font_size Size of the font for the X and Y axis tick labels
#' @param axis_label_color Color for the axis labels and tick labels
#'
#' @return a [ggplot2::ggplot()] object
#' @export
plot_survey_fit_mcmc <- function(model,
                                 type = c("age1", "acoustic"),
                                 n_posts = 1000,
                                 probs = c(0.025, 0.5, 0.975),
                                 show_legend = TRUE,
                                 leg_ncol = 1,
                                 leg_font_size = 12,
                                 axis_title_font_size = 14,
                                 axis_tick_font_size = 11,
                                 axis_label_color = "black"){

  stopifnot(length(probs) == 3)
  stopifnot(probs[2] == 0.5)

  type <- match.arg(type)

  surv_index <- ifelse(type == "age1", 3, 2)

  # Total number of samples available
  n_samp <- model$extra_mcmc$num_posts
  n_posts <- ifelse(n_samp < n_posts, n_samp, n_posts)
  # A random sub-sample of those
  subsample <- sample(n_samp, n_posts)

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
    mutate(post = factor(post, levels = sort(unique(post))))

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
  g <- ggplot(obs,
              aes(x = yr,
                  y = med,
                  ymin = lo,
                  ymax = hi)) +
    geom_line(data = index_posts,
              aes(x = yr,
                  y = med,
                  group = post),
              color = "blue",
              linewidth = 0.1,
              inherit.aes = FALSE) +
    geom_line(data = index_med,
              aes(x = yr,
                  y = med),
              color = "darkblue",
              linewidth = 2.5,
              inherit.aes = FALSE) +
    geom_errorbar(data = obs_extra_sd,
                  linewidth = 0.5,
                  width = 0.25) +
    geom_errorbar(width = 0,
                  linewidth = 1.5) +
    geom_point(shape = 1,
               size = 3) +
    scale_x_continuous(breaks = x_breaks,
                       labels = x_labels) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(ylim = c(0, 5)) +
    xlab("Year") +
    ylab("Biomass index (Mt)") +
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
    symbol_y <- 4.8
    g <- g +
      annotate("segment",
               x = symbol_x_start + 0.5,
               xend = symbol_x_end - 0.5,
               y = symbol_y,
               yend = symbol_y,
               colour = "black",
               linewidth = 1) +
      annotate("point",
               x = symbol_x,
               y = symbol_y,
               shape = 1,
               size = 3) +
      annotate("segment",
               x = symbol_x_start,
               xend = symbol_x_end,
               y = symbol_y - 0.2,
               yend = symbol_y - 0.2,
               colour = "darkblue",
               linewidth = 1.5) +
      annotate("segment",
               x = symbol_x_start,
               xend = symbol_x_end,
               y = symbol_y - 0.4,
               yend = symbol_y - 0.4,
               colour = "blue",
               linewidth = 0.5) +
      annotate("segment",
               x = symbol_x_start,
               xend = symbol_x_end,
               y = symbol_y - 0.3,
               yend = symbol_y - 0.45,
               colour = "blue",
               linewidth = 0.5) +
      annotate("segment",
               x = symbol_x_start,
               xend = symbol_x_end,
               y = symbol_y - 0.42,
               yend = symbol_y - 0.33,
               colour = "blue",
               linewidth = 0.5) +
      annotate("text",
               x = text_x,
               y = symbol_y,
               label = paste0("Observed survey biomass with input (wide) ",
                              "and estimated (narrow) 95% intervals"),
               hjust = 0) +
      annotate("text",
               x = text_x,
               y = symbol_y - 0.2,
               label = "Median MCMC estimate of expected survey biomass",
               hjust = 0) +
      annotate("text",
               x = text_x,
               y = symbol_y - 0.4,
               label = paste0("A subset (",
                              f(n_samp),
                              ") of the MCMC estimates of expected survey ",
                              "biomass"),
               hjust = 0)
  }else{
    g <- g +
      theme(legend.position = "none")
 }

  g
}