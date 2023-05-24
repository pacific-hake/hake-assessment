#' Create a plot of the stock-recruitment relationship with
#' estimated medians and credible intervals by year overlaid. An expected
#' distribution of absolute recruitment is also plotted
#'
#' @param model The model output from Stock Synthesis as loaded by
#' [create_rds_file()].
#' @param probs A vector of three probabilities to use in the call to
#' [stats::quantile()]
#' @param xlim A vector of two values. The x-axis minimum and maximum values
#' @param ylim A vector of two values. The y-axis minimum and maximum values
#' @param axis_title_font_size Size of the font for the X and Y axis labels
#' @param axis_tick_font_size Size of the font for the X and Y axis tick labels
#' @param axis_label_color Color for the axis labels and tick labels
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_stock_recruitment <- function(model,
                                   probs = c(0.025, 0.5, 0.975),
                                   xlim = c(0, 1.4),
                                   ylim = c(0, 7),
                                   axis_title_font_size = 14,
                                   axis_tick_font_size = 11,
                                   axis_label_color = "black"){

  yrs <- start_yr:end_yr

  b <- seq(0, 1.5, 0.005)
  h <- model$mcmc |>
    pull(SR_BH_steep)

  beverton_holt <- \(h, b){
    (4.0 * h * b) / ((1.0 - h) + (5.0 * h - 1.0) * b)
  }

  # Cannot use a data frame here, it's too slow to append rows one by one,
  # and lapply() or map() are too slow as well
  r_mat <- matrix(NA,
                  nrow = nrow(model$mcmc),
                  ncol = length(b))
  for(i in model$mcmc$Iter){
    r_mat[i, ] <- beverton_holt(h[model$mcmc$Iter == i], b)
  }

  r_quants <- apply(r_mat, 2, quantile, probs = probs)
  # Bias adjustment
  sigma_r <- model$sigma_R_in
  adj <- exp(-0.5 * sigma_r ^ 2)

  # Get quantities of interest and calculate lognormal distribution
  # over a range of values
  ymax <- 7
  r_vec <- seq(0, ymax, length = 1000)
  meanlog <- (sigma_r ^ 2) / 2
  dlnorm_vec <- dlnorm(x = r_vec,
                       meanlog = meanlog,
                       sdlog = sigma_r)


  # Stock-recruitment part of the plot (left panel)
  dd <- r_quants |>
    t() |>
    as_tibble() |>
    set_names(c("lo", "med", "hi")) |>
    mutate(b = b)

  r_virg <- model$mcmc[names(model$mcmc) == "Recr_Virgin"] |>
    as_tibble()
  r_vals <- model$mcmc[names(model$mcmc) %in% paste0("Recr_", yrs)] |>
    as_tibble()
  med_r_virg <- r_virg |> unlist() |> median()
  b_virg <- model$mcmc[names(model$mcmc) == "SSB_Virgin"] |>
    as_tibble()
  b_vals <- model$mcmc[names(model$mcmc) %in% paste0("SSB_", yrs)] |>
    as_tibble()

  # Standardize the recruitment and biomass data frames (many-column tibbles)
  # by the virgin recruitment and biomass (1-column tibble)
  calc_ratio <- \(df, df_virg){
    df |>
      mutate(virg = df_virg) |>
      mutate(across(-virg, ~{.x / virg})) |>
      pivot_longer(everything(), names_to = "yr") |>
      filter(yr != "virg") |>
      mutate(yr = gsub(".*?(\\d+$)", "\\1", yr)) |>
      mutate(yr = as.numeric(yr)) |>
      # To remove the weird column names that can't be fixed any other way
      as.matrix() |>
      as.data.frame() |>
      as_tibble() |>
      mutate(value = as.numeric(value)) |>
      group_by(yr) |>
      summarize(lo = quantile(value, probs = probs[1]),
                med = quantile(value, probs = probs[2]),
                hi = quantile(value, probs = probs[3]))
  }
  r_ratio <- calc_ratio(r_vals, r_virg) |>
    set_names(c("yr", "lo_r", "med_r", "hi_r"))
  b_ratio <- calc_ratio(b_vals, b_virg) |>
    set_names(c("yr", "lo_b", "med_b", "hi_b"))

  rb <- r_ratio |>
    full_join(b_ratio, by = "yr") |>
    mutate(yr = factor(yr))

  colvec <- rev(rich_colors_short(length(yrs) + 10, alpha = 0.8))[-(1:10)]
  x_breaks <- seq(min(xlim), max(xlim), by = 0.2)
  y_breaks <- sort(c(round(adj, 2), min(ylim):max(ylim)))

  p <- list()
  p[[1]] <- ggplot(dd) +
    geom_ribbon(aes(x = b,
                    ymin = lo,
                    ymax = hi),
                fill = "grey60",
                linetype = "dotted",
                color = "black",
                linewidth = 0.75) +
    geom_line(aes(x = b,
                  y = med),
              linewidth = 1.5) +
    geom_ribbon(aes(x = b,
                    ymin = lo * adj,
                    ymax = hi * adj),
                fill = "red",
                alpha = 0.5,
                linetype = "dotted",
                color = "red",
                linewidth = 0.75) +
    geom_line(aes(x = b,
                  y = med * adj),
              linewidth = 1.5,
              color = "red") +
    geom_segment(data = rb,
                 aes(x = lo_b,
                     xend = hi_b,
                     y = med_r,
                     yend = med_r,
                     color = yr),
                 linewidth = 0.5) +
    geom_segment(data = rb,
                 aes(x = med_b,
                     xend = med_b,
                     y = lo_r,
                     yend = hi_r,
                     color = yr),
                 linewidth = 0.5) +
    geom_point(data = rb,
               aes(x = med_b,
                   y = med_r),
               size = 3) +
    geom_point(data = rb,
               aes(x = med_b,
                   y = med_r,
                   color = yr),
               size = 2) +
    scale_color_manual(values = colvec) +
    geom_text_repel(data = rb,
                    aes(x = med_b,
                        y = med_r,
                        label = yr),
                    max.overlaps = 100) +
    geom_hline(yintercept = 1,
               linetype = "dashed") +
    geom_hline(yintercept = adj,
               linetype = "dashed",
               color = "red") +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    coord_cartesian(xlim = xlim,
                    ylim = ylim) +
    xlab("Relative spawning biomass") +
    ylab(expression(paste("Recruitment relative to unfished equilibrium (",
                          R[0],
                          ")"))) +
    theme(legend.position = "none",
          axis.text.x = element_text(color = axis_label_color,
                                     size = axis_tick_font_size,
                                     angle = 0,
                                     hjust = 0.5,
                                     vjust = -0.25,
                                     face = "plain"),
          axis.text.y = element_text(color = axis_label_color,
                                     size = axis_tick_font_size,
                                     hjust = 1,
                                     vjust = 0.5,
                                     face = "plain"),
          axis.title.x = element_text(color = axis_label_color,
                                      size = axis_title_font_size,
                                      angle = 0,
                                      vjust = 0,
                                      face = "plain"),
          axis.title.y = element_text(color = axis_label_color,
                                      size = axis_title_font_size,
                                      angle = 90,
                                      face = "plain"),
          # plot.margin: top, right,bottom, left
          plot.margin = margin(0, 0, 6, 6))

  # Right-hand plot - dlnorm distribution (right panel)
  y_breaks <- c(0, adj, 1)
  y_labels <- c(0, 1, 2.5)
  # Expansion (space between bottom of plot and zero) that will be applied
  # to the right hand plot because the left-hand plot is set to default (5%).
  # This is so they line up correctly
  expansion <- (ylim[2] - ylim[1]) * 0.05
  d <- tibble(r_vec = r_vec,
              dlnorm_vec = dlnorm_vec)

  p[[2]]<- ggplot(d) +
    geom_ribbon(aes(y = r_vec,
                    xmin = 0,
                    xmax = dlnorm_vec),
                fill = "grey") +
    geom_segment(aes(x = 0,
                     xend = dlnorm(x = adj,
                                   meanlog = meanlog,
                                   sdlog = sigma_r),
                     y = adj,
                     yend = adj),
                 color = "red") +
    geom_segment(aes(x = 0,
                     xend = dlnorm(x = 1,
                                   meanlog = meanlog,
                                   sdlog = sigma_r),
                     y = 1,
                     yend = 1)) +
    scale_y_continuous(breaks = y_breaks,
                       labels = y_labels,
                       expand = c(0, expansion)) +
    scale_x_continuous(expand = c(0, 0)) +
    xlab("") +
    ylab("Recruitment (billions)") +
    annotate("text",
             label = "Expected distribution of absolute recruitments",
             x = 0.2,
             y = 4.5,
             # The .pt is multiplied automatically by [grid::textGrob()]
             # for text annotations so we "undo" that here by dividing by it
             size = axis_title_font_size / .pt,
             angle = 90) +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_blank(),
          axis.title.y = element_text(color = axis_label_color,
                                      size = axis_title_font_size,
                                      angle = 90,
                                      vjust = -3,
                                      face = "plain"),
          axis.text.y = element_text(color = axis_label_color,
                                     size = axis_tick_font_size,
                                     angle = 0,
                                     hjust = 0.5,
                                     face = "plain"))

  plot_grid(plotlist = p, rel_widths = c(5, 1), align = "h")
}