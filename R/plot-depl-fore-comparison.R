#' Plot the relative spawning biomass with several forecast trajectories
#'
#' @rdname plot_biomass
#' @param fore_inds Indices of the `model$catch_levels` list to include as
#' forecast catch streams in the plot
#' @param forecast_yrs A vector of the forecast years
#' @export
plot_depl_fore_comparison <- function(model,
                                      fore_inds = c(1, 2, 6, 12, 14),
                                      xlim = c(max(forecast_yrs) - 16,
                                               max(forecast_yrs)),
                                      x_breaks = xlim[1]:xlim[length(xlim)],
                                      ylim = c(0, 3.5),
                                      y_breaks = c(
                                        0,
                                        0.1,
                                        0.4,
                                        0.5,
                                        1.0,
                                        seq(1.5,
                                            ylim[2],
                                            by = 0.5)),
                                      y_labels = expression(
                                        "0",
                                        "0.1B"[0],
                                        "0.4B"[0],
                                        "0.5",
                                        "B"[0],
                                        "1.5",
                                        "2",
                                        "2.5",
                                        "3",
                                        "3.5"),
                                      y_colors = c(
                                        "black",
                                               "red",
                                               "green",
                                               "black",
                                               "blue",
                                               rep("black", length(seq(1.5,
                                                                       ylim[2],
                                                                       by = 0.5)))),
                                      alpha = 0.2,
                                      leg_pos = c(0.15, 0.83),
                                      leg_ncol = 1,
                                      leg_font_size = 12,
                                      axis_title_font_size = 14,
                                      axis_tick_font_size = 11,
                                      forecast_yrs){

  nice_nms <- map_chr(model$ct_levels[fore_inds], ~{
    .x[[2]]
  })

  fore_len <- length(model$forecasts)
  fore_models <- model$forecasts[[fore_len]][fore_inds]
  names(fore_models) <- nice_nms
  extract_depval <- function(nm, inc_fore_yr = FALSE){
    tib <- map(fore_models, ~{
      tmp <- .x$mcmccalcs[[nm]]
      nms <- names(tmp)
      wch_yrs <- which(!is.na(suppressWarnings(as.numeric(nms))))
      tmp[wch_yrs]
    }) %>%
      do.call(rbind, .) |>
      as_tibble(rownames = "model") |>
      select(model, everything()) |>
      pivot_longer(-model, names_to = "year", values_to = nm)
    if(inc_fore_yr){
      tib
    }else{
      tib |> select(-model, -year)
    }
  }

  nrow_out <- length(unique(extract_depval("dmed", inc_fore_yr = TRUE)$year))
  nice_nms_ordered <- map(nice_nms, ~{rep(.x, nrow_out)}) |> unlist()

  fore <- bind_cols(extract_depval("dmed", inc_fore_yr = TRUE),
                    extract_depval("dlower"),
                    extract_depval("dupper")) |>
    mutate(model = factor(model, levels = nice_nms))

  fore_future <- fore |>
    mutate(year = as.numeric(year)) |>
    filter(year >= min(forecast_yrs))

  historic <- fore |>
    mutate(year = as.numeric(year)) |>
    filter(model == nice_nms[length(nice_nms)],
           !year %in% forecast_yrs[-1])

  g <- ggplot(fore_future,
              aes(fill = model,
                  color = model,
                  group = model,
                  x = year,
                  y = dmed,
                  ymin = dlower,
                  ymax = dupper)) +
    scale_fill_manual(values = plot_color(length(fore_inds))) +
    scale_color_manual(values = plot_color(length(fore_inds))) +
    coord_cartesian(xlim = xlim,
                    ylim = ylim) +
    geom_point() +
    geom_line() +
    geom_ribbon(alpha = alpha,
                linetype = "dashed") +
    geom_point(data = historic) +
    geom_line(data = historic) +
    geom_ribbon(data = historic,
                alpha = alpha,
                linetype = "dashed") +
    geom_rect(aes(xmin = forecast_yrs[1],
                  xmax = forecast_yrs[length(forecast_yrs)],
                  ymin = 0,
                  ymax = Inf),
              alpha = 0.01,
              fill = "#D2D2D2") +
    geom_hline(yintercept = 0.1,
               linetype = "dotted",
               color = "red",
               size = 1) +
    geom_hline(yintercept = 0.4,
               linetype = "dotted",
               color = "green",
               size = 1) +
    geom_hline(yintercept = 1,
               linetype = "dotted",
               color = "blue",
               size = 1) +
    scale_x_continuous(expand = c(0, 0),
                       breaks = x_breaks,
                       labels = x_breaks) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       labels = y_labels) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = leg_font_size),
          # Uncomment this to have colors for B0 text labels
          #axis.text.y = element_text(color = y_colors),
          # plot.margin: top, right,bottom, left
          # Needed to avoid tick labels cutting off
          plot.margin = margin(12, 12, 0, 0)) +
    guides(fill = guide_legend(ncol = leg_ncol,
                               label.hjust = 0),
           color = guide_legend(ncol = leg_ncol,
                                label.hjust = 0)) +
    xlab("Year") +
    ylab("Relative Spawning Biomass")

  g <- g +
    theme(axis.text.x = element_text(color = "grey20",
                                     size = axis_tick_font_size,
                                     angle = 0,
                                     hjust = 0.5,
                                     vjust = -0.25,
                                     face = "plain"),
          axis.text.y = element_text(color = "grey20",
                                     size = axis_tick_font_size,
                                     hjust = 1,
                                     vjust = 0.5,
                                     face = "plain"),
          axis.title.x = element_text(color = "grey20",
                                      size = axis_title_font_size,
                                      angle = 0,
                                      vjust = 0,
                                      face = "plain"),
          axis.title.y = element_text(color = "grey20",
                                      size = axis_title_font_size,
                                      angle = 90,
                                      face = "plain"),
          axis.ticks.length = unit(0.15, "cm"))

  if(!is.null(leg_pos)){
    g <- g +
      theme(legend.position = leg_pos)
  }

    g
}
