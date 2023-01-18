#' Plot the relative spawning biomass with several forecast trajectories
#'
#' @param model The model to plot as created by [create_rds_file()]
#' @param fore_inds The indices of the forecast lines and ribbons to show.
#' See the file `forecast-catch-levels.R` which contains the list
#' `catch-levels`. The indices match what is in this list
#' @param xlim The year limits to plot
#' @param x_breaks The year value tick marks to show for the x axis
#' @param ylim The depletion limits to plot
#' @param y_breaks The depletion value tick marks to show for the y axis
#' @param y_labels The depletion labels to show for the y axis tick marks
#' @param y_colors The color vector for each label for the y axis tick marks
#' @param alpha The transparency for all ribbons
#' @param leg_pos The position of the legend inside the plot. If `NULL`,
#' the legend will be in its default location outside the plot margin
#' @param leg_font_size The legend font size
#' @param forecast_yrs The `forecast_yrs` vector as defined in the file
#' `all.R`

#' @return a [ggplot2::ggplot()] object
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
                                      leg_font_size = 12,
                                      forecast_yrs){

  nice_nms <- map_chr(model$catch.levels[fore_inds], ~{
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
              fill = "grey50") +
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
          axis.text.y = element_text(color = y_colors),
          # plot.margin: top, right,bottom, left
          # Needed to avoid tick labels cutting off
          plot.margin = margin(12, 12, 0, 0)) +
    xlab("Year") +
    ylab("Relative Spawning Biomass")

  if(!is.null(leg_pos)){
    g <- g +
      theme(legend.position = leg_pos)
  }

    g
}