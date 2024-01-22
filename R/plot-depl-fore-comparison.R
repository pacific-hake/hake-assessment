#' Plot the relative spawning biomass with several forecast trajectories
#'
#' @rdname plot_biomass
#' @param fore_inds Indices of the `model$ct_levels` list to include as
#' forecast catch streams in the plot
#' @param forecast_yrs A vector of the forecast years
#' @export
plot_depl_fore_comparison <- function(
    model,
    fore_inds = c(1, # Zero catch
                  2, # 180,000 t catch
                  model$ct_levels_vals$ct_actual_ind,
                  model$ct_levels_vals$ct_tac_ind,
                  model$ct_levels_vals$ct_default_policy_ind),
    xlim = c(max(forecast_yrs) - 16,
             max(forecast_yrs) - 1),
    x_breaks = xlim[1]:xlim[length(xlim)],
    x_labs_mod = 5,
    x_expansion = 1,
    tick_prop = 1,
    vjust_x_labels = -2,
    ylim = c(0, 2),
    y_breaks = c(0, 0.1, 0.4, 0.5, 1, 1.5, 2),
    y_labels = expression(
      "0",
      "0.1B"[0],
      "0.4B"[0],
      "0.5",
      "B"[0],
      "1.5",
      "2"),
    y_colors = c(
      "black",
      "red",
      "green",
      "black",
      "blue",
      "black",
      "black"),
    alpha = 0.2,
    leg_pos = c(0.15, 0.83),
    leg_ncol = 1,
    leg_font_size = 12,
    forecast_yrs){

  nice_nms <- map_chr(model$ct_levels[fore_inds], ~{
    .x[[2]]
  })

  fore_len <- length(model$forecasts)
  fore_models <- model$forecasts[[fore_len]][fore_inds]
  names(fore_models) <- nice_nms
  extract_depval <- function(nm, inc_fore_yr = FALSE){
    tib <- map(fore_models, ~{
      tmp <- .x$depl[rownames(.x$depl) == nm, ]
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

  nrow_out <- length(unique(extract_depval("50%", inc_fore_yr = TRUE)$year))
  nice_nms_ordered <- map(nice_nms, ~{rep(.x, nrow_out)}) |> unlist()

  fore <- bind_cols(extract_depval("50%", inc_fore_yr = TRUE),
                    extract_depval("5%"),
                    extract_depval("95%")) |>
    mutate(model = factor(model, levels = nice_nms))

  fore_future <- fore |>
    mutate(year = as.numeric(year)) |>
    filter(year >= min(forecast_yrs))

  historic_med <- enframe(model$mcmccalcs$dmed)
  historic_lo <- enframe(model$mcmccalcs$dlower)
  historic_hi <- enframe(model$mcmccalcs$dupper)
  historic <- historic_lo |>
    full_join(historic_med, "name") |>
    full_join(historic_hi, "name") |>
    mutate(name = as.numeric(name))
  names(historic) <- c("year", "5%", "50%", "95%")
  # Remove forecast years from historic
  years_rm_historic <- sort(unique(fore_future$year))[-1]
  historic <- historic |>
    filter(!year %in% years_rm_historic)
  # Add model column to historic
  historic <- historic |>
    mutate(model = tail(levels(fore_future$model), 1)) |>
    mutate(model = factor(model)) |>
    select(model, everything())
  # Replace last year of historic with first year of projection
  year_repl_historic <- model$endyr + 1
  # Get the last year of historic and modify it
  historic <- historic |>
    filter(year != year_repl_historic)
  first_yr_row <- fore_future |>
    filter(year == year_repl_historic,
           model == tail(levels(fore_future$model), 1))
  historic <- historic |>
    bind_rows(first_yr_row) |>
    filter(year %in% xlim[1]:xlim[2])

  x_labels <- make_major_tick_labels(x_breaks = x_breaks,
                                     modulo = x_labs_mod)

  g <- ggplot(fore_future,
              aes(fill = model,
                  color = model,
                  group = model,
                  x = year,
                  y = `50%`,
                  ymin = `5%`,
                  ymax = `95%`)) +
    scale_fill_manual(values = plot_color(length(fore_inds))) +
    scale_color_manual(values = plot_color(length(fore_inds))) +
    coord_cartesian(xlim = xlim,
                    ylim = ylim,
                    clip = "off") +
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
    scale_x_continuous(expand = c(0, x_expansion),
                       breaks = x_breaks,
                       labels = x_labels) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       labels = y_labels) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = leg_font_size),
          # These two commands move the x-axis major tick labels and axis
          # title down so that the ticks. tick labels, and axis title don't
          # overlap each other
          axis.text.x = element_text(vjust = vjust_x_labels),
          axis.title.x = element_text(vjust = vjust_x_labels)) +
    guides(fill = guide_legend(ncol = leg_ncol,
                               label.hjust = 0),
           color = guide_legend(ncol = leg_ncol,
                                label.hjust = 0)) +
    xlab("Year") +
    ylab("Relative Spawning Biomass")

  # Add major tick marks
  g <- g |>
    add_major_ticks(x_breaks = x_breaks,
                    modulo = x_labs_mod,
                    # This proportion must be set by trial and error
                    # Make sure to change `vjust` value above in the `theme()`
                    # call so the labels are not overlapping the lines or
                    # too far away from the lines
                    prop = tick_prop)

  if(!is.null(leg_pos)){
    g <- g +
      theme(legend.position = leg_pos)
  }

  g
}
