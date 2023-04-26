plot_sample_size_weight_at_age_heatmap <- function(
    model,
    sample_size_df = NULL,
    fleet = 1,
    proj_line_color = "royalblue",
    proj_line_width = 1,
    proj_line_yr = NULL,
    yrs = NULL,
    cell_font_size = 4,
    axis_title_font_size = 14,
    axis_tick_font_size = 11,
    ...){

  stopifnot(!is.null(sample_size_df))

  # Set up years ----
  start_yr <- model$startyr
  end_yr <- model$endyr
  # First year in the weight-at-age data
  first_yr <- model$wtatage |>
    as_tibble() |>
    filter(Yr > 0) |>
    pull(Yr) |>
    min()

  # Configure weight-at-age data frame ----
  wa <- heatmap_extract_wa(model, fleet, ...)

  # Configure boldface mask data frame (projected years) ----
  bf <- heatmap_extract_sample_size(sample_size_df, fleet, wa)

  # At this point, `bf`and `wa` have identical dimensions, and the `bf`
  # data frame will contain only `TRUE` or `FALSE` in each cell (except the
  # `yr` column) signifying whether or not the text should be boldface in the
  # respective cell

  # Move weight-at-age data into `ggplot` (long) format ----
  w <- bf |>
    pivot_longer(-yr, names_to = "age" ) |>
    mutate(age = factor(age,
                        levels = c(names(wa)[names(wa) != "yr"], "sum"))) |>
    mutate(yr = as.integer(yr)) |>
    group_by(age) |>
    mutate(rescale = rescale(value)) |>
    ungroup()

  # Set colors for the heatmap cells, 1 for each age
  colors <- heatmap_set_colors(w, ...)

  y_breaks <- wa$yr
  y_labels <- y_breaks
  # Set up the bottom row, which contains the mean of the values
  y_labels[1] <- "sum"
  y_breaks[2] <- NA
  y_labels[2] <- ""

  # Set 1965 to colorless. Need a second value column, for a character version
  # to make the plot work right (avoids Error: Discrete value supplied to
  # continuous scale)
  w <- w |>
    mutate(value_text = ifelse(yr == 1965, "", value),
           rescale = ifelse(yr == 1965, 0, rescale))

  g <- ggplot(w,
              aes(x = age,
                  y = yr)) +
    scale_y_continuous(breaks = y_breaks,
                       labels = y_labels,
                       expand = c(0, 0)) +
    geom_tile(aes(alpha = rescale,
                  fill = value)) +
    scale_fill_gradientn(colors = colors,
                         guide = FALSE) +
    geom_text(aes(label = value_text), size = cell_font_size) +
    scale_alpha(range = c(0.1, 1)) +
    theme(legend.position = "none",
          plot.margin = margin(12, 12, 10, 0)) +
    geom_hline(yintercept = c(first_yr - 0.5,
                              end_yr + 0.5),
               color = proj_line_color,
               size = proj_line_width) +
    xlab("Age") +
    ylab("Year") +
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
                                      face = "plain"))

  g
}
