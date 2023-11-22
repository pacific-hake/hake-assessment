#' Create a weight-at-age 3D heatmap with sample size representing the height
#' of the bars and colros representing the weight-at-age values
#'
#' @details
#' Produce a matrix of colors and values of weight-at-age information
#' by year (y-axis) and age (x-axis). The z-axis (bar heights0 represents the
#' sample sizes for those year/age combinations.
#'
#' Ages zero through the maximum age modeled in the data are shown
#' for each year plotted. This maximum-age shown is found by removing ages
#' that are the duplicate of the previous age, no user input is needed.
#'
#' Users have many options to control what years are included in the plot.
#' The beginning years of data can be removed using `first_yr`.
#' Users do not have control over what are noted as projection years, which
#' are all years after the end of the data included in the assessment model
#' up to last year included in the weight-at-age matrix.
#' These projection years are noted using a horizontal line but if the
#' demarcation is not wanted, then they can change the line width to zero.
#' Finally, users can calculate the colors using a range of data specified
#' with `first_yr` and the last year of the data in the model. Then,
#' the resulting plot can be truncated to a specified year range using
#' `print_yrs`. The truncation is helpful to facilitate plots that fit
#' on a single page or on a slide with readable values. Using this truncation,
#' the colors would show trends across all plots made rather than just the
#' truncated data.
#'
#' @param model An list of results read in from an SS model using
#' [load_ss_files()]
#' @param fleet An integer value specifying which fleet you want plotted.
#' Fleet -2 will plot fecundity information.
#' Fleet -1 will plot population weight-at-age for the middle of the year.
#' Fleet 0 will plot population weight-at-age for the beginning of the year.
#' Positive values for fleet will link to a modelled fleet.
#' @param proj_line_color Line color to separate projection years.
#' @param proj_line_width Line width to separate projection years.
#' @param yrs A vector of the years to include in the table. If `NULL`, all
#' years will be included
#' @param sample_size_df A data frame where ages are columns (and start with
#' the letter 'a'). If the values are zero, the weight-at-age was
#' extrapolated/interpolated. If there is a value, the weight-at-age is data
#' @param cell_font_size Font size of the values printed in each cell of
#' the table
#' @param ... Arguments passed to `[heatmap_add_extrap_yrs_wa()] and
#' [heatmap_calc_function()]
#'
#' @return The output of [rayshader::render_snapshot()], which is typically
#' [grid::grid.raster()] or [grid::rasterGrob()]
#' @export
plot_3d_heatmap <- function(
    model,
    sample_size_df = NULL,
    fleet = 1,
    proj_line_color = "royalblue",
    proj_line_width = 1,
    proj_line_yr = NULL,
    yrs = NULL,
    cell_font_size = 4,
    ...){

  stopifnot(!is.null(sample_size_df))

  # Extract valid weight-at-age data frame for given fleet ----
  wa <- model$wtatage |>
    as_tibble() |>
    filter(Fleet == fleet) %>%
    select(Yr, matches("^\\d", .)) |>
    rename(yr = Yr) |>
    filter(yr > 0)

  # Model start and end years ----
  start_yr <- model$startyr
  end_yr <- model$endyr
  # First year in the weight-at-age data ----
  first_yr <- min(wa$yr)

  # Complete the weight-at-age data frame with pre- and post- years ----
  wa <- heatmap_add_extrap_yrs_wa(model = model,
                                  wa = wa,
                                  ...)

  # Configure sample size data frame ----
  # `wa` is an argument and is used to make sure the sample size data frame
  # `s_size` has the same dimensions as `wa` so the fill colors fit in
  #  properly
  s_size <- heatmap_extract_sample_size(sample_size_df,
                                        fleet,
                                        wa)

  # Convert data frame to long form for `ggplot` plotting.
  ss <- s_size |>
    pivot_longer(-yr,
                 names_to = "age",
                 values_to = "sample_size") |>
    mutate(age = as.numeric(age)) |>
    mutate(age = factor(age)) |>
    group_by(age) |>
    mutate(sample_size_rescaled = rescale(sample_size)) |>
    ungroup()

  ss <- ss |>
    filter(!is.na(sample_size))

  # At this point, `bf`and `wa` have identical dimensions, and the `bf`
  # data frame will contain only `TRUE` or `FALSE` in each cell (except the
  # `yr` column) signifying whether or not the text should be boldface in the
  # respective cell

  # Calculate the mean row for the bottom of the heatmap, overwrite the
  # row that is there already
  mean_row <- heatmap_calc_function(wa |>
                                      filter(yr %in% start_yr:end_yr),
                                    func = mean,
                                    ...) |>
    vec2df() |>
    mutate(yr = min(wa$yr)) |>
    select(yr, everything())
  # `rows_update` matches the first element (`yr`) by default and
  #  overwrites that row with the `mean` row
  wa <- wa |>
    rows_update(mean_row)

  # Set colors for the heatmap cells, 1 for each age
  colors <- heatmap_set_colors(wa, ...)

  # Move weight-at-age data into `ggplot` (long) format ----
  # Rescale the value column so it is between 0 and 1, which is required
  # to create a color gradient using `scale_fill_gradientn()``
  w <- wa |>
    pivot_longer(-yr, names_to = "age" ) |>
    mutate(age = as.numeric(age)) |>
    mutate(age = factor(age)) |>
    mutate(yr = as.integer(yr)) |>
    rename(waa = value) |>
    group_by(age) |>
    mutate(waa_rescaled = rescale(waa)) |>
    ungroup()

  w <- w |>
    full_join(ss, by = c("yr", "age")) |>
    mutate(age = factor(age,
                        levels = sort(unique(as.numeric(age))))) |>
    filter(!is.na(age))

  # Set up the y-axis tick mark frequency (one for every year)
  y_breaks <- wa$yr
  y_labels <- y_breaks
  # Set up the bottom row, which contains the mean of the values and
  # remove tick and label from the line above it for the blank line
  y_labels[1] <- "Mean"
  y_breaks[2] <- NA
  y_labels[2] <- ""

  # Set second year to colorless. Need a second value column,
  # which is a character column instead of numeric to make the plot
  # work right (avoids Error: Discrete value supplied to continuous scale)
  second_yr <- min(wa$yr) + 1
  w <- w |>
    mutate(waa_text = ifelse(yr == second_yr, "", f(waa, 2)),
           waa_rescaled = ifelse(yr == second_yr, 0, waa_rescaled))

  w <- w |>
    filter(yr != 1964)

  g_waa <- ggplot(w,
                  aes(x = age,
                      y = yr,
                      fill = waa,
                      color = sample_size,
                      alpha = waa_rescaled)) +
    scale_y_continuous(breaks = y_breaks,
                       labels = y_labels,
                       expand = c(0, 0)) +
    geom_raster() +
    scale_fill_gradientn(colors = colors) +
    geom_text(aes(label = f(sample_size)),
              size = cell_font_size / .pt,
              fontface = "bold",
              color = "black") +
    scale_alpha(range = c(0.1, 1)) +
    theme(legend.position = "none")

  g_samp <- ggplot(w,
                   aes(x = age,
                       y = yr,
                       fill = sample_size,
                       color = sample_size)) +
    scale_y_continuous(breaks = y_breaks,
                       labels = y_labels,
                       expand = c(0, 0)) +
    # `geom_raster()` is about 4 times faster than `geom_tile()`
    geom_raster() +
    scale_fill_gradientn(colors = colors) +
    theme(legend.position = "none")

  plot_gg(g_waa,
          ggobj_height = g_samp,
          multicore = TRUE,
          width = 5,
          height = 5,
          scale = 100,
          windowsize = c(1400, 866),
          zoom = 0.55,
          phi = 30,
          shadow = TRUE,
          raytrace = TRUE,
          # shadow = FALSE,
          # raytrace = FALSE,
          shadow_intensity = 0.75,
          height_aes = "fill")
  render_camera(zoom = 0.4, theta = 10, phi =30)
  render_snapshot()
  rglwidget()
}