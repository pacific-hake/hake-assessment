#' Create a weight-at-age heatmap including extrapolated years
#'
#' @details
#' Produce a matrix of colors and values of weight-at-age information
#' by year (y-axis) and age (x-axis). The plot uses output from a
#' Stock Synthesis model.
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
#' Positive values for fleet will link to a modeled fleet.
#' @param proj_line_color Line color to separate projection years.
#' @param proj_line_width Line width to separate projection years.
#' @param yrs A vector of the years to include in the table. If `NULL`, all
#' years will be included
#' @param sample_size_df A data frame where ages are columns (and start with
#' the letter 'a'). If the values are zero, the weight-at-age was
#' extrapolated/interpolated. If there is a value, the weight-at-age is data
#' @param cell_font_size Font size of the values printed in each cell of
#' the table
#' @param axis_title_font_size Size of the font for the X and Y axis labels
#' @param axis_tick_font_size Size of the font for the X and Y axis tick
#' labels
#'
#' @return A [ggplot2::ggplot()] object
#' @export
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

  g0 <- plot_weight_at_age_heatmap(model = model,
                                   sample_size_df = sample_size_df)

  g1 <- ggplot_build(g0)
  # `fill_cols` is a vector of all the colors in the heatmap tiles
  fill_cols <- g1$data[[1]]$fill

  ggplot_map_pos_data <- g1$layout$map_position(g1$data)
  # `mappos` contains the color, x (age), and y (yr) columns `ggplot`
  # used to build the plot`

  # Configure weight-at-age data frame ----
  wa <- heatmap_extract_wa(model,
                           fleet,
                           ...)

  ages <- names(wa) %>%
    grep("^\\d+$", ., value = TRUE) |>
    as.numeric()

  max_age <- max(ages)
  num_ages <- length(ages)

  mappos <- ggplot_map_pos_data[[1]] |>
    as_tibble() |>
    transmute(fill_col = fill,
              alpha_col = alpha,
              age = as.numeric(x),
              yr = y) |>
    # Offset the ages if they start with zero
    mutate(age = ifelse(is.na(age), "sum", ages[age])) |>
    mutate(age = factor(as.numeric(age),
                        levels = levels(g0$data$age)))

  # Configure sample size data frame ----
  s_size <- heatmap_extract_sample_size(sample_size_df,
                                        fleet,
                                        wa) |>
    # Rename the `sum` column to a number so that the `as.numeric(age)`
    # performed on the call below (`ss <- s_size ...`)` does not fail
    # due to the character string `sum`
    # The number is renamed back to `sum` later in the plotting
    rename(`999` = sum)

  # Convert to long form for `ggplot` plotting.
  ss <- s_size |>
    pivot_longer(-yr,
                 names_to = "age",
                 values_to = "sample_size") |>
    mutate(age = as.numeric(age)) |>
    mutate(age = factor(age)) |>
    full_join(mappos, by = c("yr", "age")) |>
    mutate(sample_size = f(sample_size))

  # Set colors for the sum text colors
  cols = c("red",
           "yellow",
           "green",
           "dodgerblue")
  col_func <- colorRampPalette(cols)
  sum_colors <- col_func(num_ages- 1)
  browser()
  gt <- ggplot(ss |> filter(age == 999),
               aes(x = age,
                   y = yr)) +
    geom_tile() +
    geom_text(aes(label = sample_size), size = 4) +
    scale_color_gradientn(colors = sum_colors)



  ss <- ss |>
    # Make new column for the two types, age columns and the sum column
    mutate(fill_col = ifelse(age == 999, "black", fill_col)) |>
    mutate(color_col = ifelse(age == 999, sum_colors, "black")) |>
    mutate(alpha_col = ifelse(age == 999, 1, alpha_col))


  g <- ggplot(ss,
              aes(x = age,
                  y = yr,
                  color = color_col,
                  fill = fill_col)) +
    geom_raster(aes(alpha = alpha_col), hjust = 0) +
    scale_alpha(range = c(0.1, 1)) +
    geom_text(aes(label = sample_size),
              size = 4,
              hjust = "right") +
    scale_color_identity() +
    scale_fill_identity()

g
}