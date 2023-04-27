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

  p <- plot_weight_at_age_heatmap(model = model,
                                  sample_size_df = sample_size_df)

  g <- ggplot_build(p)
  # `fill_cols` is a vector of all the colors in the
  fill_cols <- g$data[[1]]$fill

  ggplot_map_pos_data <- g$layout$map_position(g$data)
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
    # Offset the ages if they starts with zero
    mutate(age = ifelse(is.na(age), "sum", ages[age])) |>
    mutate(age = factor(as.numeric(age),
                        levels = levels(p$data$age)))

  # Configure boldface mask data frame ----
  s_size <- heatmap_extract_sample_size(sample_size_df,
                                        fleet,
                                        wa)
  ss <- s_size |>
    pivot_longer(-yr, names_to = "age", values_to = "sample_size")

  # `mappos` has the ages as indices, they need to be converted to real ages
  browser()

  j <- ss |>
    full_join(mappos, by = c("yr", "age"))

}