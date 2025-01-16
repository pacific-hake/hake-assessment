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
#' @param sum_col_fill_color The fill color for the row sum column
#' @param ... Arguments passed to `[heatmap_add_extrap_yrs_wa()],
#' [heatmap_get_wa_ggplot_vals()], and [heatmap_set_colors()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_heatmap_sample_size_weight_at_age <- function(
    model,
    sample_size_df = NULL,
    fleet = 1,
    proj_line_color = "royalblue",
    proj_line_width = 1,
    proj_line_yr = NULL,
    yrs = NULL,
    cell_font_size = 4,
    sum_col_fill_color = "white",
    ...){

  sum_col_age_val <- 999

  # Extract valid waa for given fleet ----
  wa <- model$wtatage |>
    as_tibble() |>
    dplyr::filter(fleet == !!fleet) %>%
    select(year, matches("^\\d", .)) |>
    rename(yr = year) |>
    dplyr::filter(yr > 0)

  # Model start and end years ----
  start_yr <- model$startyr
  end_yr <- model$endyr
  # First year in the weight-at-age data
  first_yr <- min(wa$yr)

  # Complete the weight-at-age data frame with pre- and post- years ----
  #
  # This `wa` data frame is used here to get the ages and to set up the
  # dimensions of the sample size data frame, so the `wa` and `sample_size`
  # data frames both have the same dimensions.
  # That ensures that there are exactly the right number of fill colors
  # extracted from the `wa` data frame below to fill in the sample size
  # heatmap without error
  wa <- heatmap_add_extrap_yrs_wa(model = model,
                                  wa = wa,
                                  ...)

  # Extract the actual plotting values used by `ggplot` which include the
  # `fill` and `alpha` values for every cell. Those values will be used to
  # color the sample size heatmap tiles
  map_pos <- heatmap_get_wa_ggplot_vals(model = model,
                                        sample_size_df = sample_size_df,
                                        wa = wa,
                                        fleet = fleet,
                                        col_nms = c("alpha", "fill"),
                                        ...)

  # Configure sample size data frame ----
  # `wa` is an argument and is used to make sure the sample size data frame
  # `s_size` has the same dimensions as `wa` so the fill colors fit in
  #  properly
  s_size <- heatmap_extract_sample_size(sample_size_df,
                                        wa)

  # Convert data frame to long form for `ggplot` plotting.
  ss <- s_size |>
    pivot_longer(-yr,
                 names_to = "age",
                 values_to = "sample_size") |>
    mutate(age = as.numeric(age)) |>
    mutate(age = factor(age)) |>
    full_join(map_pos, by = c("yr", "age"))

  # Set colors for the sum column text
  sum_colors <- heatmap_set_colors(wa, ...)

  # Extract the sum column from the sample size data frame ----
  ss_sum_col <- ss |>
    dplyr::filter(age == sum_col_age_val) |>
    group_by(age) |>
    mutate(rescale = rescale(sample_size)) |>
    ungroup()

  # Create a simple `ggplot` of only the sum text column, and extract the
  # colors that were created by `ggplot`to color the text in the main plot
  gt0 <- ggplot(ss_sum_col,
                aes(x = age,
                    y = yr)) +
    geom_raster(aes(alpha = rescale),
                fill = "transparent") +
    geom_text(aes(label = sample_size,
                  col = rescale(sample_size)),
              size = 4) +
    scale_color_gradientn(colors = sum_colors,
                          guide = FALSE)
  gt1 <- ggplot_build(gt0)
  color_vals <- gt1$data[[2]]$colour

  ss <- ss  |>
    mutate(sample_size = f(sample_size)) |>
    # Make new columns for the two types, age columns and the sum column
    # If `age == sum_col_age_val`, it is the sum column
    mutate(fill_col = ifelse(age == sum_col_age_val,
                             sum_col_fill_color,
                             fill_col)) |>
    mutate(color_col = "black") |>
    mutate(alpha_col = ifelse(age == sum_col_age_val,
                              1,
                              alpha_col))

  # Replace the sum column color from "black" to the colors returned above
  # by `gt0`
  sum_col_df <- ss |>
    dplyr::filter(age == sum_col_age_val) |>
    select(-color_col) |>
    mutate(color_col = color_vals)

  # Add the sum column, and replace all `NA`s in the final table with
  # empty strings
  ss <- ss |>
    bind_rows(sum_col_df) |>
    mutate(sample_size = ifelse(grepl("^\\s*NA$", sample_size),
                                "",
                                sample_size))

  # Set up the x- and y-axis tick marks and associated labels
  x_breaks <- levels(ss$age)
  x_labels <- x_breaks
  # Finally, replace the sum_col_age_val with the "Sum" label
  x_labels[x_breaks == as.character(sum_col_age_val)] <- "Sum"

  y_breaks <- sort(unique(ss$yr))
  y_labels <- y_breaks
  # Set up the bottom row, which contains the mean of the values
  y_labels[1] <- "Sum"
  y_breaks[2] <- NA
  y_labels[2] <- ""

  g <- ggplot(ss,
              aes(x = age,
                  y = yr,
                  color = color_col,
                  fill = fill_col)) +
    # `geom_raster()` is about 4 times faster than `geom_tile()`
    geom_raster(aes(alpha = alpha_col)) +
    scale_alpha(range = c(0.1, 1)) +
    geom_text(aes(label = sample_size),
              size = cell_font_size) +
    scale_color_identity() +
    scale_fill_identity() +
    scale_x_discrete(breaks = x_breaks,
                     labels = x_labels) +
    scale_y_continuous(breaks = y_breaks,
                       labels = y_labels,
                       expand = c(0, 0)) +
    theme(legend.position = "none") +
    geom_hline(yintercept = c(first_yr - 0.5,
                              end_yr + 0.5),
               color = proj_line_color,
               size = proj_line_width) +
    xlab("Age") +
    ylab("Year")

  g
}