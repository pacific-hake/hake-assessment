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
#' @param extrap_mask A data frame where ages are columns (and start with
#' the letter 'a'). If the values are zero, the weight-at-age was
#' extrapolated/interpolated. If there is a value, the weight-at-age is data
#' @param longterm_mean_ages A vector of mean weight-at-age values
#' per ages zero to the maximum age of fish in the data.
#' If `NULL`, the first year will be assumed to be the mean  of all years.
#' @param font_size Font size of the values printed in each box.
#' @param axis_font_size Font size for axis labels.
#' @param samplesize A logical value specifying if the heat map should be of
#' input sample size used to generate the data rather than the weights-at-age.
#' @param print_yrs A vector of years that will be included in the output,
#' but users should note that all data from `first_yr` to the terminal
#' year of the data in the model will be used to calculate means and colors.
#' This parameter facilitates splitting the plot into two separate plots when
#' there are a number of years.
#' @param colour A character string of `both`, `all`, or `age`. If the
#' default of `both`, then the colors of the boxes will be based on all of
#' the data and the transparency of the colors will strictly be a function
#' of each individual age in turn. That is, the darkness of the colors
#' used across years for age zero specifies which years have the largest
#' age-0 fish. `all` specifies colors based on the min and max values of
#' all ages without transparency. `age` colors the min and max color
#' specific for each age without transparency
#' @param start_yr Start year
#' @param end_yr End year
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_weight_at_age_heatmap <- function(
    model,
    fleet = 1,
    proj_line_color = "royalblue",
    proj_line_width = 1,
    proj_line_yr = NULL,
    yrs = NULL,
    extrap_mask = NULL,
    font_size = 4,
    axis_font_size = 10,
    samplesize = FALSE,
    print_yrs = NULL,
    colour = c("all", "age", "both"),
    ...){

  colour <- match.arg(colour)

  stopifnot(!is.null(extrap_mask))

  # Set up years ----
  start_yr <- model$startyr
  end_yr <- model$endyr
  # First year in the weight-at-age data
  first_yr <- model$wtatage |>
    as_tibble() |>
    filter(Yr > 0) |>
    pull(Yr) |>
    min()

  # Years greater than `proj_line_yr` are projected, a line will appear on
  # the plot showing this
  input_yrs <- first_yr:end_yr
  if(is.null(proj_line_yr[1])){
    proj_line_yr <- end_yr
  }

  # Configure weight-at-age data frame ----
  wa <- heatmap_extract_wa(model, fleet, ...)

  # Configure boldface mask data frame (projected years) ----
  bf <- heatmap_extract_bf(extrap_mask, fleet, wa)


  # At this point, `bf`and `wa` have identical dimensions, and the `bf`
  # data frame will contain only `TRUE` or `FALSE` in each cell (except the
  # `yr` column) signifying whether or not the text should be boldface in the
  # respective cell

  min_val <- min(wa[,-1])
  max_val <- max(wa[,-1])

  # Move weight-at-age data into `ggplot` (long) format ----
  w <- wa |>
    pivot_longer(-yr, names_to = "age" ) |>
    mutate(age = as.numeric(age)) |>
    mutate(age = factor(age)) |>
    mutate(yr = as.integer(yr)) |>
    group_by(age) |>
    mutate(rescale = rescale(value)) |>
    ungroup()

  # Append the `isbold` toggle to the new long data frame ----
  b <- bf |>
    pivot_longer(-yr, names_to = "age", values_to = "isbold")

  w <- full_join(w, b, by = c("yr", "age")) |>
    mutate(age = factor(age,
                        levels = sort(unique(as.numeric(age)))))

  # Set colors for the heatmap cells
  ages <- as.numeric(levels(unique(w$age)))
  nage <- length(ages)
  seed_cols <- c("red", "yellow", "green", "dodgerblue")
  ncols <- nage - 1
  col_func <- colorRampPalette(seed_cols)
  colors <- col_func(nage - 1)

  y_breaks <- wa$yr

  # Set 1965 to colorless
  w <- w |>
    mutate(value = ifelse(yr == 1965, NA, value),
           rescale = ifelse(yr == 1965, NA, rescale),
           isbold = ifelse(yr == 1965, FALSE, isbold))
browser()
  g <- ggplot(w,
              aes(x = age,
                  y = yr,
                  fontface = ifelse(isbold, "bold", "plain"))) +
    scale_y_continuous(breaks = y_breaks) +
    geom_tile(aes(alpha = ifelse(is.na(rescale), 0, rescale),
                  fill = ifelse(is.na(value), NA, value))) +
    scale_fill_gradientn(colors = colors,
                         guide = FALSE) +
    geom_text(aes(label = f(value, 2)), size = 4) +
    scale_alpha(range = c(0.1, 1)) +
    theme(legend.position = "none") +
    geom_hline(yintercept = c(first_yr - 0.5,
                              end_yr + 0.5),
               color = proj_line_color,
               size = proj_line_width)

  g
}