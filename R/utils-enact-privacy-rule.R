#' Apply the confidentiality rule to spatial data. Copied from the [gfplot]
#' package
#'
#' @details
#' Removes data which does not satisfy the confidentiality rules; the minimum
#' number of vessels with catch in a specific area
#'
#' @param dat Data from [gfdata::get_cpue_spatial()],
#' [gfdata::get_cpue_spatial_ll()], or [gfdata::get_catch_spatial()]
#' @param bin_width Bin width as defined in [ggplot2::stat_summary_hex()]
#' @param n_minimum_vessels The minimum number of vessels that must be present
#' in a hexagon for the hexagon to be shown
#' @param x_lim The length-2 vector representing the minimum and maximum
#' limits of the x-axis
#' @param y_lim The length-2 vector representing the minimum and maximum
#' limits of the y-axis
#'
#' @return A list containing 1) the data frame `dat` but with rows removed that
#' do not fulfill the privacy limitations, 2) The total number of fishing
#' events removed due to privacy limitations, and 3) The total number of
#' fishing events in `dat`
#'
#' @export
enact_privacy_rule <- function(dat,
                               bin_width,
                               n_minimum_vessels,
                               x_lim,
                               y_lim) {
  # count unique vessels per hexagon cell for privacy:

  # Fake data to make sure that the hexagons overlap perfectly.
  # This extends the X and Y to extreme but identical limits every time.
  rows <- dat |>
    slice(1:2) |>
    mutate(fishing_event_id = c(-999L, -998L),
           vessel_registration_number = c(-999L, -998L),
           X = c(-1000, 20000),
           Y = c(-1000, 20000))
  dat <- dat |>
    bind_rows(rows)

  # Count number of vessels per hexagon cell
  g_vessel_count <- dat |>
    ggplot(aes(X, Y)) +
    coord_equal(xlim = x_lim,
                ylim = y_lim) +
    stat_summary_hex(aes(x = X,
                         y = Y,
                         z = vessel_registration_number),
                     binwidth = bin_width,
                     fun = \(x)length(unique(x)))

  # Count fishing events per hexagon cell
  g_fe_id_count <- dat |>
    ggplot(aes(X, Y)) +
    coord_equal(xlim = x_lim,
                ylim = y_lim) +
    stat_summary_hex(aes(x = X,
                         y = Y,
                         z = fishing_event_id),
                     binwidth = bin_width,
                     fun = \(x)length(unique(x)))

  # The actual CPUE hexagon binning:
  bin_func <- \(x)exp(mean(log(x), na.rm = FALSE))

  g <- dat |>
    ggplot(aes(X, Y)) +
    coord_equal(xlim = x_lim,
                ylim = y_lim) +
    stat_summary_hex(aes(x = X,
                         y = Y,
                         z = cpue),
                     binwidth = bin_width,
                     fun = bin_func)

  # enact the privacy rule:
  gdat <- ggplot_build(g)$data[[1]] |>
    as_tibble()
  gdat_count <- ggplot_build(g_vessel_count)$data[[1]] |>
    as_tibble()
  gdat_fe_id_count <- ggplot_build(g_fe_id_count)$data[[1]] |>
    as_tibble()

  # Sanity check:
  # Number of hexagon cells for vessel count and CPUE or Catch didn't match.
  # Stopping because the privacy rule might not remain valid in this case.
  stopifnot(identical(nrow(gdat), nrow(gdat_count)))
  stopifnot(identical(nrow(gdat), nrow(gdat_fe_id_count)))

  # Remove hexagons with not enough vessels
  gdat <- gdat |>
    dplyr::filter(gdat_count$value >= n_minimum_vessels)

  # Get hexagons with not enough fishing events
  lost_fe_id_df <- gdat_fe_id_count |>
    dplyr::filter(gdat_count$value < n_minimum_vessels)

  lost_fe_ids <- sum(lost_fe_id_df$value)
  total_fe_ids <- sum(gdat_fe_id_count$value)

  list(data = gdat,
       lost_fe_ids = lost_fe_ids,
       total_fe_ids = total_fe_ids)
}
