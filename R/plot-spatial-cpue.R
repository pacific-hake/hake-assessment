#' Create a map of the west coast of North America showing places of interest
#'
#' @param d Data from [gfdata::get_cpue_spatial()],
#' [gfdata::get_cpue_spatial_ll()], or [gfdata::get_catch_spatial()]
#' @param crs_ll Coordinate Reference System (CRS) number for a
#' latitude/longitude-based projection. This could be NAD83 or WGS84 or
#' others. The default is 4326 which is WGS84: See
#' [Epsg.org](https://epsg.org/home.html) for details. Click `Text search`
#' tab and look at the `code` column for valid crs numbers
#' @param crs_utm Coordinate Reference System (CRS) number for a
#' Easting/Northing-based projection. This could be UTM9 or UTM10 or
#' others. There is a different number for each UTM zone. The default is
#' 32609 which is UTM zone 9 (west coast Canada). See
#' [Epsg.org](https://epsg.org/home.html) for details. Click `Text search`
#' tab and look at the `code` column for valid crs numbers
#' @param x_lim The length-2 vector representing the minimum and maximum
#' limits of the x-axis in degrees of longitude
#' @param y_lim The length-2 vector representing the minimum and maximum
#' limits of the y-axis in degrees of latitude
#' @param bin_width Bin width as defined in [ggplot2::stat_summary_hex()]
#' @param n_minimum_vessels The minimum number of vessels that must be present
#' in a hexagon for the hexagon to be shown
#' @param hex_alpha The transparency value of the CPUE hexagons
#' @param hex_border_color The color of the hexagon border lines
#' @param hex_border_thickness The thickness of the hexagon border lines
#' @param hex_fill_breaks The breaks to show in the color bar that describes
#' the colors of the hexagons. Must be the same length as `hex_fill_labels`
#' @param hex_fill_labels The labels to show in the color bar that describes
#' @param hex_colors A vector of colors to use to make a color ramp palette
#' for the hexagons
#' @param ...
#' the colors of the hexagons. Must be the same length as `hex_fill_breaks`
#' @return A [ggplot2::ggplot()] object
#' @export
plot_spatial_cpue <- function(
    d,
    crs_ll = 4326,
    crs_utm = 32609, # For BC (zone 9)
    #crs_utm = 32610, # For Washington, Oregon, Northern California (zone 10)
    #crs_utm = 32611, # For Southern California (zone 11)
    x_lim = c(-131.25, -124.5),
    y_lim = c(48, 52.5),
    bin_width = 7,
    n_minimum_vessels = 3,
    hex_alpha = 0.5,
    hex_border_color = "black",
    hex_border_thickness = 0.25,
    hex_fill_breaks = c(0, 1000, seq(3000, 14000, 3000)),
    hex_fill_labels = comma(hex_fill_breaks),
    hex_colors = c("antiquewhite",
                   "yellow",
                   "orange",
                   "red"),
    ...){

  g <- plot_map(crs_ll = crs_ll,
                x_lim = x_lim,
                y_lim = y_lim,
                ...)

  # CPUE hexagons ----
  # Need to project both the data and the limits to UTMs here to pass to the
  # `enact_privacy_rule()` function
  d <- d |>
    add_utm_columns()
  lims <- tibble(lon = x_lim,
                 lat = y_lim) |>
    add_utm_columns()

  privacy_out <- enact_privacy_rule(d,
                                    bin_width = bin_width,
                                    n_minimum_vessels = n_minimum_vessels,
                                    x_lim = lims$X,
                                    y_lim = lims$Y)

  public_dat <- compute_hexagon_xy(privacy_out$data,
                                   bin_width = bin_width) |>
    mutate(hex_id = as.numeric(hex_id))

  # The values must be multiplied by 1e3 here to make the UTMs into kilometers.
  # If not done, the limits of the machine will be reached due to too many
  # calculations
  x <- public_dat |>
    mutate(across(c(X, Y), ~{.x <- .x * 1e3})) |>
    st_as_sf(crs = crs_utm, coords = c("X", "Y")) |>
    st_transform(crs_ll) |>
    st_coordinates() |>
    as_tibble() |>
    setNames(c("lon", "lat"))

  public_dat <- public_dat |>
    bind_cols(x) |>
    dplyr::filter(!is.na(lat))

  polygon <- public_dat |>
    st_as_sf(coords = c("lon", "lat"), crs =crs_ll) |>
    group_by(hex_id) |>
    summarise(geometry = st_combine(geometry),
              cpue = mean(cpue)) |>
    st_cast("POLYGON")

  num_colors <- nrow(d)
  col_func <- colorRampPalette(hex_colors)
  colors <- col_func(num_colors - 1)

  g <- g +
    new_scale_fill() +
    geom_sf(data = polygon,
            aes(fill = cpue),
            alpha = hex_alpha,
            color = hex_border_color,
            linewidth = hex_border_thickness,
            inherit.aes = FALSE)
    # Fill in the hexagons
    if(nrow(polygon) > 2){
      g <- g +
        scale_fill_gradientn(colors = hex_colors,
                             breaks = hex_fill_breaks,
                             labels = hex_fill_labels)
    }
    # Alternative color ramps for the hexagons
    # scale_fill_viridis_c(option = "magma",
    #                      direction = -1,
    #                      breaks = hex_fill_breaks,
    #                      labels = hex_fill_labels) +
    # scale_fill_viridis_c(trans = "sqrt",
    #                      option = "D",
    #                      breaks = hex_fill_breaks,
    #                      labels = hex_fill_labels) +
  g <- g +
    coord_sf(datum = st_crs(crs_ll),
             xlim = x_lim,
             ylim = y_lim,
             expand = FALSE) +
    guides(fill = guide_colorbar(barwidth = 0.5,
                                 barheight = 5,
                                 title = "CPUE (kg/hr)",
                                 title.hjust = 0,
                                 label.position = "left"))

  g
}
