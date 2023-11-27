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
#' others. There is a different numbre for each UTM zone. The default is
#' 32609 which is UTM zone 9 (west coast Canada). See
#' [Epsg.org](https://epsg.org/home.html) for details. Click `Text search`
#' tab and look at the `code` column for valid crs numbers
#' @param bathy_resolution The resolution for the bathymetry in minutes.
#' See [marmap::getNOAA.bathy()]
#' @param x_lim The length-2 vector representing the minimum and maximum
#' limits of the x-axis in degrees of longitude
#' @param y_lim The length-2 vector representing the minimum and maximum
#' limits of the y-axis in degrees of latitude
#' @param bin_width Bin width as defined in [ggplot2::stat_summary_hex()]
#' @param n_minimum_vessels The minimum number of vessels that must be present
#' in a hexagon for the hexagon to be shown
#' @param coast_line_color The ljne color for the coastline
#' @param coast_line_thickness The line thickness for the coastline
#' @param coast_fill The fill color for the land areas
#' @param bath_alpha The transparency for the bathymetric raster layer
#' @param bath_contours A vector of contours to show contour lines for.
#' These can be either positive or negative; they will all be made negative
#' in the function
#' @param bath_contour_color The line color for the bathymetric contours
#' @param bath_contour_thickness  The line thickness for the bathymetric
#' contours
#' @param hexagon_border_color The color of the hexagon border lines
#' @param hexagon_border_thickness The thickness of the hexagon border lines
#' @param hex_fill_breaks The breaks to show in the color bar that describes
#' the colors of the hexagons. Must be the same length as `hex_fill_labels`
#' @param hex_fill_labels The labels to show in the color bar that describes
#' the colors of the hexagons. Must be the same length as `hex_fill_breaks`
#' @param hex_cols A vector of colors used to create a color ramp palette
#' to represent the CPUE shown in the hexagons
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_spatial_cpue <- function(
    d,
    crs_ll = 4326,
    crs_utm = 32609, # For BC (zone 9)
    #crs_utm = 32610, # For Washington, Oregon, Northern California (zone 10)
    #crs_utm = 32611, # For Southern California (zone 11)
    bathy_resolution = 1,
    x_lim = c(-131.25, -124.5),
    y_lim = c(48, 52.5),
    bin_width = 7,
    n_minimum_vessels = 3,
    coast_line_color = "black",
    coast_line_thickness = 0.5,
    coast_fill = "grey",
    bath_alpha = 0.8,
    bath_contours = c(100, 200, 500, 1000, 1500),
    bath_contour_color = "black",
    bath_contour_thickness = 0.25,
    hexagon_border_color = "black",
    hexagon_border_thickness = 0.25,
    hex_fill_breaks = c(0, 250, 1000, seq(3000, 14000, 3000)),
    hex_fill_labels = comma(hex_fill_breaks),
    hex_colors = c("antiquewhite",
                   "yellow",
                   "orange",
                   "red")){

  bath_contours <- -abs(bath_contours)

  # Coastline ----
  coast <- ne_states(country = c("United States of America",
                                 "Canada",
                                 "Mexico"),
                     returnclass = "sf") |>
    st_crop(xmin = x_lim[1],
            xmax = x_lim[2],
            ymin = y_lim[1],
            ymax = y_lim[2])

  # Bathymetry ----
  bath <- getNOAA.bathy(x_lim[1],
                        x_lim[2],
                        y_lim[1],
                        y_lim[2],
                        resolution =bathy_resolution,
                        keep = TRUE)

  # Convert 'bathy' type to regular data frame
  bath_df <- bath |>
    fortify.bathy() |>
    as_tibble()

  # CPUE hexagons ----
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

  x <- public_dat |>
    mutate(across(c(X, Y), ~{.x <- .x * 1e3})) |>
    st_as_sf(crs = crs_utm, coords = c("X", "Y")) |>
    st_transform(crs_ll) |>
    st_coordinates() |>
    as_tibble() |>
    setNames(c("lon", "lat"))

  public_dat <- public_dat |>
    bind_cols(x)

  polygon <- public_dat |>
    st_as_sf(coords = c("lon", "lat"), crs =crs_ll) |>
    group_by(hex_id) |>
    summarise(geometry = st_combine(geometry),
              cpue = mean(cpue)) |>
    st_cast("POLYGON")

  num_colors <- nrow(d)
  col_func <- colorRampPalette(hex_colors)
  colors <- col_func(num_colors - 1)

  # Make the plot ----
  g <-ggplot(coast) +
    # add 100m contour
    geom_raster(data = bath_df,
                aes(x = x,
                    y = y,
                    fill = z),
                alpha = bath_alpha,
                show.legend = FALSE) +
    geom_contour(data = bath_df,
                 aes(x = x,
                     y = y,
                     z = z),
                 breaks = bath_contours,
                 color = bath_contour_color,
                 size = bath_contour_thickness) +
    # Fill in the coastline. Must come after the bathymetry raster
    geom_sf(linewidth = coast_line_thickness,
            color = coast_line_color,
            fill = coast_fill) +
    # Fill the bathymetry raster
    scale_fill_continuous(type = "gradient") +
    new_scale_fill() +
    geom_sf(data = polygon,
            aes(fill = cpue),
            color = hexagon_border_color,
            linewidth = hexagon_border_thickness,
            inherit.aes = FALSE) +
    # Fill in the hexagons
    scale_fill_gradientn(colors = hex_colors,
                         breaks = hex_fill_breaks,
                         labels = hex_fill_labels) +
    # Alternative color ramps for the hexagons
    # scale_fill_viridis_c(option = "magma",
    #                      direction = -1,
    #                      breaks = hex_fill_breaks,
    #                      labels = hex_fill_labels) +
    # scale_fill_viridis_c(trans = "sqrt",
    #                      option = "D",
    #                      breaks = hex_fill_breaks,
    #                      labels = hex_fill_labels) +
    coord_sf(datum = st_crs(crs_ll),
             xlim = x_lim,
             ylim = y_lim,
             expand = FALSE) +
    guides(fill = guide_colorbar(barwidth = 0.5,
                                 barheight = 5,
                                 title = "CPUE (kg/hr)",
                                 title.hjust = 0,
                                 label.position = "left")) +
    xlab("Longitude (°)") +
    ylab("Latitude (°)") +
    scale_y_continuous(labels = comma)

  g
}
