#' Create a map of a part of the west coast of North America,
#' including bathymetry raster and contours
#'
#' @param crs_ll Coordinate Reference System (CRS) number for a
#' latitude/longitude-based projection. This could be NAD83 or WGS84 or
#' others. The default is 4326 which is WGS84: See
#' [Epsg.org](https://epsg.org/home.html) for details. Click `Text search`
#' tab and look at the `code` column for valid crs numbers
#' @param x_lim The length-2 vector representing the minimum and maximum
#' limits of the x-axis in degrees of longitude
#' @param y_lim The length-2 vector representing the minimum and maximum
#' limits of the y-axis in degrees of latitude
#' @param coast_line_color The line color for the coastline
#' @param coast_line_thickness The line thickness for the coastline
#' @param coast_fill The fill color for the land areas
#' @param show_bathy_contours Logical. If `TRUE` draw the bathymetry contour
#' lines
#' @param show_bathy_raster Logical. If `TRUE` draw the bathymetry raster
#' layer
#' @param bathy_resolution The resolution for the bathymetry in minutes.
#' See [marmap::getNOAA.bathy()]
#' @param bathy_alpha The transparency for the bathymetric raster layer
#' @param bathy_contours A vector of contours to show contour lines for.
#' These can be either positive or negative; they will all be made negative
#' in the function
#' @param bathy_contour_color The line color for the bathymetric contours
#' @param bathy_contour_thickness  The line thickness for the bathymetric
#' contours
#' @param bathy_raster_scale A `ggplot2::scale_*` function describing the
#' color gradient to use for the bathymetry raster. For example:
#' `scale_fill_gradientn(colors = c("red", "orange", "yellow"))`
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_map <- function(
    crs_ll = 4326,
    x_lim = c(-140, -113),
    y_lim = c(33, 58),
    coast_line_color = "black",
    coast_line_thickness = 0.1,
    coast_fill = "grey",
    show_bathy_contours = TRUE,
    show_bathy_raster = TRUE,
    bathy_resolution = 1,
    bathy_alpha = 0.8,
    bathy_contours = c(100, 200, 500, 1000, 1500),
    bathy_contour_color = "black",
    bathy_contour_thickness = 0.25,
    bathy_raster_scale = scale_fill_continuous(type = "gradient")){

  if(show_bathy_contours){
    if(is.null(bathy_contours)){
      stop("You chose to show bathymetry contours (`show_bathy_contours = ",
           "TRUE`) but `bathy_contours` is `NULL`")
    }
    bathy_contours <- -abs(bathy_contours)
  }

  # Coastline ----
  coast <- ne_states(country = c("United States of America",
                                 "Canada",
                                 "Mexico"),
                     returnclass = "sf")

  g <-ggplot(coast)

  # Bathymetry ----
  # This creates a file named like `marmap_coord*`. There is a new file
  # created for each new set of x_lim/y_lim values supplied
  if(show_bathy_raster || show_bathy_contours){
    bathy <- getNOAA.bathy(x_lim[1],
                           x_lim[2],
                           y_lim[1],
                           y_lim[2],
                           resolution =bathy_resolution,
                           keep = TRUE)

    # Convert 'bathy' type to regular data frame
    bathy_df <- bathy |>
      fortify.bathy() |>
      as_tibble()
  }
  if(show_bathy_raster){
    g <- g +
      geom_raster(data = bathy_df,
                  aes(x = x,
                      y = y,
                      fill = z),
                  alpha = bathy_alpha,
                  show.legend = FALSE)
  }
  if(show_bathy_contours){
    g <- g +
      geom_contour(data = bathy_df,
                   aes(x = x,
                       y = y,
                       z = z),
                   breaks = bathy_contours,
                   color = bathy_contour_color,
                   size = bathy_contour_thickness)
  }

  g <- g +
    # Fill in the coastline. Must come after the bathymetry raster
    geom_sf(linewidth = coast_line_thickness,
            color = coast_line_color,
            fill = coast_fill) +
    # Fill the bathymetry raster
    bathy_raster_scale +
    new_scale_fill() +
    coord_sf(datum = st_crs(crs_ll),
             xlim = x_lim,
             ylim = y_lim,
             expand = FALSE) +
    xlab("Longitude (°W)") +
    ylab("Latitude (°N)") +
    # Remove degree symbol and N and W from the tick labels and show only raw
    # lat/longs
    scale_x_continuous(labels = abs) +
    scale_y_continuous(labels = abs)

  g
}
