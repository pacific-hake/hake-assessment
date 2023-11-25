#' Create a map of the west coast of North America showing places of interest
#'
#' @param dat Data from [gfdata::get_cpue_spatial()], [gfdata::get_cpue_spatial_ll()],
#'   or [gfdata::get_catch_spatial()]
#' @param crs Coordinate Reference System (CRS) number. Default is 4326 which
#' is WGS84: See [Epsg.org](https://epsg.org/home.html) for details. Click
#' `Text search` tab and look at the `code` column for valid crs numbers
#' @param extents The extents of the map as a two-column data frame with
#' columns `latitude` and `longitude`. Change this to make the map larger
#' or smaller, i.e. to zoom map in/out
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_spatial_cpue <- function(
    d,
    crs_wgs84 = 4326,
    crs_utm = 32609, # Zone 9 = 32609
    #crs_utm = 32610, # For Washington, Oregon, California (zone 10)
    #crs_utm = 32611, # For Washington, Oregon, California (zone 11)
    # x_lim = c(122, 890),
    # y_lim = c(5373, 6027),
    bathy_resolution = 1,
    x_lim = c(-131, -124),
    y_lim = c(48.5, 53),
    # extents = tibble(lon = c(-131, -124),
    #                  lat = c(48.5, 53)),
    bin_width = 7,
    n_minimum_vessels = 3,
    bath_contours = c(100, 200, 500),
    # extents = tibble(lon = c(-140, -113),
    #                  lat = c(33, 58)),
    label_size = 4){

  bath_contours <- -abs(bath_contours)

  library(sf)
  library(sp)
  library(raster)
  library(marmap)

  # Coastline ----
  coast <- ne_states(country = c("United States of America",
                                 "Canada",
                                 "Mexico"),
                     returnclass = "sf") |>
    st_crop(xmin = x_lim[1],
            xmax = x_lim[2],
            ymin = y_lim[1],
            ymax = y_lim[2])

  # coast_utm <- coast |>
  #   # sf::st_as_sf(coords = c("longitude", "latitude"),
  #   #          crs = crs_wgs84) |>
  #   st_transform(crs_utm) |>
  #   st_crop(xmin = x_lim[1],
  #           xmax = x_lim[2],
  #           ymin = y_lim[1],
  #           ymax = y_lim[2])

  # Bathymetry ----
  bath <- marmap::getNOAA.bathy(x_lim[1],
                                x_lim[2],
                                y_lim[1],
                                y_lim[2],
                                resolution =bathy_resolution,
                                keep = TRUE)

  # Convert 'bathy' type to regular data frame, with UTMs
  bath_df <- fortify.bathy(bath) |>
    as_tibble()
    #rename(lon = x, lat = y) |>
    #add_utm_columns(utm_crs = crs_utm) |>
    #mutate(across(c(x, y), ~{.x = .x * 1e3}))

  # CPUE hexagons ----
  d <- add_utm_columns(d)
  lims <- tibble(lon = x_lim,
                 lat = y_lim) |>
    add_utm_columns()

  privacy_out <- gfplot:::enact_privacy_rule(d,
                                             bin_width = bin_width,
                                             n_minimum_vessels = n_minimum_vessels,
                                             xlim = lims$X,
                                             ylim = lims$Y,
                                             plot_catch = FALSE)

  public_dat <- compute_hexagon_xy(privacy_out$data,
                                   bin_width = bin_width) |>
    mutate(hex_id = as.numeric(hex_id))
    #filter(X >= lims$X[1] & X <= lims$X[2] & Y >= lims$Y[1] & Y <= lims$Y[2])

  x <- public_dat |>
    st_as_sf(crs = crs_utm, coords = c("X", "Y")) |>
    st_transform(crs_wgs84) |>
    st_coordinates() |>
    as_tibble() |>
    setNames(c("lon", "lat"))
  public_dat <- public_dat |>
    bind_cols(x)

  polygon <- public_dat |>
    st_as_sf(coords = c("lon", "lat"), crs =crs_wgs84) |>
    group_by(hex_id) |>
    summarise(geometry = st_combine(geometry)) |>
    st_cast("POLYGON")


  browser()
  # Make the plot ----
  g <-ggplot(coast) +
    # add 100m contour
    geom_raster(data = bath_df,
                aes(x = x,
                    y = y,
                    fill = z)) +
    geom_contour(data = bath_df,
                 aes(x = x,
                     y = y,
                     z = z),
                 breaks = bath_contours,
                 color = "grey",
                 size = 0.5) +
    ggplot2::geom_sf(linewidth = 0.5,
                     fill = "red") +
    theme_bw() +
    ggplot2::geom_sf(data = polygon,
                     aes(color = "black",
                         fill = hex_id,
                         group = hex_id)) +
    coord_sf(datum = st_crs(crs_wgs84),
             xlim = x_lim,
             ylim = y_lim,
             expand = FALSE)


  g
}
