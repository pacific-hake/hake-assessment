#' Create a map of the west coast of North America showing places of interest
#'
#' @param ports_df A data frame as read in by [readr::read_csv()]
#' @param states_df A data frame as read in by [readr::read_csv()]
#' @param crs Coordinate Reference System (CRS) number. Default is 4326 which
#' is WGS84: See [Epsg.org](https://epsg.org/home.html) for details. Click
#' `Text search` tab and look at the `code` column for valid crs numbers
#' @param extents The extents of the map as a two-column data frame with
#' columns `latitude` and `longitude`. Change this to make the map larger
#' or smaller, i.e. to zoom map in/out
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_overview_map <- function(
    ports_df,
    states_df,
    crs = 4326,
    extents = tibble(lon = c(-140, -113),
                     lat = c(33, 58))){


  coast <- ne_states(country = c("United States of America",
                                 "Canada",
                                 "Mexico"),
                     returnclass = "sf")

  ports <- ports_df |>
    mutate(lon = -abs(lon))
  ports_left_north <- ports |>
    filter(pos == "left",
           north_south == "north")
  ports_left_south <- ports |>
    filter(pos == "left",
           north_south == "south")
  ports_right_north <- ports |>
    filter(pos == "right",
           north_south == "north")
  ports_right_south <- ports |>
    filter(pos == "right",
           north_south == "south")

  ports_left_north_sf <- ports_left_north |>
    st_as_sf(coords = c("lon", "lat"), crs = crs) |>
    `st_crs<-`(crs)
  ports_left_south_sf <- ports_left_south |>
    st_as_sf(coords = c("lon", "lat"), crs = crs) |>
    `st_crs<-`(crs)
  ports_right_north_sf <- ports_right_north |>
    st_as_sf(coords = c("lon", "lat"), crs = crs) |>
    `st_crs<-`(crs)
  ports_right_south_sf <- ports_right_south |>
    st_as_sf(coords = c("lon", "lat"), crs = crs) |>
    `st_crs<-`(crs)

  states <- states_df |>
    mutate(lon = -abs(lon))
  se_alaska <- states |>
    filter(name == "Southeast Alaska")
  states <- states |>
    filter(name != "Southeast Alaska")

  extents <- st_as_sf(extents, coords = c("lon", "lat")) |>
    `st_crs<-`(crs) |>
    st_coordinates() |>
    as_tibble()

  g <- ggplot(data = coast) +
    geom_sf(color = "black",
            fill = "grey") +
    geom_sf(data = ports_left_north_sf,
            color = "red",
            size = 2,
            show.legend = FALSE) +
    geom_sf(data = ports_left_south_sf,
            color = "red",
            size = 2,
            show.legend = FALSE) +
    geom_sf(data = ports_right_north_sf,
            color = "red",
            size = 2,
            show.legend = FALSE) +
    geom_sf(data = ports_right_south_sf,
            color = "red",
            size = 2,
            show.legend = FALSE) +
    geom_text_repel(data = states,
                     aes(x = lon,
                         y = lat,
                         label = name),
                     size = 5,
                     color = "black") +
    geom_text_repel(data = se_alaska,
                    aes(x = lon,
                        y = lat,
                        label = name),
                    xlim = c(-131, NA),
                    size = 5,
                    color = "black",
                    segment.size = 1,
                    arrow = arrow(length = unit(0.02, "npc"))) +
    geom_text_repel(data = ports_left_north,
                    aes(x = lon,
                        y = lat,
                        label = name),
                    xlim = c(NA, -135),
                    size = 4.5,
                    segment.size = 0.8,
                    arrow = arrow(length = unit(0.01, "npc"))) +
    geom_text_repel(data = ports_left_south,
                    aes(x = lon,
                        y = lat,
                        label = name),
                    xlim = c(NA, -125),
                    size = 4.5,
                    segment.size = 0.8,
                    arrow = arrow(length = unit(0.01, "npc"))) +
    geom_text_repel(data = ports_right_north,
                    aes(x = lon,
                        y = lat,
                        label = name),
                    xlim = c(-129, NA),
                    size = 4.5,
                    segment.size = 0.8,
                    arrow = arrow(length = unit(0.01, "npc"))) +
    geom_text_repel(data = ports_right_south,
                    aes(x = lon,
                        y = lat,
                        label = name),
                    xlim = c(-122, NA),
                    size = 4.5,
                    segment.size = 0.8,
                    arrow = arrow(length = unit(0.01, "npc"))) +
    coord_sf(xlim = extents[[1]],
             ylim = extents[[2]]) +
    ylab(paste0("Latitude (", intToUtf8(176), ")")) +
    xlab(paste0("Longitude (", intToUtf8(176), ")"))

    g
}