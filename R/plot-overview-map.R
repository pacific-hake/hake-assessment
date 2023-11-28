#' Create a map of the west coast of North America showing places of interest
#'
#' @param crs_ll Coordinate Reference System (CRS) number. Default is
#' 4326 which is WGS84: See [Epsg.org](https://epsg.org/home.html) for
#' details. Click `Text search` tab and look at the `code` column for
#' valid crs numbers
#' @param x_lim The length-2 vector representing the minimum and maximum
#' limits of the x-axis in degrees of longitude
#' @param y_lim The length-2 vector representing the minimum and maximum
#' limits of the y-axis in degrees of latitude
#' @label_size The size of the font for the port and state labels
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_overview_map <- function(
    crs_ll = 4326,
    x_lim = c(-140, -110),
    y_lim = c(30, 58),
    label_size = 4,
    label_color_default = "white",
    label_fill_default = "white",
    label_border_size = NA,
    arrowhead_size = 0.01,
    point_color = "red",
    ...){

  g <- plot_map(crs_ll = crs_ll,
                x_lim = x_lim,
                y_lim = y_lim,
                coast_line_thickness = 0.1,
                ...)

  # locations_df <- states_df |>
  #   mutate(lon = -abs(lon)) |>
  #   mutate(color = ifelse(is.na(color), label_color, color))

  locations_df <- ports_df |>
    bind_rows(states_df) |>
    mutate(lon = -abs(lon)) |>
    mutate(label_color = ifelse(is.na(label_color),
                                label_color_default,
                                label_color)) |>
    mutate(label_fill = ifelse(is.na(label_fill),
                               label_fill_default,
                               label_fill))

  # locations_sf <- locations_df |>
  #   st_as_sf(coords = c("lon", "lat"), crs = crs_ll) |>
  #   `st_crs<-`(crs_ll)

  # Add the ports and states to the map one at a time, because each has
  # custom settings
  pmap(locations_df, ~{

    row_df <- tibble(...)
    if(is.na(row_df$lon)){
      if(is.na(row_df$label_lon)){
        stop("Both `lon` and `label_lon`cannot be `NA` in row:\n",
             row_df)
      }
      row_df$lon <- row_df$label_lon
    }
    if(is.na(row_df$lat)){
      if(is.na(row_df$label_lat)){
        stop("Both `lat` and `label_lat`cannot be `NA` in row:\n",
             row_df)
      }
      row_df$lat <- row_df$label_lat
    }
    if(is.na(row_df$label_lon)){
      row_df$label_lon <- row_df$lon
      warning("Using point longitude coordinate in place of `NA` label ",
              "coordinate")
    }
    if(is.na(row_df$label_lat)){
      row_df$label_lat <- row_df$lat
      warning("Using point latitude coordinate in place of `NA` label ",
              "coordinate")
    }

    latlon_df <- row_df |>
      select("lon", "lat")

    row_df <- row_df |>
      st_as_sf(coords = c("lon", "lat"), crs = crs_ll) |>
      `st_crs<-`(crs_ll) |>
      bind_cols(latlon_df)

    g <<- g +
      geom_sf(data = row_df,
              color = ifelse(row_df$show_point,
                             point_color,
                             "transparent"),
              size = 2,
              show.legend = FALSE)

    if(row_df$label_pos == "left"){
      label_pos_vec <- c(NA, row_df$label_lon)
    }else if(row_df$label_pos == "right"){
      label_pos_vec <- c(row_df$label_lon, NA)
    }else{
      stop("There was a bad value found in the `port-locations.csv` file. ",
           "The column `label_pos` must be either `right` or `left`. ",
           "'", row_df$label_pos, "' was found")
    }

    if(row_df$show_arrow){

      g <<- g +
        geom_segment(data = row_df,
                     aes(x = lon,
                         y = lat,
                         xend = label_lon,
                         yend = label_lat,
                         color = label_color),
                     arrow = arrow(length = unit(arrowhead_size,
                                                 "npc"),
                                   ends = "first",
                                   type = "closed")) +
        geom_label(data = row_df,
                  aes(x = label_lon,
                      y = label_lat,
                      color = label_color,
                      label = name,
                      size = label_size,
                      fill = label_fill),
                  label.size = label_border_size)
    }else{
      g <<- g +
        geom_label(data = row_df,
                   aes(x = label_lon,
                       y = label_lat,
                       color = label_color,
                       fill = label_fill,
                       label = name,
                       size = label_size),
                   label.size = label_border_size)
    }
    NULL
  })

  g <- g  +
    scale_color_identity()+
    scale_size_identity()+
    scale_fill_identity()+
    coord_sf(datum = st_crs(crs_ll),
             xlim = x_lim,
             ylim = y_lim,
             expand = FALSE)

  g
}