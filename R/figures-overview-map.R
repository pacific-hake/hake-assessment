
make.overview.map.plot <- function(path,
                                   crs = 4326,
                                   extents = data.frame(lon = c(-140, -113),
                                                        lat = c(33, 58)),
                                   contour_depths = c(100, 200, 400, 1000, 1500, 2000),
                                   contour_color = "lightblue",
                                   contour_thickness = 0.25){
  
  ports_file <- file.path(path, "port-locations.csv")
  states_file <- file.path(path, "state-locations.csv")

  coast <- ne_states(country = c("United States of America", "Canada", "Mexico"),
                     returnclass = "sf")
  
  ports <- read_csv(ports_file) %>% 
    mutate(lon = -abs(lon))
  ports_left <- ports %>% 
    filter(pos == "left")
  ports_right <- ports %>% 
    filter(pos == "right")
  
  ports_left_sf <- ports_left %>% 
    st_as_sf(coords = c("lon", "lat"), crs = crs) %>% 
    `st_crs<-`(crs)
  ports_right_sf <- ports_right %>% 
    st_as_sf(coords = c("lon", "lat"), crs = crs) %>% 
    `st_crs<-`(crs)
  
  states <- read_csv(states_file) %>% 
    mutate(lon = -abs(lon))
  states_sf <- states %>% 
    st_as_sf(coords = c("lon", "lat"), crs = crs) %>% 
    `st_crs<-`(crs)
  
  extents <- st_as_sf(extents, coords = c("lon", "lat")) %>%
    `st_crs<-`(crs) %>%
    st_coordinates() %>%
    as_tibble()
  
  g <- ggplot(data = coast) +
    geom_sf(color = "black", fill = "antiquewhite") +
    geom_sf(data = ports_left_sf, color = "red", size = 2,  show.legend = FALSE) +
    geom_sf(data = ports_right_sf, color = "red", size = 2,  show.legend = FALSE) +
    geom_label_repel(data = states,
                     aes(x = lon,
                         y = lat,
                         label = name),
                     size = 4,
                     color = "royalblue",
                     fill = "lightblue") +
    geom_label_repel(data = ports_left,
                     aes(x = lon,
                         y = lat,
                         label = name),
                     xlim = c(NA, -125),
                     fill = "white",
                     alpha = 0.6) +
    geom_label_repel(data = ports_right,
                     aes(x = lon,
                         y = lat,
                         label = name),
                     xlim = c(-122, NA),
                     fill = "white",
                     alpha = 0.6) +
    coord_sf(xlim = extents[[1]],
             ylim = extents[[2]]) +
    ylab(paste0("Latitude (", intToUtf8(176), ")")) +
    ylab(paste0("Longitude (", intToUtf8(176), ")"))
    g
}