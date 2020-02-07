
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
  ports_left_north <- ports %>% 
    filter(pos == "left",
           north_south == "north")
  ports_left_south <- ports %>% 
    filter(pos == "left",
           north_south == "south")
  ports_right_north <- ports %>% 
    filter(pos == "right",
           north_south == "north")
  ports_right_south <- ports %>% 
    filter(pos == "right",
           north_south == "south")
  
  ports_left_north_sf <- ports_left_north %>% 
    st_as_sf(coords = c("lon", "lat"), crs = crs) %>% 
    `st_crs<-`(crs)
  ports_left_south_sf <- ports_left_south %>% 
    st_as_sf(coords = c("lon", "lat"), crs = crs) %>% 
    `st_crs<-`(crs)
  ports_right_north_sf <- ports_right_north %>% 
    st_as_sf(coords = c("lon", "lat"), crs = crs) %>% 
    `st_crs<-`(crs)
  ports_right_south_sf <- ports_right_south %>% 
    st_as_sf(coords = c("lon", "lat"), crs = crs) %>% 
    `st_crs<-`(crs)
  
  states <- read_csv(states_file) %>% 
    mutate(lon = -abs(lon))
  se_alaska <- states %>% 
    filter(name == "Southeast Alaska")
  states <- states %>% 
    filter(name != "Southeast Alaska")
  
  extents <- st_as_sf(extents, coords = c("lon", "lat")) %>%
    `st_crs<-`(crs) %>%
    st_coordinates() %>%
    as_tibble()
  
  g <- ggplot(data = coast) +
    geom_sf(color = "black", fill = "grey") +
    geom_sf(data = ports_left_north_sf, color = "red", size = 2,  show.legend = FALSE) +
    geom_sf(data = ports_left_south_sf, color = "red", size = 2,  show.legend = FALSE) +
    geom_sf(data = ports_right_north_sf, color = "red", size = 2,  show.legend = FALSE) +
    geom_sf(data = ports_right_south_sf, color = "red", size = 2,  show.legend = FALSE) +
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
                    xlim = c(NA, -133),
                    segment.size = 0.8,
                    arrow = arrow(length = unit(0.01, "npc"))) +
    geom_text_repel(data = ports_left_south,
                    aes(x = lon,
                        y = lat,
                        label = name),
                    xlim = c(NA, -125),
                    segment.size = 0.8,
                    arrow = arrow(length = unit(0.01, "npc"))) +
    geom_text_repel(data = ports_right_north,
                    aes(x = lon,
                        y = lat,
                        label = name),
                    xlim = c(-129, NA),
                    segment.size = 0.8,
                    arrow = arrow(length = unit(0.01, "npc"))) +
    geom_text_repel(data = ports_right_south,
                    aes(x = lon,
                        y = lat,
                        label = name),
                    xlim = c(-122, NA),
                    segment.size = 0.8,
                    arrow = arrow(length = unit(0.01, "npc"))) +
    coord_sf(xlim = extents[[1]],
             ylim = extents[[2]]) +
    ylab(paste0("Latitude (", intToUtf8(176), ")")) +
    xlab(paste0("Longitude (", intToUtf8(176), ")"))
    g
}