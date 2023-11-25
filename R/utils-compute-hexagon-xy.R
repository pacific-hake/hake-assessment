compute_hexagon_xy <- function(d,
                               bin_width){

  dx <- bin_width / 2
  dy <- bin_width / 2

  map_dfr(seq_len(nrow(d)), \(i){
    tibble(hex_id = i,
           cpue = d[i, "value"],
           hex_coords(d[i, "x"],
                      d[i, "y"], dx, dy))
  }) |>
    rename(X = x, Y = y)
}
