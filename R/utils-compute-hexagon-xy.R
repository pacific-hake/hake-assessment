#' Compute the coordinates for a hexagon to be shown in a spatial CPUE plot
#'
#' @param d Data from [gfdata::get_cpue_spatial()],
#' [gfdata::get_cpue_spatial_ll()], or [gfdata::get_catch_spatial()]
#' @param bin_width Bin width as defined in [ggplot2::stat_summary_hex()]
#'
#' @return A [tibble::tibble()] containing the columns 'hex_id', 'cpue$value,
#' X, and Y
#' @export
compute_hexagon_xy <- function(d,
                               bin_width){

  dx <- dy <- bin_width / 2

  d |>
    mutate(hex_id = row_number()) |>
    pmap(~{
      row <- tibble(...)
      # Calculate the six x coordinates for this hexagon
      hexagon_x_coords <- tibble(X = hex_coords(dx)$x + row$x)
      # Calculate the six y coordinates for this hexagon
      hexagon_y_coords <- tibble(Y = hex_coords(dy)$y + row$y)
      hexagon_df <- hexagon_x_coords |>
        bind_cols(hexagon_y_coords) |>
        transmute(hex_id = row$hex_id,
                  cpue = row$value,
                  X,
                  Y)

      hexagon_df
    }) |>
    bind_rows()
}
