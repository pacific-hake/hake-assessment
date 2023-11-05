#' Colors for the Hake Assessment
#'
#' Generate the color scheme for the hake assessment
#' such that as more years of historical data are added on
#' that the more recent years are still the same color.
#' For example, the most recent year of data will always be
#' black and the second most recent year will always be red.
#' If more than the maximum number of color for the `palette`
#' are requested, a color ramp will be constructed for the
#' remaining colors, based on the `palette`.
#'
#' @param num_colors The number of colors to return
#' @param palette A palette found in [RColorBrewer::brewer.pal.info]
#'
#' @return A vector of HEX colours
#' @export
#' @examples
#' n <-18
#' plot(data.frame(1:n, 1), col= plot_color(n), pch = 19, cex = 5)
plot_color <- function(num_colors = 10,
                       palette = "Set1"){

  palette_table <- brewer.pal.info |>
    as_tibble(rownames = "palette")

  if(!palette %in% palette_table$palette){
    stop("`", palette, "` is not a valid palette. Valid palettes are:\n\n",
         paste(palette_table$palette, collapse = ", "))
  }

  palette_info <- brewer.pal.info[palette_table$palette == palette, ]
  if(num_colors <= palette_info$maxcolors){
    if(num_colors <= 2){
      # Avoid warning when there's only one model. `brewer.pal() needs n = 3
      # or greater
      num_colors <- 3
    }
    base <- brewer.pal(name = palette, n = num_colors)
    colors <- c(base[(num_colors - 1):1], "#000000")
  }else{
    base <- brewer.pal(name = palette, n = palette_info$maxcolors)
    colors <- c(base[(palette_info$maxcolors - 1):1], "#000000")

    palette_func <- colorRampPalette(brewer.pal(palette_info$maxcolors, palette))
    palette_colors <- palette_func(n = num_colors - palette_info$maxcolors + 3)

    # Get rid of the last few colors by creating 3 more color than needed and
    # then removing them from the beginning. This avoids the last color being
    # the same as the first when more than `palette_info$maxcolors` length
    palette_colors <- palette_colors[-(1:3)]
    colors <- c(palette_colors, colors)
  }

  colors
}
