#' Create a boxplot of depths for a given data frame
#'
#' @param depth_df A data frame containing boxplot input, see files like
#' `depth-us-*.csv`
#' @param yrs A vector of the years to show in the plot
#' @param title_text The title to show. If `NULL`, do not show the title
#' @param line_col The color of the median line and box outline
#' @param box_width With of each box
#' @param y_lim The vector of two value representing the y-limits for the plot
#' @param y_breaks The vector of y-axis values to show on the plot
#' @param reverse_y Logical. If `TRUE`, place the zero on top of the y-axis
#' and the depths in reverse order downward, to mirror the water column layout
#' in the real world
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_depth <- function(depth_df,
                       yrs,
                       title_text = NULL,
                       line_col = "grey",
                       box_width = 0.75,
                       y_lim = NULL,
                       y_breaks = NULL,
                       reverse_y = TRUE,
                       ...){

  yrs_not_present <- yrs[!yrs %in% depth_df$year]
  if(!is.null(yrs_not_present)){
    new_rows <- map(yrs_not_present, ~{
      c(.x, rep(NA, ncol(depth_df) - 1)) |>
        vec2df(nms = names(depth_df))
    }) |>
      bind_rows()

    depth_df <- depth_df |>
      bind_rows(new_rows)
  }
  depth_df <- depth_df |>
    dplyr::filter(year %in% yrs) |>
    rename(lower = lowerhinge,
           upper = upperhinge,
           middle = median,
           ymin = lower95,
           ymax = upper95)

  col <- plot_color(nrow(depth_df))

  g <- ggplot(depth_df,
              aes(x = year,
                  lower = lower,
                  upper = upper,
                  middle = middle,
                  ymin = ymin,
                  ymax = ymax,
                  group = year)) +
    geom_boxplot(stat="identity",
                 position = "identity",
                 fill = col,
                 col = line_col,,
                 width = box_width) +
    scale_x_continuous(breaks = yrs) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_text(hjust = 5)) +
    xlab("")

  if(reverse_y){
    y_breaks <- rev(y_breaks)
    y_lim <- rev(y_lim)
  }
  if(!is.null(y_breaks)){
    g <- g +
      scale_y_continuous(breaks = y_breaks,
                         labels = comma)
  }
  if(!is.null(y_lim)){
    g <- g +
      ylim(y_lim)
  }
  if(!is.null(title_text)){
    g <- g +
      ggtitle(title_text)
  }

  g
}
