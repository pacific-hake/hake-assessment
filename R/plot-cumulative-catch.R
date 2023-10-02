#' Create a plot of cumulative catches, with one panel for each fishery
#'
#' @param catch_lst A list of data tables with either three columns: month,
#' year, and catch or 13 columns: Year, and one column each for the 12 months
#' @param names_lst A list of names for the panel titles in the plot. Must be
#' the same length as the `catch_lst` list
#' @param yrs A vector of two years representing the maximum and minimum years
#' to include in the data shown in the plot
#' @param scale A number to divide the catch by
#' @param type The type of plot, one of default (no change to data),
#' proportion, or cumulative
#' @param leg_pos A two-element vector describing the placement of the
#' legend inside the plotting area where the x,y values are both between 0
#' and 1 or "none" for no legend
#' @param leg_font_size The font size for the legend items
#' @param line_width The width of the lines
#' @param line_gap The gap between the points and lines (blank spacing)
#' @param point_shape The point shape type
#' @param point_size The size of the points
#' @param point_stroke The stroke value for the points
#' @param ax_title_font_size The title font size
#' @param ax_tick_font_size The tick label font size
#' @param ax_label_color The axis label color
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_cumulative_catches <- function(catch_lst,
                                    names_lst,
                                    yrs = c((year(now()) - 4),
                                            (year(now()) - 1)),
                                    scale = 1000,
                                    type = c("default",
                                             "proportion",
                                             "cumulative"),
                                    leg_pos = c(0.1, 0.7),
                                    leg_font_size = 8,
                                    line_width = ts_linewidth,
                                    line_gap = ts_linegap,
                                    point_shape = ts_pointshape,
                                    point_size = ts_pointsize,
                                    point_stroke = ts_pointstroke,
                                    ax_title_font_size = axis_title_font_size,
                                    ax_tick_font_size = axis_tick_font_size,
                                    ax_label_color = axis_label_color){

  type <- match.arg(type)

  if(length(catch_lst) != length(names_lst)){
    warning("`names_lst` is not te same length as `catch_lst. Using ",
            "default panel titles")
    names_lst <- as.list(paste0("Fishery ", seq(1, length(catch_lst))))
  }
  colors <- plot_color(yrs[2] - yrs[1] + 1)

  # Plot a single cumulative catch plot
  #
  # @param d The data frame to plot
  # @param type The type of plot to produce
  # @param leg_pos A vector of two values for x/y location of the legend
  # or "non" for no legend
  # @param title The text to use for the panel title
  #
  # @return A [ggplot2::ggplot()] object
  plot_single_panel <- function(d,
                                type = type,
                                leg_pos = leg_pos,
                                title = ""){

    if(type == "proportion"){
      y <- cumsum(y) / sum(y)
    }
    if(type == "cumulative"){
      d <- d |>
        group_by(year) |>
        mutate(catch = cumsum(catch))
    }

    g <- ggplot(d,
                aes(x = month,
                    y = catch,
                    color = year)) +
      geom_pointpath(linewidth = line_width,
                     mult = line_gap,
                     size = point_size,
                     shape = point_shape,
                     stroke = point_stroke) +
      scale_color_manual(values = colors) +
      scale_x_continuous(breaks = seq(2, 12, 2),
                         labels = month.abb[seq(2, 12, 2)]) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(color = ax_label_color,
                                       size = ax_tick_font_size,
                                       angle = 0,
                                       hjust = 0.5,
                                       vjust = -0.25,
                                       face = "plain"),
            axis.text.y = element_text(color = ax_label_color,
                                       size = ax_tick_font_size,
                                       hjust = 1,
                                       vjust = 0.5,
                                       face = "plain"),
            plot.title = element_text(size = ax_title_font_size,
                                      hjust = 0.5),
            legend.position = leg_pos,
            legend.key.size = unit(0.2, 'cm'),
            legend.title = element_blank(),
            legend.text = element_text(size = leg_font_size),
            legend.text.align = 0) +
      ggtitle(title)

    g
  }

  p <- imap(catch_lst, function(d, ind){
    if(!"month" %in% names(d)){
      # Canadian tables are width-wise not long-wise, so make them long
      d <- d |>
        pivot_longer(-Year) |>
        setNames(c("year", "month", "catch")) |>
        select(month, year, catch) |>
        mutate(month = as.numeric(month))
    }

    d <- d |>
      filter(year %in% yrs[1]:yrs[2]) |>
      mutate(year = factor(year)) |>
      mutate(catch = catch / scale) |>
      complete(year = year, month = 1:12, fill = list(catch = 0))

    if(ind != 1){
      leg_pos <- "none"
    }

    plot_single_panel(d, type,
                      leg_pos = leg_pos,
                      title = names_lst[[ind]])
  })

  plt <- plot_grid(plotlist = p, ncol = 2, align = "v")

  # x_grob <- textGrob("Month",
  #                    gp = gpar(fontsize = ax_title_font_size))
  y_grob <- textGrob("Cumulative catch (kt)",
                     gp = gpar(fontsize = ax_title_font_size),
                     rot = 90)

  grid.arrange(arrangeGrob(plt,
                           #bottom = x_grob,
                           left = y_grob))
}
