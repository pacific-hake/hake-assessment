#' Create a plot of cumulative catches, with one panel for each fishery
#'
#' @param catch_lst A list of data tables with either three columns: month,
#' year, and catch or 13 columns: Year, and one column each for the 12 months
#' @param names_lst A list of names for the panel titles in the plot. Must be
#' the same length as the `catch_lst` list
#' @param yrs A vector of two years representing the maximum and minimum years
#' to include in the data shown in the plot
#' @param scale A number to divide the catch by
#' @param type The type of plot, one of `catch`, `proportion`, `cumulative`,
#' or `quota`
#' @param quota_lst A list the same length as `catch_lst` and `names_lst`
#' containing data frames which have two columns each, one named `Year`
#' and one representing the sector quota for that year for which the name
#' does not matter
#' @param disclaimer_text Text to show in the empty panel at the bottom
#' right of the plot grid. If `NULL`, Nothing will be shown there
#' @param y_breaks A vector of y-axis values to show. The viewable part of
#' the y-axis will also be set to the minimum to maximum of these values
#' @param leg_pos A two-element vector describing the placement of the
#' legend inside the plotting area where the x,y values are both between 0
#' and 1 or "none" for no legend
#' @param leg_font_size The font size for the legend items
#' @param line_width The width of the lines
#' @param line_gap The gap between the points and lines (blank spacing)
#' @param point_shape The point shape type
#' @param point_size The size of the points
#' @param point_stroke The stroke value for the points
#' @param title_font_size Size of the title text for each panel

#' @return A [ggplot2::ggplot()] object
#' @export
plot_catch_by_month <- function(catch_lst,
                                names_lst,
                                yrs = c((year(now()) - 4),
                                        (year(now()) - 1)),
                                scale = 1000,
                                type = c("catch",
                                         "proportion",
                                         "cumulative",
                                         "quota"),
                                quota_lst = NULL,
                                disclaimer_text = NULL,
                                y_breaks = NULL,
                                leg_pos = c(0.1, 0.7),
                                leg_font_size = 8,
                                line_width = ts_linewidth,
                                line_gap = ts_linegap,
                                point_shape = ts_pointshape,
                                point_size = ts_pointsize,
                                point_stroke = ts_pointstroke,
                                title_font_size = axis_title_font_size,
                                ax_title_x = axis_title_font_size,
                                ax_title_y = axis_title_font_size,
                                ax_text_x = axis_tick_font_size,
                                ax_text_y = axis_tick_font_size){

  type <- match.arg(type)

  if(type == "quota"){
    if(is.null(quota_lst)){
      stop("You provided `quota` as the `type` and the data frame containing ",
           "quotas `quota_df` is `NULL`")
    }
    if(length(quota_lst) != length(catch_lst)){
      stop("The `quota_lst` list has a different length than the ",
           "`catch_lst` list")
    }
  }

  if(length(catch_lst) != length(names_lst)){
    warning("`names_lst` is not te same length as `catch_lst. Using ",
            "default panel titles")
    names_lst <- as.list(paste0("Fishery ", seq(1, length(catch_lst))))
  }
  colors <- plot_color(yrs[2] - yrs[1] + 1)

  # Plot a single catch plot
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
                                title = "",
                                quota = NULL){

    if(type == "proportion"){
      d <- d |>
        group_by(year) |>
        mutate(catch = cumsum(catch) / sum(catch))
    }
    if(type == "cumulative"){
      d <- d |>
        group_by(year) |>
        mutate(catch = cumsum(catch))
    }
    if(type == "quota"){
      if(is.null(quota)){
        stop("`quota` is `NULL` in `plot_single_panel()`")
      }
      d <- d |>
        group_by(year) |>
        mutate(catch = cumsum(catch) / quota * 1000)
    }

    x_breaks <- 1:12
    x_labels <- month.abb[x_breaks]
    x_labels[x_breaks %% 2 == 1] <- ""

    # Zero catch data frame
    d_zero <- d |>
      dplyr::filter(catch == 0)

    g <- ggplot(d,
                aes(x = month,
                    y = catch,
                    color = year)) +
      geom_pointpath(linewidth = line_width,
                     mult = line_gap,
                     size = point_size,
                     shape = point_shape,
                     stroke = point_stroke) +
      geom_point(data = d_zero,
                 aes(x = month,
                     y = catch,
                     color = year),
                 shape = 17,
                 size = 2.5) +
      scale_color_manual(values = colors) +
      scale_x_continuous(breaks = x_breaks,
                         labels = x_labels) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = ax_text_x),
            axis.text.y = element_text(size = ax_text_y),
            plot.title = element_text(size = title_font_size,
                                      hjust = 0.5),
            plot.margin = margin(0, 0, 10, 10),
            legend.position = leg_pos,
            legend.key.size = unit(0.2, 'cm'),
            legend.title = element_blank(),
            legend.text = element_text(size = leg_font_size),
            legend.text.align = 0) +
      ggtitle(title)

    if(!is.null(y_breaks)){
      g <- g +
        scale_y_continuous(breaks = y_breaks) +
        coord_cartesian(ylim = c(min(y_breaks), max(y_breaks)))
    }

    g
  }

  if(!is.null(quota_lst)){
    # This will be the quotas for the last year by sector, in the same order
    # as `names_lst`
    quotas_last_yr_by_sector <- map_dbl(quota_lst, ~{
      .x |>
        dplyr::filter(Year == yrs[2]) |>
        pull(2)
    })
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
      dplyr::filter(year %in% yrs[1]:yrs[2]) |>
      mutate(year = factor(year)) |>
      mutate(catch = catch / scale) |>
      complete(year = year, month = 1:12, fill = list(catch = 0))

    if(ind != 1){
      leg_pos <- "none"
    }

    plot_single_panel(d,
                      type,
                      leg_pos = leg_pos,
                      title = names_lst[[ind]],
                      quota = ifelse(type == "quota",
                                     quotas_last_yr_by_sector[ind],
                                     NULL))
  })

  if(!is.null(disclaimer_text)){
    p[[length(p) + 1]] <- ggplot(data = tibble(x = 1, y = 1),
                                 aes(x, y)) +
      geom_text(x = 0,
                label = disclaimer_text,
                size = 4,
                hjust = 0) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank())
  }

  plt <- plot_grid(plotlist = p, ncol = 2, align = "v")

  y_axis_label <- switch(type,
                         "catch" = "Catch (kt)",
                         "proportion" = "Proportion of total catch",
                         "cumulative" = "Cumulative catch (kt)",
                         "quota" = "Proportion of sector quota")

  y_grob <- textGrob(y_axis_label,
                     gp = gpar(fontsize = axis_title_font_size),
                     rot = 90)

  grid.arrange(arrangeGrob(plt,
                           left = y_grob))
}
