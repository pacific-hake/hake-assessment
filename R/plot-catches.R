#' Plot catch data as a stacked barplot
#'
#' @param ct The data frame which is read in from `landings-tac-history.csv`
#' @param leg_font_size The legend font size
#' @param leg_key_size The size in cm of the bars of color in the legend
#' @param leg_ratio A vector of length two, for the ratio of the legend size
#' vs the plot size respectively. Default is c(1, 2). You will likely need to
#' change this if changing `leg_key_size` and/or `leg_font_size`
#' @param clip_cover There is a white rectangle drawn on top of the plot
#' to cover any of the plot that made it outside the plot area. `clip` has to
#' be set to `off` for the major x-axis tick marks to work, So, this is required.
#' If you make the plot in a grid, the rectangle may overwrite some of the plot
#' above it, and this number will have to be changed through trial and error
#' until you cannot see the white rectangle anymore.
#' @param xlim The year limits to plot
#' @param x_breaks The year value tick marks to show for the x axis
#' @param x_expansion Amount to expand the x axis. See the `expand` argument
#' in [ggplot2::scale_x_continuous()]
#' @param x_labs_mod Value for major X-axis tick marks. Every Nth tick
#' will be longer and have a label. The first and last will be shown
#' regardless of what this number is
#' @param ylim The y-axis min and max limits for the plot
#' @param y_breaks The tick mark values to show for the y axis
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_catches <- function(ct,
                         leg_font_size = 12,
                         leg_key_size = 0.7,
                         leg_ratio = c(1, 2),
                         clip_cover = 2,
                         xlim = c(1966, year(Sys.time())),
                         x_breaks = xlim[1]:xlim[2],
                         x_expansion = 1,
                         x_labs_mod = 5,
                         ylim = c(0, 450),
                         y_breaks = seq(0, 400, 100)){

  fishery_nms <- c(
    "U.S. Joint-venture",
    "U.S. Foreign",
    "Canada Joint-venture",
    "Canada Foreign",
    "Canada Freezer-trawler",
    "Canada Shoreside",
    "U.S. Shoreside",
    "U.S. Catcher-processor",
    "U.S. Mothership")

  fishery_colors <- c(
    rgb(0, 0.4, 0),
    rgb(0, 0.2, 0),
    #rgb(0, 0.6, 0),
    #rgb(0, 0.8, 0),
    rgb(0.79, 0.2, 0.9),
    rgb(0.79, 0.6, 0.8),
    rgb(0.4, 0, 0),
    rgb(0.8, 0, 0),
    rgb(0, 0, 1),
    rgb(0, 0, 0.4),
    rgb(0, 0, 0.7))

  # The quoted spaces here are to create fake factors so that when the legend
  # is constructed, there will be blank spaces in it. That way there are
  # three columns, each with a different number of items
  fishery_nms_f <- c(fishery_nms[1:6],
                     " ",
                     "  ",
                     fishery_nms[7:9],
                     "   ")

  d <- enframe(fishery_nms, name = NULL) |>
    set_names(c("fishery"))

  ct <- ct |>
    select(Year,
           all_of(fishery_nms)) |>
    mutate(across(-Year, ~{.x / 1e3})) |>
    pivot_longer(-Year, names_to = "fishery") |>
    left_join(d, by = "fishery")

  # Add dummy rows for blank legend entries
  dum <- vec2df(c(1966, " ", 0), nms = names(ct)) |>
    mutate(Year = as.numeric(Year),
           value = as.numeric(value))
  dum2 <- vec2df(c(1966, "  ", 0), nms = names(ct)) |>
    mutate(Year = as.numeric(Year),
           value = as.numeric(value))
  dum3 <- vec2df(c(1966, "   ", 0), nms = names(ct)) |>
    mutate(Year = as.numeric(Year),
           value = as.numeric(value))

  ct <- ct |>
    bind_rows(dum) |>
    bind_rows(dum2) |>
    bind_rows(dum3) |>
    mutate(fishery = factor(fishery,
                            levels = fishery_nms_f))

  x_labels <- make_major_tick_labels(x_breaks = x_breaks,
                                     modulo = x_labs_mod)

  d <- ct |>
    mutate(ord = case_when(fishery == "Canada Joint-venture" ~ 1,
                           fishery == "Canada Foreign" ~ 2,
                           fishery == "U.S. Joint-venture" ~ 3,
                           fishery == "U.S. Foreign" ~ 4,
                           fishery == "Canada Freezer-trawler" ~ 5,
                           fishery == "Canada Shoreside" ~ 6,
                           fishery == " " ~ 7,
                           fishery == "  " ~ 8,
                           fishery == "   " ~ 9,
                           fishery == "U.S. Shoreside" ~ 10,
                           fishery == "U.S. Catcher-processor" ~ 11,
                           fishery == "U.S. Mothership" ~ 12))

  d <- d[order(d$ord), ]
  g <-
    ggplot(d,
           aes(x = Year,
               y = value,
               fill = fishery)) +
    #fill = forcats::fct_reorder(fishery, ord))) +
  geom_hline(yintercept = y_breaks,
               linetype = "dashed",
               linewidth = 0.25) +
    geom_col(color = "transparent") +
    scale_fill_manual(values = c(fishery_colors[1:6],
                                 "transparent",
                                 "transparent",
                                 fishery_colors[7:9],
                                 "transparent")) +
    scale_x_continuous(expand = c(0, x_expansion),
                       breaks = x_breaks,
                       labels = x_labels) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks) +
    coord_cartesian(xlim = xlim,
                    ylim = ylim,
                    clip = "off") +
    theme(legend.position = "none",
          # These two commands move the x-axis major tick labels and axis
          # title down so that the ticks. tick labels, and axis title don't
          # overlap each other
          axis.text.x = element_text(vjust = -2),
          axis.title.x = element_text(vjust = -2)) +
    ylab("Catch (kt)")

  # Add major tick marks
  g <- g |>
    add_major_ticks(x_breaks = x_breaks,
                    modulo = x_labs_mod,
                    # This proportion must be set by trial and error
                    # Make sure to change `vjust` value above in the `theme()`
                    # call so the labels are not overlapping the lines or
                    # too far away from the lines
                    prop = 1.2)

  # Draw a white rectangle over the top of the plot, obscuring any
  # unclipped plot parts. Clipping has to be off to allow different size
  # tick marks. `grid` package used here
  g <- g +
    annotation_custom(grob = rectGrob(gp = gpar(col = NA,
                                                fill = "white")),
                      xmin = xlim[1],
                      xmax = xlim[2],
                      ymin = ylim[2],
                      ymax = ylim[2] + clip_cover)

  legend <- get_plot_component(
    g + guides(fill = guide_legend(nrow = 4)) +
      theme(legend.position = "top",
            legend.direction = "vertical",
            legend.title = element_blank(),
            legend.text.align = 0,
            legend.text = element_text(size = leg_font_size),
            legend.key.size = unit(leg_key_size, "cm")),
    pattern = "guide-box-top",
    return_all = TRUE)

  p <- plot_grid(legend,
                 g,
                 ncol = 1,
                 rel_heights = leg_ratio)

  p
}
