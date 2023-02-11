plot_selex_mountains <- function(selex_by_yr,
                                 yrs = NULL,
                                 ages = NULL,
                                 scale = 3){

  yr_vec <- yrs[1]:yrs[2]

  if(!is.null(yrs)){
    selex_by_yr <- selex_by_yr |>
      filter(Yr %in% yr_vec)
  }
  if(!is.null(ages)){
    selex_by_yr <- selex_by_yr |>
      select(Yr, all_of(as.character(ages)))
  }

  d <- selex_by_yr |>
    pivot_longer(-Yr, names_to = "age", values_to = "prop") |>
    mutate(prop = prop * scale)

  d <- d |>
    filter(Yr %in% 2012) |>
    mutate(Yr = factor(Yr, levels = rev(sort(unique(Yr)))))


  # ggplot(d, aes(x = age, ymax = prop, group = Yr)) +
  #   geom_ribbon(ymin = 0, color = "black", fill = "lightgrey")  +

  xlist1 <- seq(0, 1 * scale, by = 0.02)
  #xlist1 <- sort(c(max(d$prop),tail(d$prop, 1),
  #                 pretty(c(0,min(d$prop)),n = 20, min.n = 20)))
  browser()
  ggplot(d, aes(x = age, y = prop, group = Yr)) +
    geom_line() +
    mapply(function(ylow, yhigh, col, a = 0.1){
      geom_ribbon(aes(ymin = ylow,
                      ymax = yhigh),
                  alpha = a,
                  fill = col)
    },
    tail(xlist1, -1),
    head(xlist1, -1),
    "salmon",
    seq(0, 1, length = length(xlist1) - 1)) +
    geom_line(size = 2, color = "white") +
    geom_line(size = 1.1) +
    geom_ribbon(aes(ymin = prop, ymax = Inf), fill = "white", alpha = 0.8) +
    facet_wrap(~Yr, ncol = 1) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(-2, "cm"),
          strip.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = margin(12, 12, 0, 0),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent")) +
    scale_y_continuous(expand = c(0, 0))

          # axis.text.x = element_text(color = "grey20",
          #                            size = axis_tick_font_size),
          # axis.title.x = element_text(color = "grey20",
          #                             size = axis_title_font_size),
          # axis.title.y = element_text(color = "grey20",
          #                             size = axis_title_font_size),
          # axis.ticks.length = unit(0.15, "cm"))

  # ggplot(d, aes(x = age,
  #               y = Yr,
  #               height = prop * scale,
  #               group = Yr,
  #               fill = prop)) +
  #   ggridges::geom_ridgeline(fill = "white") +
  #   ggpattern::geom_polygon_pattern(
  #     pattern_fill = "white",
  #     pattern_orientation = 2,
  #     pattern          = "gradient",
  #     pattern_fill2 = "black")

}