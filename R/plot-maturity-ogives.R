#' Create a plot containing the maturity ogives by year on a single panel
#' with inset for an area of interest to contain a detailed view
#'
#' @details
#' The package data frame `maturity_estimates_df` is used.
#'
#' @param show_inset Logical. If `TRUE`, show the inset panel
#' @param from The limits of the inset on the main plot to extract.
#' See [ggmagnify::geom_magnify()]
#' @param to The limits on the main panel of the location to place the
#' inset. See [ggmagnify::geom_magnify()]
#' @param leg_font_size The legend font size
#' @param eq_line_color Color for the Equilibrium line
#' @param fore_line_color Color for the Forecast line
#' @param eq_y_start_legend Y value where the legend for the Equilibrium line
#' and the Forecast line starts. Will likely change each year
#' @param eq_fore_alpha The transparency value for the Equilibrium and
#' Forecast lines
#' @param eq_fore_line_width The line width for the Equilibrium and
#' Forecast lines
#' @param leg_line_size_cm The legend line length in cm
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_maturity_ogives <- function(show_inset = TRUE,
                                 from = c(xmin = 3,
                                          xmax = 6.5,
                                          ymin = 0.75,
                                          ymax = 1),
                                 to = c(xmin = 8,
                                        xmax = 14,
                                        ymin = 0.05,
                                        ymax = 0.6),
                                 eq_line_color = "black",
                                 fore_line_color = "red",
                                 eq_y_start_legend = 0.1,
                                 eq_fore_alpha = 0.7,
                                 eq_fore_line_width = 3,
                                 leg_line_size_cm = 1,
                                 leg_font_size = 14){

  d <- maturity_estimates_df |>
    group_by(model) |>
    mutate(is_fore_yr = year %in% ((max(year) - 4):max(year))) |>
    group_by(model, is_fore_yr, age) |>
    group_modify(~ .x |>
                   add_row(year = 9999,
                           p_mature = mean(.x$p_mature,
                                           na.rm = TRUE))) |>
    group_by(model, age) |>
    group_modify(~ .x |>
                   add_row(
                     year = 0,
                     p_mature = mean(.x$p_mature[.x$year != 9999],
                                     na.rm = TRUE))) |>
    ungroup() |>
    filter(grepl("temperature", model)) |>
    mutate(line = case_when(year == min(year) ~ "equilibrium",
                            year == max(year) ~ "forecast",
                            TRUE ~ "main"))

  if(length(unique(d$model)) > 1){
    stop("There is more than one model being useed in this plot.")
  }

  d_fore <- d |>
    filter(line == "forecast",
           is_fore_yr)
  d_equil <- d |>
    filter(line == "equilibrium")
  d <- d |>
    filter(!line %in% c("forecast", "equilibrium"))

  #colors <- plot_color(length(unique(d$year)))
  colors <- rev(rich_colors_short(length(unique(d$year))))

  g<- d |>
    ggplot(aes(x = age,
               y = p_mature,
               group = as.factor(year),
               color = as.factor(year)),
           linetype = "dashed",
           linewidth = ts_linewidth) +
    geom_vline(xintercept = 5,
               linetype = "dashed",
               color = "grey20") +
    geom_line(data = d_equil,
              linewidth = eq_fore_line_width,
              color = eq_line_color,
              alpha = eq_fore_alpha) +
    geom_line(data = d_fore,
              linewidth = eq_fore_line_width,
              color = fore_line_color,
              alpha = eq_fore_alpha) +
    geom_line() +
    scale_color_manual(values = colors,
                       labels = sort(unique(d$year)),
                       name = "Year") +
    xlab("Age (years)") +
    ylab("Probability of being mature") +
    theme(legend.key.size = unit(leg_line_size_cm, "cm"),
          legend.text = element_text(size = leg_font_size),
          legend.title=element_text(size = leg_font_size + 2))

  if(show_inset){
    g <- g +
      geom_magnify(from = from,
                   to = to,
                   shadow = TRUE,
                   colour = "black",
                   linewidth = 0.5,
                   proj.linetype = 3,
                   shadow.args = list(colour = "black",
                                      sigma = 10,
                                      x_offset = 2,
                                      y_offset = 5))
  }

  # "Legend" is manually made and has three entries each with text and
  # one or more symbols
  lyr <- layer_scales(g)
  x_rng <- lyr$x$range$range
  y_rng <- lyr$y$range$range
  symbol_x <- x_rng[2]
  symbol_x_start <- symbol_x + 1.5
  symbol_x_end <- symbol_x_start + 1
  text_x <- symbol_x_end + 0.25
  symbol_y <- eq_y_start_legend
  between_eq_for_y_offset <- 0.04

  g <- g +
    coord_cartesian(xlim = range(d$age),
                    clip = "off") +
    theme(plot.margin = margin(0, 30, 6, 6)) +
    # First symbol
    annotate("segment",
             x = symbol_x_start,
             xend = symbol_x_end,
             y = symbol_y,
             yend = symbol_y,
             colour = eq_line_color,
             alpha = eq_fore_alpha,
             linewidth = eq_fore_line_width) +
    #Second symbol
    annotate("segment",
             x = symbol_x_start,
             xend = symbol_x_end,
             y = symbol_y - between_eq_for_y_offset,
             yend = symbol_y - between_eq_for_y_offset,
             colour = fore_line_color,
             alpha = eq_fore_alpha,
             linewidth = eq_fore_line_width) +
      # First text
    annotate("text",
             x = text_x,
             y = symbol_y,
             label = "Equilibrium",
             size = leg_font_size / .pt,
             hjust = 0) +
    # Second text
    annotate("text",
             x = text_x,
             y = symbol_y - between_eq_for_y_offset,
             label = "Forecast",
             size = leg_font_size / .pt,
             hjust = 0)

  g
}
