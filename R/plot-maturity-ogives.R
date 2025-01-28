#' Create a plot containing the maturity ogives by year on a single panel
#' with inset for an area of interest to contain a detailed view
#'
#' @details
#' The package data frame `maturity_estimates_df` is used.
#' The 'Equilibrium' and 'Forecast' legend items are not part of the legend,
#' they are annotated text and have to be placed manually if you change the
#' size of this plot. use the arguments `eq_x_start_legend` and
#' `eq_y_start_legend` to do so.
#'
#' @param doy Day of year. This must be present in the
#' `hake::maturity_estimates_df` data frame unde the column `doy`.
#' @param show_inset Logical. If `TRUE`, show the inset panel
#' @param from The limits of the inset on the main plot to extract.
#' See [ggmagnify::geom_magnify()]
#' @param to The limits on the main panel of the location to place the
#' inset. See [ggmagnify::geom_magnify()]
#' @param x_breaks A vector of values on the x axis to show ticks for
#' @param x_label_every_nth Every nth value on the x axis, label the tick
#' mark. So if this is 2, label every second tick age on the x axis
#' @param vert_lines A vector of ages to draw vertical lines for, to aid
#' the eye. If `NULL`, draw no lines.
#' @param vert_lines_type Type of vertical lines if `vert_lines` is `TRUE`
#' @param vert_lines_color Color of vertical lines if `vert_lines` is `TRUE`
#' @param vert_lines_thickness Thickness of vertical lines if `vert_lines` is
#' `TRUE`
#' @param leg_font_size The legend font size
#' @param eq_line_color Color for the Equilibrium line
#' @param fore_line_color Color for the Forecast line
#' @param eq_x_start_legend X value where the legend for the Equilibrium line
#' and the Forecast line starts. Will likely change each year.
#' If `NULL`, the default of outside the plotting area, under the legend
#' will be used
#' @param eq_y_start_legend Y value where the legend for the Equilibrium line
#' and the Forecast line starts. Will likely change each year
#' @param eq_fore_alpha The transparency value for the Equilibrium and
#' Forecast lines
#' @param eq_fore_line_width The line width for the Equilibrium and
#' Forecast lines
#' @param leg_line_size_cm The legend line length in cm
#' @param space_between_legend_items_cm Vertical space in cm between the
#' legend items
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_maturity_ogives <- function(doy = 278,
                                 show_inset = TRUE,
                                 from = c(xmin = 2,
                                          xmax = 6.5,
                                          ymin = 0.8,
                                          ymax = 1),
                                 to = c(xmin = 5,
                                        xmax = 14,
                                        ymin = 0.05,
                                        ymax = 0.6),
                                 x_breaks = sort(unique(d$age)),
                                 x_label_every_nth = 5,
                                 vert_lines = 5,
                                 vert_lines_type = "dashed",
                                 vert_lines_color = "grey20",
                                 vert_lines_thickness = 0.5,
                                 eq_line_color = "black",
                                 fore_line_color = "red",
                                 eq_x_start_legend = NULL,
                                 eq_y_start_legend = 0.1,
                                 eq_fore_alpha = 0.7,
                                 eq_fore_line_width = 3,
                                 leg_line_size_cm = 1,
                                 leg_font_size = 14,
                                 space_between_legend_items_cm = 0.5){

  check_doy_val <- maturity_estimates_df |>
    dplyr::filter(doy == !!doy)

  if(!nrow(check_doy_val)){
    stop("Day of year `doy` value not found in the data frame ",
         "`hake::maturity_estimates_df`")
  }

  d <- maturity_estimates_df |>
    dplyr::filter(doy == !!doy) |>
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
    dplyr::filter(grepl("temperature", model)) |>
    mutate(line = case_when(year == min(year) ~ "equilibrium",
                            year == max(year) ~ "forecast",
                            TRUE ~ "main"))

  if(length(unique(d$model)) > 1){
    stop("There is more than one model being useed in this plot.")
  }

  d_fore <- d |>
    dplyr::filter(line == "forecast",
                  is_fore_yr)
  d_equil <- d |>
    dplyr::filter(line == "equilibrium")
  d <- d |>
    dplyr::filter(!line %in% c("forecast", "equilibrium"))

  #colors <- plot_color(length(unique(d$year)))
  colors <- rev(rich_colors_short(length(unique(d$year))))

  x_labels <- x_breaks
  x_labels[!x_breaks %% x_label_every_nth == 0] <- ""

  g <- d |>
    ggplot(aes(x = age,
               y = p_mature,
               group = as.factor(year),
               color = as.factor(year)),
           linetype = "dashed",
           linewidth = ts_linewidth)

  if(!is.null(vert_lines[1])){
    g <- g +
      geom_vline(xintercept = vert_lines,
                 linetype = vert_lines_type,
                 color = vert_lines_color,
                 linewidth = vert_lines_thickness)
  }

  g <- g +
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
    scale_x_continuous(breaks = x_breaks,
                       labels = x_labels) +
    xlab("Age (years)") +
    ylab("Probability of being mature") +
    theme(legend.key.size = unit(leg_line_size_cm, "cm"),
          legend.text = element_text(size = leg_font_size),
          legend.spacing.y = unit(space_between_legend_items_cm, "cm"),
          legend.title = element_text(size = leg_font_size + 2))

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
  symbol_x <- eq_x_start_legend %||% x_rng[2]
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
