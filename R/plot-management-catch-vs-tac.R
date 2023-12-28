#' Make a time series plot of the management-chosen TAC, assessment
#' estimated TAC, and realized catch
#'
#' @param d The data as read in using [readr::read_csv()] from the file
#' "catch-targets.csv"
#' @param curr_assess_biomass Current year's assessment-estimated biomass
#' (or any value). If `NULL` it will not be shown
#' @param line_type Which linetype to use for connecting the
#' values with vertical lines
#' @param line_width Width of the connecting lines
#' @param line_alpha Which alpha level (0-1) to use for connecting
#' the values with vertical lines
#' @param leg_pos The legend position as x-y coordinates (vector of 2)
#' @param leg_ncol The number of columns to split the legend into
#' @param leg_font_size The legend font size
#' @param point_size The point size
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_management_catch_vs_tac <- function(d,
                                         curr_assess_biomass = NULL,
                                         line_type = "solid",
                                         line_width = 1,
                                         line_alpha = 0.5,
                                         leg_pos = c(0.65, 0.83),
                                         leg_ncol = 1,
                                         leg_font_size = 12,
                                         point_size = 3){

  d <- d |>
    select(-c(Depletion, `Biomass estimate`)) |>
    filter(!is.na(`Default HCR TAC`))
  if(!is.null(curr_assess_biomass)){
    new_row <- c(max(d$Year) + 1, NA, NA, curr_assess_biomass)
    names(new_row) <- names(d)
    d <- bind_rows(d, new_row)
  }

  group_ord <- c("Default HCR TAC", "Total TAC", "Realized catch")

  d <- d |>
    pivot_longer(-Year) |>
    mutate(name = factor(name, levels = group_ord)) |>
    mutate(value = value / 1e3)

  x_min <- d$Year |> min()
  x_max <- d$Year |> max()
  x_breaks <- d$Year |> unique() |> sort()
  x_labels <- x_breaks
  x_labels[x_labels %% 2 == 1] <- ""

  g <- ggplot(d, aes(x = Year,
                     y = value,
                     color = name,
                     shape = name)) +
    geom_point(size = point_size) +
    geom_line(aes(group = name, color = name),
              linetype = line_type,
              linewidth = line_width,
              alpha = line_alpha) +
    labs(y = "Catch or TAC (kt)") +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = leg_font_size),
          # Following make the legend smaller and legend items closer together
          legend.key.size = unit(0.25, "cm"),
          legend.text.align = 0,
          legend.spacing.y = unit(0.01, "cm")) +
    guides(color = guide_legend(byrow = TRUE)) +
    scale_y_continuous(labels = comma,
                       limits = c(0, NA)) +
    scale_x_continuous(breaks = x_breaks,
                       labels = x_labels)

  if(is.null(leg_pos[1]) || is.na(leg_pos[1])){
    g <- g +
      theme(legend.position = "none")
  }else{
    g <- g +
      theme(legend.position = leg_pos) +
      guides(fill = guide_legend(ncol = leg_ncol),
             color = guide_legend(ncol = leg_ncol,
                                  label.hjust = 0),
             shape = guide_legend(label.hjust = 0))
  }

  g
}
