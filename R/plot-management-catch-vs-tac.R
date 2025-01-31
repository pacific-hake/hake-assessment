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
#' @param ret_tbl Logical. If `TRUE`, return a [kableExtra::kbl()] containing
#' the outputs instead of the [ggplot2::ggplot()] object
#' @param font_size The table data and header font size in points
#' @param right_cols_cm The number of centimeters wide to make all of the
#' rightmost columns (all the value columns)
#' @param y_breaks A vector of values to show on the y-axis
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
                                         point_size = 3,
                                         ret_tbl = FALSE,
                                         font_size = 10,
                                         right_cols_cm = 1.8,
                                         y_breaks = seq(0, 1000, 100),
                                         ...){

  d <- d |>
    select(-c(Depletion, `Biomass estimate`)) |>
    dplyr::filter(!is.na(`Default HCR TAC`))
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

  if(ret_tbl){

    d <- d |>
      pivot_wider(names_from = "name", values_from = "value") |>
      mutate(across(-Year, ~{f(.x)}))
    k <- kbl(d,
             format = "latex",
             booktabs = TRUE,
             align = c("l",
                       rep(paste0("R{",
                                  right_cols_cm,
                                  "cm}"),
                           ncol(d) - 1)),
             linesep = "",
             escape = FALSE,
             ...) |>
      row_spec(0, bold = TRUE) |>
      kable_styling(font_size = font_size,
                    latex_options = c("repeat_header"))

    return(k)
  }
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
                       limits = c(0, NA),
                       breaks = y_breaks) +
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
