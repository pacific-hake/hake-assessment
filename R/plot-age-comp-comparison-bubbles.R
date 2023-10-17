#' Create a plot of age proportions as bubbles with fishery and survey values
#' overlaid in one plot
#'
#' @param model A model object
#' @param alpha Transparanecy of the bubbles
#' @param ax_title_font_size The font size for the axis titles and strip
#' titles (sector names)
#' @param ax_tick_font_size The foint size for the axis tick labels
#' (years and ages)
#' @param ax_label_color The color for the axis tick labels
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_age_comp_comparison_bubbles <- function(model,
                                             alpha = 0.3){

  d <- model$dat$agecomp |>
    as_tibble() |>
    select(-c(Seas, Gender, Part, Ageerr, Lbin_lo, Lbin_hi, Nsamp)) |>
    rename(year = Yr)
  ages <- as.numeric(gsub("a", "", grep("a[0-9]+", names(d), value = TRUE)))

  d_fishery <- d |>
    filter(FltSvy == 1) |>
    select(-FltSvy) |>
    setNames(c("year", ages)) |>
    pivot_longer(-year) |>
    setNames(c("year", "age", "value")) |>
    mutate(age = as.numeric(age)) |>
    mutate(type = "Fishery") |>
    group_by(year) |>
    mutate(value = value / sum(value)) |>
    ungroup()
  d_survey <- d |>
    filter(FltSvy == 2) |>
    select(-FltSvy) |>
    setNames(c("year", ages)) |>
    pivot_longer(-year) |>
    setNames(c("year", "age", "value")) |>
    mutate(age = as.numeric(age)) |>
    mutate(type = "Survey") |>
    group_by(year) |>
    mutate(value = value / sum(value)) |>
    ungroup()

  years <- sort(intersect(d_fishery$year, d_survey$year))
  years_inc <- years[1]:years[length(years)]

  d_fishery <- d_fishery |>
    mutate(year = factor(year, levels = years_inc)) |>
    filter(year %in% years) |>
    complete(year = year,
             age = age,
             type = type,
             fill = list(value = NA))

  d_survey <- d_survey |>
    mutate(year = factor(year, levels = years_inc)) |>
    filter(year %in% years) |>
    complete(year = year,
             age = age,
             type = type,
             fill = list(value = NA))

  d <- map_dfr(list(d_fishery,
                    d_survey), ~{.x})
  years_labels <- gsub(2011, "", years)
  years_labels <- gsub("2013", "", years_labels)

  ages <- sort(unique(d$age))

  colors <- c("red", "blue")

  g <- ggplot(d,
              aes(x = year,
                  y = age,
                  size = value,
                  fill = type)) +
     geom_point(pch = 21,
                alpha = alpha) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(breaks = ages) +
    scale_x_discrete(breaks = years,
                     labels = years_labels) +
    xlab("Year") +
    ylab("Age") +
    theme(axis.text.x = element_text(color = axis_label_color,
                                     size = axis_tick_font_size,
                                     angle = 0,
                                     hjust = 0.5,
                                     vjust = -0.25),
          plot.title = element_text(size = axis_title_font_size,
                                    hjust = 0.5),
          legend.position = "top",
          legend.title = element_blank(),
          strip.background = element_rect(fill = "transparent"),
          strip.text = element_text(size = axis_title_font_size)) +
    guides(fill = guide_legend(override.aes = list(size = 7)))

  g
}
