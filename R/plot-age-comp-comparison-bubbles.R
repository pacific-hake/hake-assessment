#' Create a plot of age proportions as bubbles with fishery and survey values
#' overlaid in one plot
#'
#' @param model A model object
#' @param alpha Transparanecy of the bubbles
#' @param remove_yr_labels A vector of years to remove the ,labels for in
#' case they are overlapping
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_age_comp_comparison_bubbles <- function(model,
                                             alpha = 0.3,
                                             remove_yr_labels = NULL){

  d <- model$dat$agecomp |>
    as_tibble() |>
    select(-c(month, sex, part, ageerr, Lbin_lo, Lbin_hi, Nsamp))

  ages <- as.numeric(gsub("a", "", grep("a[0-9]+", names(d), value = TRUE)))

  d_fishery <- d |>
    dplyr::filter(fleet == 1) |>
    select(-fleet) |>
    setNames(c("year", ages)) |>
    pivot_longer(-year) |>
    setNames(c("year", "age", "value")) |>
    mutate(age = as.numeric(age)) |>
    mutate(type = "Fishery") |>
    group_by(year) |>
    mutate(value = value / sum(value)) |>
    ungroup()
  d_survey <- d |>
    dplyr::filter(fleet == 2) |>
    select(-fleet) |>
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
    dplyr::filter(year %in% years) |>
    complete(year = year,
             age = age,
             type = type,
             fill = list(value = NA))

  d_survey <- d_survey |>
    mutate(year = factor(year, levels = years_inc)) |>
    dplyr::filter(year %in% years) |>
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

  x_breaks <- unique(d$year) |> as.character() |> as.numeric()
  x_labels <- x_breaks
  actual_yrs <- d |>
    mutate(year = as.character(year),
           year = as.numeric(year)) |>
    dplyr::filter(!is.na(value)) |>
    pull(year) |>
    unique()
  x_labels[!x_labels %in% actual_yrs] <- ""
  if(!is.null(remove_yr_labels)){
    x_labels[x_labels %in% remove_yr_labels] <- ""
  }

  g <- ggplot(d,
              aes(x = year,
                  y = age,
                  size = value,
                  fill = type)) +
     geom_point(pch = 21,
                alpha = alpha) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(breaks = ages) +
    scale_x_discrete(breaks = x_breaks,
                     labels = x_labels) +
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
