#' Create a multi-panel plot of age proportion bubble plots, one for each
#' sector of the fishery
#'
#' @param age_data_lst A list of age proportion data frames
#' @param nms_vec A vector of names for the sectors, must be the same length
#' as `age_data-lst`
#' @param years A vector of two values re0-presenting the minimum and maximum
#' year values to plot
#' @param ax_title_font_size The font size for the axis titles and strip
#' titles (sector names)
#' @param ax_tick_font_size The foint size for the axis tick labels
#' (years and ages)
#' @param ax_label_color The color for the axis tick labels
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_age_comp_bubbles_data <- function(age_data_lst,
                                       nms_vec,
                                       years = c(year(now()) - 6,
                                                 year(now()) - 1)){

  if(length(age_data_lst) != length(nms_vec)){
    warning("`age_data_lst` is not the same length as `nms_vec`. Using ",
            "default panel titles")
    nms_vec <- as.list(paste0("Fishery ", seq(1, length(age_data_lst))))
  }
  colors <- plot_color(length(age_data_lst))

  years <- years[1]:years[2]

  df <- map2_dfr(age_data_lst, nms_vec, function(d, nm){

    ages <- names(d)
    ages <- sort(as.numeric(ages[!ages %in% c("year",
                                              "num_fish",
                                              "num_samples")]))

    d |>
      select(-c(num_fish, num_samples)) |>
      dplyr::filter(year %in% years) |>
      pivot_longer(-year) |>
      setNames(c("year", "age", "value")) |>
      mutate(type = nm) |>
      mutate(age = as.numeric(age)) |>
      mutate(year = factor(year, levels = years)) |>
      mutate(type = factor(type, levels = nms_vec)) |>
      complete(year = year,
               age = age,
               type = nm,
               fill = list(value = NA))
  })

  x_breaks <- seq(min(years), max(years), 1)
  x_labels <- x_breaks
  x_labels[x_breaks %% 2 == 1] <- ""

  g <- ggplot(df,
              aes(x = year,
                  y = age,
                  size = value,
                  fill = type)) +
    geom_point(pch = 21,
               color = "black") +
    scale_fill_manual(values = colors) +
    scale_x_discrete(breaks = x_breaks,
                     labels = x_labels) +
    facet_grid(~type, scales = "free_x") +
    xlab("Year") +
    ylab("Age") +
    theme(axis.text.x = element_text(color = axis_label_color,
                                     size = axis_tick_font_size,
                                     angle = 0,
                                     hjust = 0.5,
                                     vjust = -0.25),
          legend.position = "none",
          strip.background = element_rect(fill = "transparent"),
          strip.text = element_text(size = axis_title_font_size))

  g
}