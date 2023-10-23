#' Create a multi-panel plot of age proportion bubble plots, one for each
#' sector of the fishery
#'
#' @param age_data_lst A list of age proportion data frames
#' @param names_lst A list of names for the sectors, must be the same length
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
                                       names_lst,
                                       years = c(year(now()) - 6,
                                                 year(now()) - 1)){

  if(length(age_data_lst) != length(names_lst)){
    warning("`names_lst` is not the same length as `names_lst. Using ",
            "default panel titles")
    names_lst <- as.list(paste0("Fishery ", seq(1, length(catch_lst))))
  }
  colors <- plot_color(length(age_data_lst))

  years <- years[1]:years[2]

  df <- imap(age_data_lst, function(d, ind){

    # Put all data frames are now in exactly the same format
    if(is.matrix(d)){
      # Canadian format
      yrs <- rownames(d) |> as.numeric()
      d <- d |>
        as_tibble() |>
        mutate(year = yrs) |>
        select(year, everything())
    }else{
      # US format
      yrs <- d$year
      d <- d |>
        select(-year, -starts_with("n."))
      nms <- gsub("a", "", names(d))
      d <- d |>
        mutate(year = yrs)  |>
        select(year, everything()) |>
        setNames(c("year", nms))
    }

    # Put into long format for ggplot, and add the fishery name
    d |>
      pivot_longer(-year) |>
      setNames(c("year", "age", "value")) |>
      mutate(type = names_lst[[ind]]) |>
      mutate(age = as.numeric(age)) |>
      mutate(year = factor(year, levels = years)) |>
      mutate(type = factor(type, levels = names_lst)) |>
      filter(year %in% years) |>
      complete(year = year,
               age = age,
               type = type,
               fill = list(value = NA))
  }) |>
    map_dfr(~{.x})

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