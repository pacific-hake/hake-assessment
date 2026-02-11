#' Plot the Canadian age by month and fleet
#'
#' @param x_lim Limits for the x-axis
#' @param x_breaks Breaks for the x-axis
#' @param y_lim Limits for the y-axis
#' @param yr Year of data to plot
#'
#' @returns A [ggplot2:ggplot()] object
#' @export
plot_age_by_month <- \(fleet_lst,
                       x_lim = c(1, 15),
                       x_breaks = seq(x_lim[1], x_lim[2], 2),
                       y_lim = c(0, 0.55),
                       yr = 2025){

  names(fleet_lst) <- c("Freezer trawlers", "Shoreside")
  # add proportion at age by year
  d <- imap(fleet_lst, \(fleet_df, fleet_nm){

    j <- fleet_df |>
      dplyr::filter(year == yr) |>
      select(month, age) |>
      dplyr::filter(!is.na(age)) |>
      split(~age) |>
      map(~{
        .x |>
          split(~month) |>
          map_df(~{.x |>
              mutate(count = nrow(.)) |>
              distinct()
            })
        }) |>
      bind_rows()|>
      split(~month) |>
      map_df(~{.x |> mutate(prop = count / sum(count))}) |>
      arrange(month, age) |>
      split(~month) |>
      map_df(~{.x |> mutate(prop = count / sum(count))}) |>
      mutate(fleet = fleet_nm)
  }) |>
    bind_rows()

  g <- ggplot(d,
              aes(x = age,
                  y = prop)) +
    geom_bar(stat = "identity",
             fill = main_fill,
             alpha = main_alpha) +
    scale_x_continuous(breaks = x_breaks,
                       labels = x_breaks) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(xlim = x_lim,
                    ylim = y_lim,
                    expand = TRUE) +
    facet_wrap(~ fleet + month) +
    xlab("") +
    ylab("Proportion by month") +
    ggtitle("") +
    theme(plot.title = element_text(hjust = 0.5))

  g
}
