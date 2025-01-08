#' Create a plot with three panels showing a summary of the data used in the
#' assessment as proportional bubbles
#'
#' @param model A model, created by [create_rds_file()]
#' @param ax_title_font_size Size of the font for the X and Y axis labels
#' @param ax_tick_font_size Size of the font for the X and Y axis tick labels
#' @param label_y_placement A vector of three values, one for each of the
#' panel labels, Catches, Abundance Indices, and Age Compositions respectively
#' which represent the y value to place the labels at
#' @param x_breaks The year tick marks to show for the x axis
#'
#' @return a [ggplot2::ggplot()] object
#' @export
plot_data_summary <- function(model,
                              label_y_placement = c(1.8, 2.5, 2.5),
                              x_breaks = seq(model$startyr - 1,
                                             model$endyr,
                                             by = 5)){

  start_yr <- model$startyr
  end_yr <- model$endyr

  ct <- model$catch |>
    as_tibble() |>
    transmute(yr = Yr, val = Obs) |>
    dplyr::filter(yr %in% start_yr:end_yr) |>
    mutate(val = val / sum(val)) |>
    transmute(type = "Catches",
           fleet = "Fishery",
           yr,
           val)

  cpue <- model$cpue |>
    as_tibble() |>
    dplyr::filter(Use > 0) |>
    transmute(yr = Yr,
              fleet = Fleet_name,
              val = Exp,
              se = SE) |>
    dplyr::filter(yr %in% start_yr:end_yr) |>
    split(~fleet) |>
    map_df(~{
      .x |>
        mutate(val = val / mean(val)) |>
        select(-se)
      }) |>
    mutate(val = val / sum(val)) |>
    mutate(fleet = ifelse(fleet == "Acoustic_Survey",
                          "Acoustic Survey",
                          "Age 1 Survey")) |>
    transmute(fleet, yr, val) %>%
    mutate(fleet = factor(fleet, levels = rev(unique(.[["fleet"]]))))

  agedbase <- model$agedbase |>
    as_tibble() |>
    transmute(yr = Yr, fleet = Fleet, val = Nsamp_adj) |>
    mutate(fleet = ifelse(fleet == 1,
                          "Fishery",
                          "Acoustic Survey")) |>
    dplyr::filter(yr %in% start_yr:end_yr) |>
    split(~fleet) |>
    map_df(~{
      .x |>
        split(~yr) |>
        map_df(~{.x |> slice(1)})
      }) |>
    group_by(fleet) |>
    summarize(yr, val = val / sum(val)) %>%
    mutate(fleet = factor(fleet, levels = unique(.[["fleet"]])))

  plotter <- function(obj,
                      cols = "blue",
                      label = "",
                      label_y,
                      show_x_axis = FALSE){

    # Needed to make `annotate` text boldface. Need to have `parse = TRUE`
    # in the `annotate()` call for it to work
    label <- paste0("atop(bold('", label, "'))")

    g <- ggplot(obj,
                aes(x = yr,
                    y = fleet,
                    size = val,
                    fill = fleet)) +
      geom_point(pch = 21,
                 color = "black") +
      scale_x_continuous(breaks = x_breaks,
                         limits = c(start_yr, end_yr)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.margin = unit(c(0, 0, 1, 0), "lines"),
            legend.position = "none") +
      scale_fill_manual(values = cols) +
      scale_y_discrete(position = "right",
                       expand = c(0, 2)) +
      annotate("text",
               label = label,
               parse = TRUE,
               x = (end_yr - start_yr) / 2 + start_yr,
               y = label_y,
               size = 6,
               colour = axis_label_color)

    if(!show_x_axis){
      g <- g +
        theme(axis.text.x = element_blank(),
              axis.title.x = element_blank())
    }

    g
  }

  p <- list()
  p[[1]] <- plotter(ct,
                    cols = "blue",
                    label = "Catches",
                    label_y = label_y_placement[1])
  p[[2]] <- plotter(cpue,
                    cols = c( "green", "red"),
                    label = "Abundance Indices",
                    label_y = label_y_placement[2])
  p[[3]] <- plotter(agedbase,
                    cols = c("red", "blue"),
                    label = "Age Compositions",
                    label_y = label_y_placement[3],
                    show_x_axis = TRUE)

  plt <- plot_grid(plotlist = p, ncol = 1, align = "v")

  x_grob <- textGrob("Year",
                     gp = gpar(fontsize = axis_title_font_size))

  grid.arrange(arrangeGrob(plt, bottom = x_grob))
}
