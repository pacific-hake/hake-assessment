#' Create a plot of median selectivity as a set of overlapping "mountains"
#' which are shaded top-down for visual appeal
#'
#' @param model The model output from Stock Synthesis as loaded by
#'   [create_rds_file()].
#' @param yrs A vector of years to include in the plot. If `NULL`, all years
#' found in the data will be included
#' @param ages A vector of ages to include in the plot
#' @param scale A scaling factor to increase or decrease the height of the
#' selectivity plots
#' @param fill_num_colors The number of colors for the gradient of colors
#' between `fill_col_upper` and `fill_col_lower`
#' @param fill_col_upper The top color for the gradient color scheme
#' @param fill_col_lower  The bottom color for the gradient color scheme
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_selex_mountains <- function(model,
                                 yrs = NULL,
                                 ages = 1:8,
                                 scale = 1,
                                 fill_num_colors = 20,
                                 fill_col_upper = "darkblue",
                                 fill_col_lower = "lightblue"){

  color_func <- colorRampPalette(colors = c(fill_col_upper, fill_col_lower))
  gradient_cols <- color_func(n = fill_num_colors)
  fill_reduction_prop <- 1 - (1 / fill_num_colors)
  prop_seq <- seq(0, fill_reduction_prop, 1 / fill_num_colors)

  sel_med <- model$extra_mcmc$sel_fishery_med |>
    select(-iter)

  if(!is.null(yrs)){
    sel_med <- sel_med |>
      dplyr::filter(yr %in% yrs)
  }

  if(!is.null(ages)){
    sel_med <- sel_med |>
      select(yr, all_of(as.character(ages)))
  }

  d <- sel_med |>
    pivot_longer(-yr, names_to = "age", values_to = "prop") |>
    mutate(prop = prop * scale) |>
    mutate(age = as.numeric(age)) |>
    arrange(-yr, age) |>
    rename(ymax = prop) |>
    split(~yr) |>
    rev() |>
    set_names(NULL) |>
    imap(~{.x |>
        mutate(ymin = .y)}) |>
    map_df(~{.x}) |>
    mutate(across(everything(), ~{as.numeric(.x)})) |>
    mutate(which_ribbon = 1) |>
    # The `add_extra_ribbon_data()` function is found in this file, below
    add_extra_ribbon_data(prop_seq) |>
    mutate(ymax = ymin + ymax) |>
    mutate(ribbon_color = gradient_cols[which_ribbon]) |>
    arrange(-yr, age)

  g <- ggplot(d,
              aes(x = age,
                  y = yr,
                  group = yr,
                  color = yr)) +
    geom_ribbon(aes(x = age,
                    ymax = ymax,
                    ymin = ymin,
                    group = which_ribbon,
                    fill = ribbon_color),
                alpha = 0.5,
                inherit.aes = FALSE) +
    scale_fill_identity() +
    scale_x_continuous(expand = c(0, 0),
                       breaks = ages) +
    coord_cartesian(xlim = c(0, max(ages))) +
    geom_line(data = d |>
                dplyr::filter(which_ribbon == 1),
              aes(x = age,
                  y = ymax,
                  group = yr),
              color = "white",
              linewidth = 0.75,
              alpha = 0.5,
              inherit.aes = FALSE) +
    geom_line(data = d |>
                dplyr::filter(which_ribbon == 1),
              aes(x = age,
                  y = ymax,
                  group = yr),
              color = "black",
              linewidth = 0.5,
              inherit.aes = FALSE) +
    geom_text(aes(x = 0.75,
                  y = ymin + 0.5,
                  hjust = 0.75,
                  vjust = -0.5,
                  label = yr),
              inherit.aes = FALSE) +
    facet_grid(yr ~ ., switch = "y") +
    theme(strip.background = element_blank(),
          panel.spacing = unit(-2, "cm"),
          strip.text.x = element_blank(),
          strip.text.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = margin(0, 6, 6, 12),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.position = "none") +
    xlab("Age") +
    ylab("Selectivity by year")

  g
}

#' Add extra data for decreasing-sized polygons which will be used to make
#' ribbons, one inside the other. See [plot_selex_mountains()]
#'
#' @param d The data frame to add ribbon data to
#' @param prop_seq A sequence of decreasing proportion values which
#' will be applied to the height data iteratively to create smaller
#' polygon data
#'
#' @return A modified data frame with the same structure as `d`, but with
#' many more rows which contain the extra ribbon polygon data
add_extra_ribbon_data <- function(d, prop_seq){

  prop_seq <- prop_seq[prop_seq != 0]
  prop_seq <- rev(sort(prop_seq))

  d |>
    split(~yr) |>
    map(~{
      out <- .x
      for(i in seq_along(prop_seq)){
        tmp <- .x |>
          mutate(ymax = ymax * prop_seq[i]) |>
          mutate(which_ribbon = i + 1)
        out <- out |>
          bind_rows(tmp)
      }
      out
    }) |>
    map_df(~{.x})
}
