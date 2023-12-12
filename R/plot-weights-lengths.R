#' Plot some years of weight of length histograms
#'
#' @param df A data frame as returned by [gfplot::tidy_lengths_raw()]
#' @param type Either "length" or "weight"
#' @param include_sex Logical. If `TRUE`, include the sex data in the plot,
#' with two bar colors
#' @param fill_col A named vector of two colors for Female and male bars.
#' The names are "F" and "M" respectively.
#' @param line_col A named vector of two colors for Female and male bar
#' outlines. The names are "F" and "M" respectively.
#' @param unsexed_fill_col The color to use for non-sex-specific bar fill
#' @param unsexed_col Thew color to use for the bar border for unsexed bars
#' @param min_total Minimum number of fish for a given survey and year needed
#' before a histogram is shown.
#' @param bin_width The width of the bars in units of the plot
#' @param x_breaks A vector of values to show on the x-axis as tick labels
#' @param label_font_size The font size for the sample size labels
#' @param unit_str A string representing the units, e.g. 'g' or 'kg'
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_weights_lengths <- function(df,
                                 type = c("length", "weight"),
                                 include_sex = TRUE,
                                 min_total = 20,
                                 bin_width = 0.075,
                                 x_breaks = pretty(d[[bin_col]], 4L),
                                 label_font_size = 2.25,
                                 fill_col = c("F" = rgb(1, 0, 0, 0.3),
                                              "M" = rgb(0, 0, 1, 0.3)),
                                 line_col = c("F" = rgb(1, 0, 0, 0.3),
                                              "M" = rgb(0, 0, 1, 0.3)),
                                 unsexed_fill_col = rgb(0, 0, 1, 0.3),
                                 unsexed_col = "black",
                                 unit_str = ifelse(type == "length", "cm", "kg")){

  type <- match.arg(type)

  bin_col <- paste0(type, "_bin")
  bin_col_sym <- sym(bin_col)

  if(include_sex){
    d <- df |>
      group_by(year, survey_abbrev) |>
      mutate(proportion = proportion / max(proportion)) |>
      ungroup() |>
      mutate(sex = factor(sex, levels = rev(sort(unique(sex))))) |>
      arrange(year, survey_abbrev, sex) |>
      mutate(proportion = ifelse(total >= min_total, proportion, NA))
  }else{
    d <- df |>
      group_by(year, survey_abbrev) |>
      mutate(proportion = proportion / max(proportion)) |>
      ungroup() |>
      arrange(year, survey_abbrev) |>
      mutate(proportion = ifelse(total >= min_total, proportion, NA))
  }

  range_lengths <- diff(range(d[[bin_col]], na.rm = TRUE))

  counts <- d |>
    group_by(survey_abbrev, year) |>
    summarize(total = first(total)) |>
    mutate(total = f(total)) |>
    ungroup()


  if(include_sex){
    g <- d |>
      ggplot(aes(!!bin_col_sym,
                 proportion)) +
      geom_col(aes(color = sex,
                   fill = sex),
               size = 0.3,
               width = bin_width,
               position = position_identity()) +
      labs(color = "Sex",
           fill = "Sex")
  }else{
    g <- d |>
      select(-sex) |>
      ggplot(aes(!!bin_col_sym,
                 proportion)) +
      geom_col(col = unsexed_col,
               fill = unsexed_fill_col,
               size = 0.3,
               width = bin_width,
               position = position_identity())
  }
  g <- g +
    geom_text(data = counts,
              aes(label = total),
              x = max(d[[bin_col]], na.rm = TRUE) - 0.2 * range_lengths,
              y = 0.75,,
              inherit.aes = FALSE,
              color = "black",
              size = label_font_size,
              hjust = 0)
  if(include_sex){
    g <- g +
      scale_fill_manual(values = fill_col, breaks = rev(names(fill_col))) +
      scale_color_manual(values = line_col, breaks = rev(names(fill_col)))
  }else{
    g <- g +
      scale_fill_manual(values = unsexed_fill_col) +
      scale_color_manual(values = unsexed_col)
  }
  g <- g +
    coord_cartesian(expand = FALSE) +
    scale_x_continuous(breaks = x_breaks) +
    ylim(-0.04, 1.07) +
    theme(panel.spacing = unit(-0.1, "lines"),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.background = element_rect("transparent", "transparent")) +
    facet_grid(forcats::fct_rev(as.factor(year)) ~ survey_abbrev,
               drop = FALSE,
               switch = "y") +
    ylab("")

  if(type == "length"){
    g <- g +
      xlab(paste0("Length (", unit_str, ")"))
  }else{
    g <- g +
      xlab(paste0("Weight (", unit_str, ")"))
  }

  g
}

