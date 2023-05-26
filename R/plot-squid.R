#' Make the retrospective recruitment deviations plot (squid plot)
#'
#' @param model Model with retrospectives
#' @param show_ci Logical. If `TRUE`, plot the credible interval ribbons
#' around the median lines
#' @param ci_alpha The a transparency value for the credible interval
#' ribbon fill
#' @param ci_yrs A vector of years to include credible intervals for.
#' If `NULL`, all will be shown. Only used if `show_ci` is `TRUE`
#' @param axis_title_font_size Size of the font for the X and Y axis labels
#' @param axis_tick_font_size Size of the font for the X and Y axis tick labels
#' @param axis_label_color Color for the axis labels and tick labels
#' @param year_label_font_size Size of the font for the year labels
#'
#' @export
plot_squid <- function(model,
                       show_ci = FALSE,
                       ci_alpha = 0.2,
                       ci_yrs = NULL,
                       axis_title_font_size = 14,
                       axis_tick_font_size = 11,
                       axis_label_color = "black",
                       year_label_font_size = 4){

  model_lst <- c(list(model), map(model$retros, ~{.x}))
  end_yr <- model$endyr

  cohorts <- map_dbl(model_lst, \(mdl){
    mdl$endyr
  }) |>
    sort()

  model_nms <- rev(cohorts + 1)

  d_obj <- create_group_df_recr(model_lst,
                                model_nms,
                                devs = TRUE)

  d <- d_obj$d |>
    mutate(model = as.numeric(as.character(model))) |>
    filter(year %in% cohorts) |>
    split(~year) |>
    map(~{
      .x |>
        filter(year <= model) |>
        mutate(age = model - year)
    }) |>
    map_df(~{.x})

  g <- ggplot(d,
              aes(x = age,
                  y = devmed,
                  color = factor(year)))

  if(show_ci){
    dat <- d
    if(!is.null(ci_yrs[1])){
      dat <- dat |>
        filter(year %in% ci_yrs)
    }
    g <- g +
      geom_ribbon(data = dat,
                  aes(ymin = devlower,
                      ymax = devupper,
                      x = age,
                      fill = factor(year)),
                  alpha = ci_alpha,
                  linetype = "dotted",
                  linewidth = 0.5) +
      geom_segment(data = dat |>
                     filter(model == end_yr + 1) |>
                     mutate(year = factor(year)) |>
                     mutate(model = factor(model)),
                   aes(x = age,
                       xend = age,
                       y = devlower,
                       yend = devupper,
                       color = year),
                   linetype = "dotted",
                   linewidth = 0.5,
                   inherit.aes = FALSE)
  }

  g <- g +
    geom_hline(yintercept = 0,
               linewidth = 0.25) +
    geom_hline(yintercept = c(-2, 2),
               linetype = "dashed",
               linewidth = 0.1) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 3) +
    geom_text_repel(data = d |>
                      filter(model == end_yr + 1) |>
                      mutate(year = factor(year)) |>
                      mutate(model = factor(model)),
                    aes(x = age,
                        y = devmed,
                        label = year),
                    seed = 1,
                    size = year_label_font_size,
                    nudge_x = 0.5,
                    nudge_y = -0.25,
                    direction = "both") +
    scale_x_continuous(breaks = c(seq_along(cohorts),
                                  length(cohorts) + 1) - 1) +
    scale_y_continuous(breaks = seq(-3, 3)) +
    xlab("Age") +
    ylab("Recruitment deviation") +
  theme(legend.position = "none",
          axis.text.x = element_text(color = axis_label_color,
                                     size = axis_tick_font_size,
                                     angle = 0,
                                     hjust = 0.5,
                                     vjust = -0.25,
                                     face = "plain"),
          axis.text.y = element_text(color = axis_label_color,
                                     size = axis_tick_font_size,
                                     hjust = 1,
                                     vjust = 0.5,
                                     face = "plain"),
          axis.title.x = element_text(color = axis_label_color,
                                      size = axis_title_font_size,
                                      angle = 0,
                                      vjust = 0,
                                      face = "plain"),
          axis.title.y = element_text(color = axis_label_color,
                                      size = axis_title_font_size,
                                      angle = 90,
                                      face = "plain"),
        # plot.margin: top, right,bottom, left
        plot.margin = margin(0, 6, 6, 6))

  g
}
