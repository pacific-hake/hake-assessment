#' Title
#'
#' @param model_lst A list of models, each created by [create_rds_file()]
#' @param model_names A vector of model names,the same length as `models_lst`
#' @param xlim The year limits to plot
#' @param x_breaks The year value tick marks to show for the x axis
#' @param ylim The depletion limits to plot
#' @param y_breaks The depletion value tick marks to show for the y axis
#' @param alpha The transparency for all ribbons
#' @param leg_pos The position of the legend inside the plot. If `NULL`,
#' `NA`, or `none`, the legend will not be shown
#' @param leg_font_size The legend font size
#' @param point_size Size of all points shownin plot
#' @param line_width Width of all lines on the plot
#'
#' @return a [ggplot2::ggplot()] object
#' @export
plot_biomass <- function(model_lst,
                         model_names,
                         xlim = c(model_lst[[1]]$startyr - 2,
                                  model_lst[[1]]$endyr + 1),
                         x_breaks = c(model_lst[[1]]$startyr,
                                      seq(1970, 2020, by = 5),
                                      model_lst[[1]]$endyr + 1),
                         ylim = c(0, 4.5),
                         y_breaks = seq(ylim[1], ylim[2], by = 0.5),
                         alpha = 0.1,
                         leg_pos = c(0.65, 0.83),
                         leg_font_size = 12,
                         point_size = 2,
                         line_width = 1){

  init_year <- model_lst[[1]]$startyr - 1

  bo <- map(model_lst, ~{
    .x$mcmccalcs$sinit
  }) |>
    map_dfr(~{.x}) |>
    mutate(model = model_names) |>
    mutate(year = init_year) |>
    select(model, year, everything()) |>
    setNames(c("model", "year", "slower", "smed", "supper")) |>
    mutate(model = factor(model, levels = model_names),
           year = as.numeric(year))

  d <- bind_cols(extract_mcmc_quant(model_lst, model_names, "slower", TRUE),
                 extract_mcmc_quant(model_lst, model_names, "smed"),
                 extract_mcmc_quant(model_lst, model_names, "supper")) |>
    mutate(model = factor(model, levels = model_names),
           year = as.numeric(year))

  colors <- plot_color(length(model_lst))

  g <- ggplot(d,
              aes(x = year,
                  y = smed,
                  ymin = slower,
                  ymax = supper,
                  group = model,
                  color = model,
                  fill = model)) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    coord_cartesian(xlim = xlim,
                    ylim = ylim) +
    geom_ribbon(alpha = alpha,
                linetype = "dashed") +
    geom_line(size = line_width) +
    geom_point(size = point_size) +
    scale_x_continuous(expand = c(0, 0),
                       breaks = x_breaks,
                       labels = x_breaks) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       labels = y_breaks) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = leg_font_size),
          # plot.margin: top, right,bottom, left
          # Needed to avoid tick labels cutting off
          plot.margin = margin(12, 12, 0, 0)) +
    xlab("Year") +
    ylab("Spawning Biomass (million t)")

  # Add B0 to the plot
  g <- g +
    geom_point(data = bo,
               size = point_size,
               position = position_dodge(1.5)) +
    geom_errorbar(data = bo,
                  size = line_width,
                  position = position_dodge(1.5))

  if(is.null(leg_pos) || is.na(leg_pos)){
    g <- g +
      theme(legend.position = "none")
  }else{
    g <- g +
      theme(legend.position = leg_pos)
  }

  g
}