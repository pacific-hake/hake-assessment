#' Plot recruitment deviations from MCMC output for one or more models
#'
#' @param model_lst A list of models, each created by [create_rds_file()]
#' @param model_names A vector of model names,the same length as `models_lst`
#' @param xlim The year limits to plot
#' @param x_breaks The year value tick marks to show for the x axis
#' @param ylim The depletion limits to plot
#' @param y_breaks The depletion value tick marks to show for the y axis
#' @param y_labels The depletion labels to show for the y axis tick marks
#' @param y_colors The color vector for each label for the y axis tick marks
#' @param alpha The transparency for all ribbons
#' @param leg_pos The position of the legend inside the plot. If `NULL`,
#' `NA`, or `none`, the legend will not be shown
#' @param leg_font_size The legend font size
#' @param point_size Size of all points shownin plot
#' @param line_width Width of all lines on the plot
#' @param dodge_val The amount to offset the lines from each other in the
#' case of multiple models
#'
#' @return a [ggplot2::ggplot()] object
#' @export
plot_recdevs <- function(model_lst,
                         model_names,
                         xlim = c(min(model_lst[[1]]$recruit$Yr),
                                  model_lst[[1]]$endyr + 1),
                         x_breaks = c(min(model_lst[[1]]$recruit$Yr),
                                      seq(
                                        round(min(model_lst[[1]]$recruit$Yr) + 10, -1),
                                        # Current decade, i.e. 2020
                                        round(lubridate::year(Sys.time()) - 10, -1),
                                        by = 10),
                                      model_lst[[1]]$endyr + 1),
                         ylim = c(-4, 4),
                         y_breaks = seq(ylim[1], ylim[2], by = 2),
                         y_labels = expression("-4", "-2", "0", "2", "4"),
                         y_colors = c("black", "black", "blue", "black", "black"),
                         alpha = 0.1,
                         leg_pos = c(0.65, 0.83),
                         leg_font_size = 12,
                         point_size = 1.5,
                         line_width = 0.5,
                         dodge_val = 0.5){

  d <- bind_cols(extract_mcmc_quant(model_lst, model_names, "devlower", TRUE),
                 extract_mcmc_quant(model_lst, model_names, "devmed"),
                 extract_mcmc_quant(model_lst, model_names, "devupper")) |>
    mutate(model = factor(model, levels = model_names),
           year = as.numeric(year))

  colors <- plot_color(length(model_lst))

  g <- ggplot(d,
              aes(x = year,
                  y = devmed,
                  ymin = devlower,
                  ymax = devupper,
                  group = model,
                  color = model,
                  fill = model)) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    coord_cartesian(xlim = xlim,
                    ylim = ylim) +
    geom_hline(yintercept = 0,
               color = "blue",
               linetype = "dotted",
               size = 1) +
    geom_point(size = point_size,
               position = position_dodge(dodge_val)) +
    geom_errorbar(size = line_width,
                  position = position_dodge(dodge_val)) +
    scale_x_continuous(expand = c(0, 0),
                       breaks = x_breaks,
                       labels = x_breaks) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       labels = y_labels) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = leg_font_size),
          axis.text.y = element_text(color = y_colors),
          # plot.margin: top, right,bottom, left
          # Needed to avoid tick labels cutting off
          plot.margin = margin(12, 12, 0, 0)) +
    xlab("Year") +
    ylab("Recruitment deviations")

  if(is.null(leg_pos[1]) || is.na(leg_pos[1])){
    g <- g +
      theme(legend.position = "none")
  }else{
    g <- g +
      theme(legend.position = leg_pos)
  }

  g
}