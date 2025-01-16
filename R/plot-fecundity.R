#' Create a aplot of fecundity-at-age and mean-weight-at-age
#'
#' @param model A model, created by [create_rds_file()]
#' @param yrs A vector of the years to use in the calculations
#' @param leg_pos The position of the legend inside the plot. If `NULL`,
#' `NA`, or `none`, the legend will not be shown
#' @param leg_ncol The number of columns to show in the legend
#' @param leg_font_size The legend font size
#' @param axis_title_font_size Size of the font for the X and Y axis labels
#' @param axis_tick_font_size Size of the font for the X and Y axis tick labels
#' @param ... Arguments passed to [plot_maturity()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_fecundity <- function(model,
                           yrs = NULL,
                           line_colors = c("blue", "red"),
                           line_alpha = 0.5,
                           leg_pos = c(0.68, 0.13),
                           leg_ncol = 1,
                           leg_font_size = 14,
                           ...){

  d <- plot_maturity_ogives(model = model, ret_df = TRUE, ...)

  calc_mean <- function(fleet){
    x <- model$wtatage |>
      as_tibble() |>
      dplyr::filter(fleet == !!fleet)
    if(!is.null(yrs)){
      x <- x |>
        dplyr::filter(year %in% yrs)
    }
    x |>
      select(matches("^\\d")) |>
      apply(2, mean)
  }
  wt <- calc_mean(1)
  fec <- calc_mean(-2)
  age <- as.numeric(names(fec))
  d <- tibble(age = age,
              `Mean weight at age` = wt,
              `Mean fecundity (maturity-at-age X weight-at-age)` = fec) |>
    pivot_longer(-age) |>
    dplyr::filter(age > 0) |>
    mutate(name = factor(name, levels = unique(name)))

  age_max <- max(d$age)
  x_breaks <- seq_len(age_max)
  x_labels <- x_breaks
  x_labels[length(x_labels)] <- paste0(x_labels[length(x_labels)], "+")
  y_breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2)

  g <- ggplot(d,
              aes(x = age,
                  y = value,
                  group = name,
                  color = name)) +
    geom_line(linewidth = 1.5,
              alpha = line_alpha) +
    geom_hline(yintercept = y_breaks,
               linewidth = 0.5,
               alpha = 0.5,
               linetype = "dashed") +
    scale_x_continuous(breaks = x_breaks, labels = x_labels) +
    scale_y_continuous(breaks = y_breaks) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = leg_font_size),
          legend.text.align = 0) +
    labs(x = "Age",
         y = "Weight or fecundity (kg)") +
    scale_color_manual(values = line_colors)


  if(is.null(leg_pos[1]) || is.na(leg_pos[1])){
    g <- g +
      theme(legend.position = "none")
  }else{
    g <- g +
      theme(legend.position = leg_pos) +
      guides(fill = guide_legend(ncol = leg_ncol),
             color = guide_legend(ncol = leg_ncol,
                                  override.aes = list(size = 10,
                                                      linewidth = 1.5)))
  }

  g
}