#' Create a plot showing the density of the posterior for the catch forecast
#' for a given year
#'
#' @param model The model output from Stock Synthesis as loaded by
#' [create_rds_file()].
#' @param yr Forecast year to plot. Must be a column in the MCMC output
#' @param xlim A vector of two values. The x-axis minimum and maximum values
#' @param fill_color The color used to fill under the density curve
#' @param fill_alpha_tail The transparency used for the color of the tails
#' @param fill_alpha_main The transparency used for the color of the main part
#' of the curve (-1SD - 1SD)
#' @param ... Arguments to pass to [theme_axis_text()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_catch_forecast_density <- function(model,
                                        yr,
                                        xlim = c(0, 4),
                                        fill_color = ts_single_model_ribbon_fill,
                                        fill_alpha_tail = 0.3,
                                        fill_alpha_main = 0.7,
                                        ...){

  col <- paste0("ForeCatch_", yr)
  col_sym <- sym(col)

  d <- model$mcmc |>
    as_tibble() |>
    mutate(!!col_sym := !!col_sym / 1e6) |>
    select(!!col_sym) |>
    rename(value = !!col_sym)

  quants <- quantile(d |> pull(), probs = probs)

  g <- ggplot(d) +
    geom_density(aes(x = value),
                 fill = fill_color,
                 alpha = fill_alpha_tail)
  gb <-  ggplot_build(g)$data[[1]] |>
    as_tibble()

  if(nrow(d) %% 2 == 0){
    # Even number of rows
    wch_less_than_median <- which(gb$x < quants[2])
    med_ind <- tail(wch_less_than_median, 1)
    med_dens <- (gb$density[med_ind] + gb$density[med_ind + 1]) / 2
  }else{
    med_ind <- which(gb$x == quants[2])
    med_dens <- gb$density[med_ind]
  }

  med <- tibble(x = quants[2],
                y = med_dens)

  g <- g +
    geom_area(data = gb |>
                filter(x >= quants[1] & x <= quants[3]),
              aes(x = x,
                  y = y),
              fill = fill_color,
              alpha = fill_alpha_main) +
    geom_segment(data = med,
                 aes(x = x,
                     xend = x,
                     y = 0,
                     yend = y),
                 linewidth = 1) +
    coord_cartesian(xlim = xlim) +
    xlab(paste0("Projected ", yr, " catch (Mt) based on the default ",
                "harvest policy")) +
    ylab("Density")

  g
}
