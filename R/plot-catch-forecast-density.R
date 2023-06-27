#' Title
#'
#' @param model The model output from Stock Synthesis as loaded by
#' [create_rds_file()].
#' @param yr Forecast year to plot. Must be a column in the MCMC output
#' @param xlim A vector of two values. The x-axis minimum and maximum values
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_catch_forecast_density <- function(model,
                                        yr,
                                        xlim = c(0, 4)){

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
                 fill = "royalblue",
                 alpha = 0.3)
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
              fill = hake::main_fill,
              alpha = hake::main_alpha) +
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
