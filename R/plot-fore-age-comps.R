#' Create a two-panel plot showing the forecasted age compositions by
#' numbers and weight for the first forecast year (`endyr` + 1)
#'
#' @param model  A model object, created by [create_rds_file()]
#' @param probs A vector of three quantiles for the lower CI,
#' median, and upper CI. The second value must be 0.5
#' @param x_lim A vector of 2 representing the minimum and maximum x-axis
#' values to plot
#' @param x_breaks A vector of values to show on the x-axis
#' @param y_lim  A vector of 2 representing the minimum and maximum y-axis
#' values to plot
#' @param whisker_width The width of the top and bottom crossbars
#' (whiskers) on the error bars
#' @param point_size The size of the median points
#' @param glow_size The size of the white glow behind the points (must be
#' larger than `point_size` to be seen)
#' @param by_number_only If `TRUE`, return only the by number plot (single plot)
#' instead of the grid of two plots which includes the by-weight plot
#' @param is_catch_proj If `TRUE`, increment the year by one. Used only for
#' catch projection models
#'
#' @return A [cowplot::plot_grid()] object
#' @export
plot_fore_age_comps <- function(model,
                                x_lim = c(1, 15),
                                x_breaks = seq(x_lim[1], x_lim[2], 2),
                                y_lim = c(0, 0.55),
                                whisker_width = 0.5,
                                point_size = 2,
                                glow_size = 2.5,
                                by_number_only = FALSE,
                                is_catch_proj = FALSE,
                                show_x_title = TRUE){

  natsel_prop <- model$extra_mcmc$natsel_prop
  natselwt_prop <- model$extra_mcmc$natselwt_prop

  perc <- paste0(forecast_probs * 100, "%")

  reformat <- function(df){
    df |>
      apply(2, quantile, probs = forecast_probs) |>
      as_tibble(rownames = "quant") |>
      mutate(quant = ifelse(quant == perc[1],
                            "lowest",
                            ifelse(quant == perc[2],
                                   "lower",
                                   ifelse(quant == perc[3],
                                          "median",
                                          ifelse(quant == perc[4],
                                                 "higher",
                                                 "highest"))))) |>
      pivot_longer(-quant, names_to = "age", values_to = "prop") |>
      pivot_wider(names_from = "quant", values_from = "prop") |>
      mutate(age = as.numeric(age))
  }

  by_nums <- reformat(natsel_prop)
  by_weight <- reformat(natselwt_prop)

  plist <- NULL
  plist[[1]] <- ggplot(by_nums,
                       aes(x = age,
                           y = median)) +
    geom_bar(stat = "identity",
             fill = main_fill,
             alpha = main_alpha) +
    geom_errorbar(aes(ymin = lowest,
                      ymax = highest),
                  linewidth = 0.5,
                  width = whisker_width) +
    geom_errorbar(aes(ymin = lower,
                      ymax = higher),
                  linewidth = 2,
                  width = 0) +
    geom_point(color = "white",
               size = glow_size) +
    geom_point(size = point_size) +
    scale_x_continuous(breaks = x_breaks,
                       labels = x_breaks) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(xlim = x_lim,
                    ylim = y_lim,
                    expand = TRUE)
  if(show_x_title){
    plist[[1]] <- plist[[1]] +
      xlab("Age")
  }else{
    plist[[1]] <- plist[[1]] +
      xlab("")
  }
  plist[[1]] <- plist[[1]] +
    ylab(paste0("Exp. prop ",
                model$endyr + ifelse(is_catch_proj, 2, 1),
                " catch"))
  if(!by_number_only){
    plist[[1]] <- plist[[1]] +
      ggtitle("By number")
  }
  plist[[1]] <- plist[[1]] +
    theme(plot.title = element_text(hjust = 0.5))

  if(by_number_only){
    return(plist[[1]])
  }

  plist[[2]] <- ggplot(by_weight,
                       aes(x = age,
                           y = median)) +
    geom_bar(stat = "identity",
             fill = main_fill,
             alpha = main_alpha) +
    geom_errorbar(aes(ymin = lowest,
                      ymax = highest),
                  linewidth = 0.5,
                  width = whisker_width) +
    geom_errorbar(aes(ymin = lower,
                      ymax = higher),
                  linewidth = 2,
                  width = 0) +
    geom_point(color = "white",
               size = glow_size) +
    geom_point(size = point_size) +
    scale_x_continuous(breaks = x_breaks,
                       labels = x_breaks) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(xlim = x_lim,
                    ylim = y_lim,
                    expand = TRUE)
    if(show_x_title){
      plist[[2]] <- plist[[2]] +
        xlab("Age")
    }else{
      plist[[2]] <- plist[[2]] +
        xlab("")
    }
  plist[[2]] <- plist[[2]] +
    ylab("") +
    ggtitle("By weight") +
    theme(plot.title = element_text(hjust = 0.5))

  plot_grid(plotlist = plist, nrow = 1, align = "h")
}
