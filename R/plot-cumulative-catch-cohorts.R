#' Creates a plot showing the cumulative catches for certain cohorts
#' as they age
#'
#' @param model A model list, as created by [create_rds_file()]
#' @param cohorts A vector of years representing cohorts to how
#' @param leg_pos The position of the legend inside the plot. If `NULL`,
#' `NA`, or "none", the legend will not be shown
#' @param leg_ncol The number of columns to show in the legend
#' @param show_inset Logical. If `TRUE`, show the inset panel
#' @param from The limits of the inset on the main plot to extract.
#' See [ggmagnify::geom_magnify()]
#' @param to The limits on the main panel of the location to place the
#' inset. See [ggmagnify::geom_magnify()]
#'
#' @return a [ggplot2::ggplot()] object
#' @export
plot_cumulative_catch_cohorts <- function(model,
                                          cohorts = c(1999,
                                                      2010,
                                                      2014,
                                                      2016,
                                                      2017,
                                                      2020,
                                                      2021),
                                          leg_pos = c(0.7, 0.4),
                                          leg_ncol = 1,
                                          show_inset = FALSE,
                                          from = c(xmin = 0,
                                                   xmax = 4,
                                                   ymin = 0,
                                                   ymax = 0.35),
                                          to = c(xmin = 10,
                                                 xmax = 20,
                                                 ymin = 0,
                                                 ymax = 1.5)){

  d <- map_dfr(cohorts, \(cohort){
    cs <- cumsum(cohort_catch(model, cohort))
    as_tibble(cs, rownames = "age") |>
      mutate(Age = as.numeric(age) - cohort) |>
      mutate(cohort = cohort) |>
      mutate(Cohort = factor(cohort, levels = cohorts)) |>
      mutate(`Cumulative catch (Mt)` = value / 1e6) |>
      select(Cohort, Age, `Cumulative catch (Mt)`)
  })

  colors <- plot_color(length(cohorts))
  g <- ggplot(d,
              aes(x = Age,
                  y = `Cumulative catch (Mt)`,
                  group = Cohort,
                  color = Cohort)) +
    geom_path(linewidth = 1,
              arrow = arrow(length = unit(0.25, "cm"),
                            type = "closed")) +
    scale_y_continuous(breaks = seq(0, 3, 0.5)) +
    scale_color_manual(values = colors)


  # Add legend if requested
  if(is.null(leg_pos[1]) || is.na(leg_pos[1])){
    g <- g +
      theme(legend.position = "none")
  }else{
    g <- g +
      theme(legend.position = leg_pos) +
      guides(color = guide_legend(ncol = leg_ncol))
  }

  if(show_inset){
    g <- g +
      geom_magnify(from = from,
                   to = to,
                   shadow = TRUE,
                   colour = "black",
                   linewidth = 0.5,
                   proj.linetype = 3,
                   shadow.args = list(colour = "black",
                                      sigma = 10,
                                      x_offset = 2,
                                      y_offset = 5))
  }

  g
}
