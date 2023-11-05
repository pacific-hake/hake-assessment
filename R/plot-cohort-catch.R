#' Create a plot of cohorts by age as a set of lines
#'
#' @param model A model, created by [create_rds_file()]
#' @param cohorts A vector of cohorts to make lines for
#' @param ages A vector of ages to show
#' @param arrow_size size of the arrow heads, `npc` units. See [grid::unit()]
#' For description of `npc`
#' @param show_arrowheads Logical. If `TRUE` show arrowheads on the ends of
#' the lines
#'
#' @rdname plot_biomass
#' @export
plot_cohort_catch <- function(model,
                              cohorts = c(1999,
                                          2010,
                                          2014,
                                          2016,
                                          2020),
                              ages = 0:20,
                              y_breaks = seq(0, 1250, 250),
                              leg_pos = c(0.85, 0.5),
                              leg_ncol = 1,
                              leg_font_size = 12,
                              axis_title_font_size = 14,
                              axis_tick_font_size = 11,
                              line_width = 1,
                              show_arrowheads = TRUE,
                              arrow_size = 0.03){

  retro_yrs <- seq(1, length(cohorts))
  end_yrs <- c(model$endyr, model$endyr - retro_yrs)
  len_each <- length(ages)

  cohort_df <- map(cohorts, \(cohort){
    j <- cumsum(cohort_catch(model,
                             cohort,
                             ages = ages,
                             trim_end_year = end_yr))
    length(j) <- len_each
    names(j) <- ages
    j / 1e3
  }) |>
    setNames(retro_yrs) |>
    map_dfr(~{.x}) |>
    mutate(Cohort = cohorts) |>
    select(Cohort, everything()) |>
    pivot_longer(-Cohort, names_to = "Age", values_to = "Cumulative catch (kt)") |>
    mutate(Cohort = factor(Cohort, levels = cohorts),
           Age = as.numeric(Age))

  colors <- plot_color(length(cohorts))

  g <- ggplot(cohort_df,
              aes(x = Age,
                  y = `Cumulative catch (kt)`,
                  group = Cohort,
                  color = Cohort))
  if(show_arrowheads){
    g <- g +
      geom_line(linewidth = line_width,
                lineend = "round",
                linejoin = "round",
                arrow = arrow(length = unit(arrow_size,
                                            "npc"),
                              type = "closed"))
  }else{
    g <- g +
      geom_line(linewidth = line_width,
                lineend = "round",
                linejoin = "round")
  }

  g <- g +
    scale_color_manual(values = colors) +
    scale_y_continuous(breaks = y_breaks,
                       labels = scales::comma) +
    theme(legend.text = element_text(size = leg_font_size),
          # Needed to avoid tick labels cutting off
          plot.margin = margin(12, 12, 10, 0))

  g <- g +
    theme(axis.text.x = element_text(color = "grey20",
                                     size = axis_tick_font_size,
                                     angle = 0,
                                     hjust = 0.5,
                                     vjust = -3,
                                     face = "plain"),
          axis.text.y = element_text(color = "grey20",
                                     size = axis_tick_font_size,
                                     hjust = 1,
                                     vjust = 0.5,
                                     face = "plain"),
          axis.title.x = element_text(color = "grey20",
                                      size = axis_title_font_size,
                                      angle = 0,
                                      vjust = -2,
                                      face = "plain"),
          axis.title.y = element_text(color = "grey20",
                                      size = axis_title_font_size,
                                      angle = 90,
                                      face = "plain"),
          axis.ticks.length = unit(0.15, "cm"))

  if(is.null(leg_pos[1]) || is.na(leg_pos[1])){
    g <- g +
      theme(legend.position = "none")
  }else{
    g <- g +
      theme(legend.position = leg_pos) +
      guides(fill = guide_legend(ncol = leg_ncol),
             color = guide_legend(ncol = leg_ncol))
  }

  g
}
