#' Make a bubble plot from the given data
#'
#' @param model A model, created by [create_rds_file()]
#' @param type 1 = Fishery, any other value = Survey
#' @param clines An optional vector of years to draw cohort lines through
#' @param tick_prop A value that the length of the major tick marks are
#' multiplied by. This proportion must be set by trial and error. Make sure
#' to change `vjust_x_labels` so the labels are not overlapping the lines or
#' are too far away from the lines
#' @param vjust_x_labels Adjustment to move the x-axis tick labels and label
#' up or down. Negative numbers move down
#' @param remove_yr_labels A vector of years to remove the ,labels for in
#' case they are overlapping
#' @param x_labs_mod How many years between year labels on the x-axis
#' @param leg_pos See the `leg_pos` parameter of
#' [ggplot2::theme()]
#' @param alpha See [ggplot2::geom_point()]
#' @param xlim Limits for the x-axis
#' @param ... Additional parameters passed to [ggplot2::geom_point()],
#' [ggplot2::geom_segment()] and [ggplot2::theme()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_pearson_bubbles <- function(model,
                                 type = c("fishery", "survey"),
                                 clines = age_bubble_cohorts,
                                 tick_prop = 1,
                                 vjust_x_labels = -1.5,
                                 remove_yr_labels = NULL,
                                 x_labs_mod = 5,
                                 leg_pos = "none",
                                 alpha = 0.3,
                                 xlim = c(1975, year(Sys.Date())),
                                 point_alpha = main_alpha,
                                 point_fill = main_fill,
                                 point_color = "black",
                                 diag_line_color = age_diag_line_color,
                                 diag_line_width = age_diag_line_width,
                                 diag_line_type = age_diag_line_type,
                                 ...){

  type <- match.arg(type)

  if(type == "fishery"){
    d <- model$extra_mcmc$residuals_fishery
  }else{
    d <- model$extra_mcmc$residuals_survey
  }

  x_breaks <- xlim[1]:xlim[2]
  x_labels <- x_breaks
  if(is.null(x_labs_mod)){
    x_labels[!x_labels %in% unique(d$Year)] <- ""
  }else{
    x_labels[!x_labels %in% seq(xlim[1], xlim[2], x_labs_mod)] <- ""
  }
  if(!is.null(remove_yr_labels)){
    x_labels[x_labels %in% remove_yr_labels] <- ""
  }

  y_breaks <- min(d$age):max(d$age)
  y_lim <- c(min(d$age), max(d$age))

  g <- ggplot(d, aes(x =yr,
                     y = age,
                     size = abs(pearson_med),
                     fill = factor(sign(as.numeric(pearson_med))))) +
    geom_point(pch = 21,
               alpha = point_alpha,
               color = point_color,
               ...) +
    scale_x_continuous(breaks = x_breaks,
                       labels = x_labels) +
    scale_y_continuous(breaks = y_breaks,
                       labels = y_breaks) +
    coord_cartesian(ylim = y_lim,
                    clip = "off") +
    scale_fill_manual(values = c("white",
                                 point_fill),
                      guide = "none") +
    scale_size_continuous(breaks = c(1, 1, 2, 2, 3, 3),
                          labels = c(-8, -4, -0.1, 0.1, 4, 8),
                          range = c(0.1, 8))  +
    theme(axis.text.x = element_text(vjust = vjust_x_labels),
          axis.title.x = element_text(vjust = vjust_x_labels)) +
    ylab("Age") +
    xlab("Year")

  # Add a major tick mark every `x_labs_mod` years
  g <- g |>
    add_major_ticks(x_breaks = x_breaks,
                    modulo = x_labs_mod,
                    prop = tick_prop,
                    ...)

  if(!is.null(clines)){
    age_range <- range(as.numeric(as.character(d$age)))
    ages <- seq(min(age_range), max(age_range))
    cohort_lines_df <- clines |>
      map_df(\(cohort_yr){
        map_df(ages, \(age){
          d |>
            dplyr::filter(yr == cohort_yr + age,
                   age == age)
        }) |>
          mutate(cohort = cohort_yr)
      })

    g <- g +
      geom_path(data = cohort_lines_df,
                aes(x = yr,
                    y = age,
                    group = factor(cohort)),
                linewidth = diag_line_width,
                color = diag_line_color,
                linetype = diag_line_type,
                show.legend = FALSE)
  }

  g <- g +
    theme(legend.position = leg_pos) +
    guides(size =
             guide_legend(title = "Residuals",
                          nrow = ifelse(leg_pos == "right" |
                                          leg_pos == "left", 10, 1),
                          override.aes =
                            list(fill = c("white", "white", "white",
                                          point_fill, point_fill, point_fill),
                                color = c(point_color,
                                          point_color,
                                          point_color),
                                 alpha = c(point_alpha,
                                           point_alpha,
                                           point_alpha),
                                 size = c(8, 4, 0.1,
                                          0.1, 4, 8))))

  g
}
