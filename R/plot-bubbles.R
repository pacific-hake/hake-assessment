#' Make a bubble plot from the given data
#'
#' @param d a [tibble::tibble()] of the data in long format with column
#' names `Year`, `Age`, and `Proportion`
#' @param clines An optional vector of years to draw cohort lines through
#' @param mean_age A two-column tibble with column names `Year` and `Age` where
#' each row contains a year and `Age` represents the mean age for each year
#' @param mean_age_line_color The line color for the mean age line
#' @param mean_age_line_size The line thickness for the mean age line
#' @param mean_age_line_type The line type for the mean age line
#' @param yrs A vector of 2, for the years to show on the plot
#' @param by How many years between year labels on the x-axis
#' @param leg_pos See the `legend.position` parameter in
#' [ggplot2::theme()]
#' @param point_alpha Transparency of the bubble fill
#' @param point_fill Color of the bubble fill
#' @param point_color Color of the bubble outline
#' @param xlim Limits for the x-axis
#' @param leg_title The legend title text
#' @param ... Additional parameters passed to [ggplot2::geom_point()],
#' [ggplot2::geom_segment()] and [ggplot2::theme()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_bubbles <- function(d,
                         clines = c(1980, 1984, 1999, 2010, 2014, 2016, 2020),
                         mean_age = NULL,
                         mean_age_line_color = "red",
                         mean_age_line_width = 1.5,
                         mean_age_line_type = "solid",
                         diag_line_color = "darkgreen",
                         diag_line_width = 1,
                         diag_line_type = "solid",
                         yrs = NULL,
                         by = 5,
                         leg_pos = "none",
                         point_alpha = main_alpha,
                         point_fill = main_fill,
                         point_color = "black",
                         xlim = c(1966, year(Sys.Date())),
                         leg_title = "Proportion",
                         ...){

  if(!all(c("Year", "Age", "Proportion") %in% names(d))){
    stop("One of the necessary columns is missing. This function requires ",
         "`Year`, `Age`, and `Proportion` columns",
         call. = FALSE)
  }

  if(!is.null(xlim[1])){
    d <- d |>
      filter(Year %in% xlim[1]:xlim[2])
  }

  # Start breaks at the last divisible-by-5 year before the first year
  x_breaks <- seq(xlim[1] - (xlim[1] %% 5), xlim[2], 5)

  g <- ggplot(d,
              aes(x = Year,
                  y = Age,
                  size = Proportion)) +
    geom_point(shape = 21,
               alpha = point_alpha,
               fill = point_fill,
               color = point_color,
               ...) +
    scale_x_continuous(breaks = x_breaks,
                       expand = c(0.025, 0)) +
    coord_cartesian(xlim) +
    expand_limits(x = xlim[1]:xlim[2]) +
    scale_size_continuous(range = c(0.5, 10))

  if(!is.null(clines)){
    age_range <- range(as.numeric(as.character(d$Age)))
    ages <- seq(min(age_range), max(age_range))
    cohort_lines_df <- clines |>
      map_df(\(cohort_yr){
        map_df(ages, \(age){
          d |>
            filter(Year == cohort_yr + age,
                   Age == age)
        }) |>
          mutate(cohort = cohort_yr)
      })

    g <- g +
      geom_path(data = cohort_lines_df,
                aes(x = Year,
                    y = Age,
                    group = factor(cohort)),
                linewidth = diag_line_width,
                color = diag_line_color,
                linetype = diag_line_type)
  }

  if(!is.null(mean_age)){
    g <- g +
      geom_line(data = mean_age,
                aes(x = Year,
                    y = Age),
                inherit.aes = FALSE,
                color = mean_age_line_color,
                linewidth = mean_age_line_width,
                linetype = mean_age_line_type) +
      # Glow effect for points
      geom_point(data = mean_age,
                 aes(x = Year,
                     y = Age),
                 inherit.aes = FALSE,
                 color = "white",
                 alpha = 0.5,
                 size = mean_age_line_width + 1) +
      geom_point(data = mean_age,
                 aes(x = Year,
                     y = Age),
                 inherit.aes = FALSE,
                 color = mean_age_line_color,
                 size = mean_age_line_width)
  }

  g <- g +
    theme(legend.position = leg_pos, ...) +
    guides(size = guide_legend(title = leg_title))

  g
}
