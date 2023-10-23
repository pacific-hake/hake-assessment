#' Make a bubble plot from the given data
#'
#' @param d a [tibble::tibble()] of the data in long format with column
#' names `Year`, `Age`, and the column name given by `val_col_nm`
#' @param clines An optional vector of years to draw cohort lines through
#' @param val_col_nm The name of the column in `d` to use for the values
#' @param remove_yr_labels A vector of years to remove the ,labels for in
#' case they are overlapping
#' @param mean_age A two-column tibble with column names `Year` and `Age`
#' where each row contains a year and `Age` represents the mean age for
#' each year
#' @param mean_age_line_color The line color for the mean age line
#' @param mean_age_line_width The line width for the mean age line
#' @param mean_age_line_type The line type for the mean age line
#' @param diag_line_color The line color for the cohort diagonal lines
#' @param diag_line_width The line width for the cohort diagonal lines
#' @param diag_line_type The line type for the cohort diagonal lines
#' @param yrs A vector of 2, for the years to show on the plot
#' @param by How many years between year labels on the x-axis
#' @param leg_pos See the `legend.position` parameter in [ggplot2::theme()]
#' @param point_alpha Transparency of the bubble fill
#' @param point_fill Color of the bubble fill
#' @param point_color Color of the bubble outline
#' @param xlim Limits for the x-axis
#' @param show_x_axis_labels Logical. If `TRUE`, show the x-axis title and
#' tick labels
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_bubbles <- function(d,
                         clines = age_bubble_cohorts,
                         val_col_nm = "Proportion",
                         remove_yr_labels = NULL,
                         mean_age = NULL,
                         mean_age_line_color = "red",
                         mean_age_line_width = 1.5,
                         mean_age_line_type = "solid",
                         diag_line_color = age_diag_line_color,
                         diag_line_width = age_diag_line_width,
                         diag_line_type = age_diag_line_type,
                         yrs = NULL,
                         by = 5,
                         leg_pos = "none",
                         point_alpha = main_alpha,
                         point_fill = main_fill,
                         point_color = "black",
                         xlim = c(1966, year(Sys.Date())),
                         show_x_axis_labels = TRUE,
                         ...){

  if(!all(c("Year", "Age", val_col_nm) %in% names(d))){
    stop("One of the necessary columns is missing. This function requires ",
         "`Year` and `Age` columns and a column with the name assigned ",
         "to the argument `val_col_nm` (default name is `Proportion`)",
         call. = FALSE)
  }

  val_col_sym <- sym(val_col_nm)

  if(!is.null(xlim[1])){
    d <- d |>
      filter(Year %in% xlim[1]:xlim[2])
  }

  x_breaks <- xlim[1]:xlim[2]
  x_labels <- x_breaks
  x_labels[!x_labels %in% unique(d$Year)] <- ""
  if(!is.null(remove_yr_labels)){
    x_labels[x_labels %in% remove_yr_labels] <- ""
  }

  g <- ggplot(d,
              aes(x = Year,
                  y = Age,
                  size = !!val_col_sym)) +
    geom_point(shape = 21,
               alpha = point_alpha,
               fill = point_fill,
               color = point_color) +
    scale_x_continuous(breaks = x_breaks,
                       labels = x_labels,
                       expand = c(0.025, 0)) +
    coord_cartesian(xlim = xlim) +
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

  if(!show_x_axis_labels){
    g <- g +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank())
  }

  g <- g +
    theme(legend.position = leg_pos, ...)

  g
}
