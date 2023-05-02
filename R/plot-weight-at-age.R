#' Create a plot of weight-at-age lines by age and year
#'
#' @param wa A data frame of weight-at-age with a `yr` column and
#' numerically-named columns for ages
#' @param ages A vector of ages to plot. If `NULL`, all ages in the `wa`
#' data frame will be plotted
#' @param bold_ages A vector of ages to make bold in the plot. The lines for
#' these ages will be thicker and the labels for them will be bold as well
#' @param cols A vector of colors to base the color ramp on. See
#' [ggplot2::scale_color_gradientn()]
#' @param axis_title_font_size Size of the font for the X and Y axis labels
#' @param axis_tick_font_size Size of the font for the X and Y axis tick labels
#' @param axis_text_color The color of the tick labels and axis labels
#' @param age_label_font_size The font size for the labels pointing to the age
#' lines
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_weight_at_age <- function(wa,
                               ages = NULL,
                               bold_ages = NULL,
                               cols = c("purple",
                                        "darkblue",
                                        "yellow",
                                        "darkgreen"),
                               axis_title_font_size = 14,
                               axis_tick_font_size = 11,
                               axis_text_color = "black",
                               age_label_font_size = 4){

  age_inds <- grep("^\\d+$", names(wa))
  if(!length(age_inds)){
    stop("There were no age columns found in the data frame `wa`. Age ",
         "column names are numeric values of one or more digits only",
         call. = FALSE)
  }

  if(is.null(ages[1])){
    ages <- names(wa)[age_inds]
  }
  col_inds <- which(names(wa) %in% ages)
  wa <- wa |>
    select(yr, col_inds)

  num_ages <- length(ages)
  min_yr <- min(wa$yr)
  max_yr <- max(wa$yr)
  col_func <- colorRampPalette(cols)
  colors <- col_func(num_ages)

  w <- wa |>
    pivot_longer(-yr, names_to = "age") |>
    mutate(age = as.numeric(age)) |>
    mutate(age = factor(age, levels = sort(unique(age)))) |>
    mutate(isbold = FALSE)

  if(!is.null(bold_ages)){
    if(!all(bold_ages %in% ages)){
      stop("Not all of the selected ages to be bolded (`bol_ages`) are ",
           "in the ages to be plotted",
           call. = FALSE)
    }
    w <- w |>
      mutate(isbold = ifelse(age %in% bold_ages, TRUE, FALSE))
  }

  # Set up the y-axis tick mark frequency (one for every year)
  y_breaks <- seq(0, max(w$value), by = 0.25)
  y_labels <- y_breaks
  x_breaks <- seq(min_yr, max_yr, by = 5)
  x_labels <- x_breaks

  max_value <- max(w$value)

  g <- ggplot(w,
              aes(x = yr,
                  y = value,
                  color = age,
                  linewidth = isbold,
                  fontface = ifelse(isbold, "bold", "plain"))) +
    geom_line() +
    geom_point(data = w |>
                 filter(yr %in% c(min_yr, max_yr))) +
    geom_point(data = w |>
                 filter(yr %in% x_breaks)) +
    geom_point(data = w |>
                 filter(age %in% bold_ages),
               size = 3) +
    scale_color_manual(values = colors) +
    scale_linewidth_manual(values = c(0.5, 1.5)) +
    scale_x_continuous(breaks = x_breaks,
                       labels = x_labels,
                       expand = c(0.05, 0.05)) +
    scale_y_continuous(breaks = y_breaks,
                       labels = y_labels,
                       expand = c(0, 0),
                       limits = c(0, max_value)) +
    geom_label_repel(data = w |> filter(yr == min_yr),
                     aes(label = age),
                     nudge_x = -3.5,
                     size = age_label_font_size) +
    geom_label_repel(data = w |> filter(yr == max_yr),
                     aes(label = age),
                     nudge_x = 3.5,
                     size = age_label_font_size) +
    xlab("Year") +
    ylab("Mean weight-at-age (kg)") +
    theme(axis.text.x = element_text(color = axis_text_color,
                                     size = axis_tick_font_size,
                                     angle = 0,
                                     hjust = 0.5,
                                     vjust = -0.25,
                                     face = "plain"),
          axis.text.y = element_text(color = axis_text_color,
                                     size = axis_tick_font_size,
                                     hjust = 1,
                                     vjust = 0.5,
                                     face = "plain"),
          axis.title.x = element_text(color = axis_text_color,
                                      size = axis_title_font_size,
                                      angle = 0,
                                      vjust = 0,
                                      face = "plain"),
          axis.title.y = element_text(color = axis_text_color,
                                      size = axis_title_font_size,
                                      angle = 90,
                                      face = "plain"),
          plot.margin = margin(12, 12, 7, 7),
          legend.position = "none")

  g
}