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
#' @param tick_prop A value that the length of the major tick marks are
#' multiplied by. This proportion must be set by trial and error. Make sure
#' to change `vjust_x_labels` so the labels are not overlapping the lines or
#' are too far away from the lines
#' @param vjust_x_labels Adjustment to move the x-axis tick labels and label
#' up or down. Negative numbers move down
#' @param remove_yr_labels A vector of years to remove the ,labels for in
#' case they are overlapping
#' @param x_lim The min and max limits as a vector for the x-axis
#' @param y_lim The min and max limits as a vector for the y-axis
#' @param y_lab_by The amount between each y-axis label
#' @param x_labs_mod How many years between year labels on the x-axis
#' @param age_label_font_size The font size for the labels pointing to the age
#' lines
#' @param age_label_side Where to place the age labels next to the lines.
#' One of "none", "both", "left", or "right"
#' @param x_labels_mod Show every nth year where n is this number
#' @param ... Arguments passed to [ggrepel::geom_label_repel()]
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
                               x_lim = c(start_yr_age_comps, last_data_yr),
                               y_lim = c(0, 2),
                               y_labs_by = 0.25,
                               x_labels_mod = 5,
                               x_labs_mod = 5,
                               tick_prop = 1,
                               vjust_x_labels = -1.5,
                               remove_yr_labels = NULL,
                               age_label_font_size = 4,
                               age_label_side = "right",
                               ...){

  age_inds <- grep("^\\d+$", names(wa))
  if(!length(age_inds)){
    stop("There were no age columns found in the data frame `wa`. Age ",
         "column names are numeric values of one or more digits only")
  }

  if(is.null(ages[1])){
    ages <- names(wa)[age_inds]
  }
  col_inds <- which(names(wa) %in% ages)

  wa <- wa |>
    select(year, col_inds)

  num_ages <- length(ages)
  min_yr <- min(wa$year)
  max_yr <- max(wa$year)
  col_func <- colorRampPalette(cols)
  colors <- col_func(num_ages)

  w <- wa |>
    pivot_longer(-year, names_to = "age") |>
    mutate(age = as.numeric(age)) |>
    mutate(age = factor(age, levels = sort(unique(age)))) |>
    mutate(isbold = FALSE)

  if(!is.null(bold_ages)){
    if(!all(bold_ages %in% ages)){
      stop("Not all of the selected ages to be bolded (`bol_ages`) are ",
           "in the ages to be plotted")
    }
    w <- w |>
      mutate(isbold = ifelse(age %in% bold_ages, TRUE, FALSE))
  }

  x_breaks <- x_lim[1]:x_lim[2]
  x_labels <- x_breaks
  if(is.null(x_labs_mod)){
    x_labels[!x_labels %in% unique(d$Year)] <- ""
  }else{
    x_labels[x_labels %% x_labs_mod != 0] <- ""
  }
  if(!is.null(remove_yr_labels)){
    x_labels[x_labels %in% remove_yr_labels] <- ""
  }

  y_breaks <- seq(y_lim[1], y_lim[2], y_labs_by)

  max_value <- max(w$value)

  g <- ggplot(w,
              aes(x = year,
                  y = value,
                  color = age,
                  linewidth = isbold,
                  fontface = ifelse(isbold, "bold", "plain"))) +
    geom_line() +
    geom_point(data = w |>
                 dplyr::filter(year %in% c(min_yr, max_yr))) +
    geom_point(data = w |>
                 dplyr::filter(year %in% x_breaks)) +
    geom_point(data = w |>
                 dplyr::filter(age %in% bold_ages),
               size = 3) +
    scale_color_manual(values = colors) +
    scale_linewidth_manual(values = c(0.5, 1.5)) +
    scale_x_continuous(breaks = x_breaks,
                       labels = x_labels,
                       expand = c(0.05, 0.05)) +
    scale_y_continuous(breaks = y_breaks,
                       labels = y_breaks,
                       expand = c(0, 0)) +
    coord_cartesian(ylim = y_lim,
                    clip = "off") +
    xlab("Year") +
    ylab("Mean weight-at-age (kg)") +
    theme(legend.position = "none")

  # Add a major tick mark every `x_labs_mod` years
  g <- g |>
    add_major_ticks(x_breaks = x_breaks,
                    modulo = x_labs_mod,
                    prop = tick_prop,
                    ...)

  if("left" %in% age_label_side || "both" %in% age_label_side){
    g <- g +
      geom_label_repel(data = w |>
                         dplyr::filter(year == min_yr),
                       aes(label = age),
                       nudge_x = -3.5,
                       size = age_label_font_size,
                       arrow = arrow(length = unit(3, "mm"),
                                     type = "closed"),
                       ...)
  }
  if("right" %in% age_label_side || "both" %in% age_label_side){
    g <- g +
      geom_label_repel(data = w |>
                         dplyr::filter(year == max_yr),
                       aes(label = age),
                       nudge_x = 3.5,
                       size = age_label_font_size,
                       arrow = arrow(length = unit(3, "mm"),
                                     type = "closed"),
                       ...)
  }

  g
}