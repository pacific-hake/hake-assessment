#' Create a vector of labels to use when major and minor tick marks are
#' to be used in a plot, and the custom tick data frame to be used to draw
#' the lines outside the ggplot panel area using [ggplot2::geom_linerange()]
#'
#' @param breaks A vector of values which are where the minor ticks will
#' be placed
#' @param modulo The number to use as a modulus so that every n-th tick mark
#' is a major tick
#' @param lower_lim The lower limit of the plot, usually zero
#' @param mjr_tick_length The length of the major tick mark, in centimeters
#'
#' @return A list of two elements, the first is a vector of labels to use
#' for the major ticks only. The minor tick locations do not have labels.
#' The second list element is a data frame of two columns, the breaks (values)
#' where the major ticks will occur and the ending position of the lines
#' used to mark the major ticks
#' @export
create_labels_and_ticks <- function(breaks,
                                    modulo,
                                    lower_lim = 0,
                                    mjr_tick_length = major_tick_length){

  labels <- NULL
  for(i in breaks){
    if(i %% modulo == 0){
      labels <- c(labels, i)
    }else{
      labels <- c(labels, "")
    }
  }
  # X-axis tick mark lengths adjusted here
  breaks_nth <- breaks[breaks %% modulo == 0]
  top_y_pos <- lower_lim
  mjr_tick_length <-
    as.numeric(convertX(unit(mjr_tick_length, "cm"), "pt"))
  bot_pos <- -mjr_tick_length

  custom_ticks <- tibble(group = breaks_nth,
                         end = bot_pos)

  list(labels = labels,
       custom_ticks = custom_ticks)
}