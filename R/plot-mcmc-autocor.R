#' Plot the autocorrelation from a pre-calculated data frame
#'
#' @param d A dta frame containing the column with name `col_nm` which
#' contains the autocorrelation calculations as done in
#' [calc_mcmc_param_stats()]. These are calculated using [stats::acf()]
#' @param col_nm The name of the column in `d`containing the
#' autocorrelation values
#' @param x_lim A vector of two values. The min and max to show on the
#' x-axis. If `NULL`, values will be calculated from the input values
#' @param y_lim A vector of two values. The min and max to show on the
#' y-axis. If `NULL`, values will be calculated from the input values
#' @param x_breaks A vector of values to show on the x-axis. If `NULL`,
#' values will be calculated from the input values
#' @param y_breaks A vector of values to show on the y-axis. If `NULL`,
#' values will be calculated from the input values
#' @param x_brk A value to use to create the `x_breaks` values if it is
#' `NULL`. This value is used as the `by` argument like this:
#' `seq(xlim[1], xlim[2], by = x_brk)`
#' @param y_brk A value to use to create the `y_breaks` values if it is
#' `NULL`. This value is used as the `by` argument like this:
#' `seq(ylim[1], ylim[2], by = y_brk)`
#' @param ... Additional arguments to pass to the plotting function
#' [ggplot2::geom_histogram()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_mcmc_autocor <- function(d,
                              col_nm,
                              x_lim = NULL,
                              y_lim = NULL,
                              x_breaks = NULL,
                              y_breaks = NULL,
                              x_brk = 0.1,
                              y_brk = 20,
                              ...){

  if(!col_nm %in% names(d)){
    stop("Column `", col_nm, "` is not present in the data frame provided",
         call. = FALSE)
  }

  col_sym <- sym(col_nm)

  d <- d |>
    select(!!col_sym) |>
    rename(x = !!col_sym) |>
    mutate(x = as.numeric(x))

  g <- ggplot(d) +
    geom_histogram(aes(x = x),
                   binwidth = 0.1,
                   ...)

  # Set up limits and breaks
  gx <- layer_data(g)
  if(is.null(x_lim[1])){
    x_lim <- c(min(gx$x), max(gx$x))
  }
  if(is.null(x_breaks[1])){
    x_breaks <- seq(x_lim[1], x_lim[2], by = 0.1)
  }
  if(is.null(y_lim[1])){
    # Make the upper y limit evenly divisible by the `y_brk` sequence value
    y_lim <- c(0, max(gx$y) + (y_brk - max(gx$y) %% y_brk))
  }
  if(is.null(y_breaks[1])){
    y_breaks <- seq(y_lim[1], y_lim[2], by = y_brk)
  }

  g <- g +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks,
                       expand = c(0, 0),
                       labels = comma) +
    coord_cartesian(xlim = x_lim,
                    ylim = y_lim) +
    xlab("Autocorrelation, lag = 1") +
    ylab("Count")

  g
}