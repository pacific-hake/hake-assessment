#' Plot a histogram for a given column in a data frame
#'
#' @param d A dta frame containing the column with name `col_nm` which
#' contains the values to use in the plot
#' @param col_nm The name of the column in `d`containing the
#' values to use in the plot
#' @param x_lab The label to show on the x-axis
#' @param y_lab The label to show on the y-axis
#' @param barplot Logical. If `TRUE`, return a [ggplot2::geom_bar()], which
#' is used for character columns (discrete data). If `FALSE`, return a
#' [ggplot2::geom_histogram()] which is for numerical data (continuous data)
#' @param lvls A vector of levels that should appear in the data if
#' `barplot` is `TRUE`. If `NULL`, the data will be plotted as-is
#' @param show_ro Logical. If `TRUE`, make a label with arrow showing which
#' bin the R0 parameter is in
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
#' `seq(xlim[1], xlim[2], by = x_brk)`. This is also used as the `binwidth`
#' in the [ggplot2::geom_histogram()] call
#' @param y_brk A value to use to create the `y_breaks` values if it is
#' `NULL`. This value is used as the `by` argument like this:
#' `seq(ylim[1], ylim[2], by = y_brk)`
#' @param ... Additional arguments to pass to the plotting functions
#' [ggplot2::geom_histogram()] and [ggplot2::geom_bar()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_mcmc_histogram <- function(d,
                                col_nm,
                                x_lab = col_nm,
                                y_lab = "Count",
                                barplot = FALSE,
                                lvls = NULL,
                                scale_effn = 1e3,
                                show_ro = FALSE,
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

  ro_ind <- grep("R0", d$label)

  d <- d |>
    select(!!col_sym) |>
    rename(x = !!col_sym)

  ro_val <- d |>
    slice(ro_ind) |> pull()
  ro_val <- ro_val * scale_effn

  if(barplot){
    # This is the count data frame, which we use directly to plot instead of
    # the raw column
    d <- d |>
      count(x) |>
      rename(count = n)
    if(!is.null(lvls[1])){
      # One or more levels might be missing from the data frame. If so,
      # we still want to see it in the plot so have to figure out which are
      # missing and add NAs for them to the count data frame
      vals <- unique(d$x)
      wch <- which(!lvls %in% vals)
      if(length(wch)){
        # Need to add one or more values to the table
        for(i in seq_along(wch)){
          row <- vec2df(c(lvls[i], NA), nms = names(d)) |>
            mutate(count = as.integer(count))
          d <- d |>
            bind_rows(row)
        }
      }
      d <- d |>
        mutate(x = factor(x, levels = lvls))
    }

    g <- ggplot(d) +
      geom_bar(aes(x = x,
                   y = count),
               stat = "identity",
               ...)
    # Set up limits and breaks
    gx <- layer_data(g)
    if(is.null(y_lim[1])){
      # Make the upper y limit evenly divisible by the `y_brk` sequence value
      y_lim <- c(0, max(gx$y) + (y_brk - max(gx$y) %% y_brk))
    }
    if(is.null(y_breaks[1])){
      y_breaks <- seq(y_lim[1], y_lim[2], by = y_brk)
    }
    g <- g +
      scale_y_continuous(breaks = y_breaks,
                         expand = c(0, 0),
                         labels = comma) +
      coord_cartesian(xlim = x_lim,
                      ylim = y_lim) +
      xlab(x_lab) +
      ylab(y_lab)

    return(g)
  }

  # Histograms only, barplots returned above
  d <- d |>
    mutate(x = as.numeric(x))

  g <- ggplot(d) +
    geom_histogram(aes(x = x),
                   binwidth = x_brk,
                   ...)

  # Set up limits and breaks
  gx <- layer_data(g)
  if(is.null(x_lim[1])){
    x_lim <- c(min(gx$x), max(gx$x))
  }
  if(is.null(x_breaks[1])){
    x_breaks <- seq(x_lim[1], x_lim[2], by = x_brk)
  }
  if(is.null(y_lim[1])){
    # Make the upper y limit evenly divisible by the `y_brk` sequence value
    y_lim <- c(0, max(gx$y) + (y_brk - max(gx$y) %% y_brk))
  }
  if(is.null(y_breaks[1])){
    y_breaks <- seq(y_lim[1], y_lim[2], by = y_brk)
  }

  g <- g +
    scale_x_continuous(breaks = x_breaks,
                       expand = c(0, x_brk / 1.5),
                       labels = comma) +
    scale_y_continuous(breaks = y_breaks,
                       expand = c(0, 0),
                       labels = comma) +
    coord_cartesian(xlim = x_lim,
                    ylim = y_lim) +
    xlab(x_lab) +
    ylab(y_lab) +
    stat_bin(aes(x = x,
                 label = after_stat(ifelse(count > 50, count, NA))),
             color = "white",
             binwidth = x_brk,
             geom = "text",
             position = position_stack(vjust = 0.5)) +
    stat_bin(aes(x = x,
                 label = after_stat(ifelse(count <= 50, count, NA))),
             binwidth = x_brk,
             geom = "text",
             vjust = -0.5) +
    annotate("text",
             x = ro_val / scale_effn,
             y = (y_lim[2] - y_lim[1]) / 2,
             label = paste0("R0 (", ro_val, ")"))

  g
}