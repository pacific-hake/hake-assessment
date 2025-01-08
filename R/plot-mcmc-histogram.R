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
#' @param ro_arrow_length The length in count units (y-axis units) of the
#' arrow pointing to the bin that the R0 parameter is in. Only used if
#' `show_ro` is `TRUE`
#' @param ro_text_nudge Amount to nudge to the left(negative) or right
#' (positive) the R0 location label
#' @param ro_text_size The font size for the R0 labels
#' @param bar_label_limit The number in a bin (count - y-axis), above which
#' the label of the count for the bar is in white in the middle of the bar
#' vertically. Below this value, the bar value will be above the bar. To
#' have all value labels above the bars, set this to a value larger than the
#' highest count bin
#' @param bar_text_color The color of the count labels for each bar. Only
#' used if `show_bar_values` is `TRUE`
#' @param bar_text_size The font size of the count labels for each bar. Only
#' used if `show_bar_values` is `TRUE`
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
#' @param scale_effn
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_mcmc_histogram <- function(d,
                                col_nm,
                                x_lab = col_nm,
                                y_lab = "Count",
                                barplot = FALSE,
                                lvls = NULL,
                                scale_effn = 1,
                                show_ro = FALSE,
                                show_bar_values = TRUE,
                                ro_arrow_length = 20,
                                ro_text_nudge = 0,
                                ro_text_size = 5,
                                bar_label_limit = 10,
                                bar_text_color = "navyblue",
                                bar_text_size = 2,
                                x_lim = NULL,
                                y_lim = NULL,
                                x_breaks = NULL,
                                y_breaks = NULL,
                                x_brk = 0.1,
                                y_brk = 20,
                                ...){

  if(!col_nm %in% names(d)){
    stop("Column `", col_nm, "` is not present in the data frame provided")
  }

  col_sym <- sym(col_nm)

  ro_ind <- grep("R0", d$label)

  d <- d |>
    select(!!col_sym) |>
    rename(x = !!col_sym)

  ro_val <- d |>
    slice(ro_ind) |> pull()

  if(barplot){
    # This is the count data frame, which we use directly to plot instead of
    # the raw column
    d <- d |>
      count(x) |>
      rename(count = n)
    if(!is.null(lvls[1])){
      # One or more levels might be missing from the data frame. If so,
      # we still want to see it in the plot so have to figure out which are
      # missing and add zeroes for them to the count data frame
      vals <- unique(d$x)
      wch <- which(!lvls %in% vals)
      if(length(wch)){
        # Need to add one or more values to the table
        for(i in seq_along(wch)){
          row <- vec2df(c(lvls[i], 0), nms = names(d)) |>
            mutate(count = as.integer(count))
          d <- d |>
            bind_rows(row)
        }
      }
      d <- d |>
        mutate(x = factor(x, levels = lvls))
    }

    g <- ggplot(d) +
      geom_bar(data = d,
               aes(x = x,
                   y = count),
               stat = "identity",
               ...)

    if(show_bar_values){
      g <- g +
        geom_text(data = d |>
                    dplyr::filter(count > bar_label_limit),
                  aes(x = x,
                      y = count,
                      label = count),
                  color = bar_text_color,
                  size = bar_text_size,
                  position = position_stack(vjust = 0.5)) +
        geom_text(data = d |>
                    dplyr::filter(count <= bar_label_limit),
                  aes(x = x,
                      y = count,
                      label = count),
                  color = bar_text_color,
                  size = bar_text_size,
                  vjust = -0.5)
    }

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

    if(show_ro){
      # The levels of the x-axis variable
      grps_data <- levels(g$data$x)
      # The group number for the group containing the R0 value
      ro_grp <- which(grps_data == ro_val)
      # `gr` is the table with the plotting data details in it, linking group
      # number to plotting coordinates
      gr <- ggplot_build(g)$data[[1]]

      y_scale <- 1 / (y_lim[2] - y_lim[1]) * 100
      extra_text_space <- 4 / y_scale

      ro_grp_row <- gr[gr$group == ro_grp, ]
      ro_x <- ro_grp_row |>
        pull(x)
      ro_y0 <-  ro_grp_row |>
        pull(y)
      ro_y1 <- ro_y0 + ro_arrow_length
      # Arrow data frame
      lbl <- tibble(x = ro_x,
                    xend = ro_x,
                    y = ro_y0,
                    yend = ro_y1,
                    label = paste0("R[0] (", ro_val, ")"))

      g <- g +
        geom_segment(data = lbl,
                     aes(x = x,
                         xend = xend,
                         y = y,
                         yend = yend),
                     arrow = arrow(type = "closed",
                                   ends = "first",
                                   length = unit(2.5, 'mm'))) +
        geom_text(data = lbl,
                  aes(x = x + ro_text_nudge,
                      y = yend + extra_text_space,
                      label = label),
                  size = ro_text_size,
                  parse = TRUE)
    }
    return(g)
  }

  ro_val <- ro_val * scale_effn

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
    ylab(y_lab)

  if(show_bar_values){
    g <- g +
      stat_bin(aes(x = x,
                   label = after_stat(ifelse(count > bar_label_limit,
                                             count,
                                             NA))),
               color = bar_text_color,
               binwidth = x_brk,
               geom = "text",
               size = bar_text_size,
               position = position_stack(vjust = 0.5)) +
      stat_bin(aes(x = x,
                   label = after_stat(ifelse(count <= bar_label_limit,
                                             count,
                                             NA))),
               color = bar_text_color,
               binwidth = x_brk,
               geom = "text",
               size = bar_text_size,
               vjust = -0.5)
  }

  if(show_ro){
    y_scale <- 1 / (y_lim[2] - y_lim[1]) * 100
    extra_text_space <- 4 / y_scale

    gr <- ggplot_build(g)$data[[1]]
    wch_less <- which(gr$xmin < ro_val / scale_effn)
    wch_more <- which(gr$xmin >= ro_val / scale_effn)
    if(tail(wch_less, 1) + 1 != head(wch_more, 1)){
      stop("The R0 value does not seem to fit into a bin on the histogram")
    }
    ro_xmin <- gr |>
      slice(tail(wch_less, 1)) |>
      pull(xmin)
    ro_xmax <- gr |>
      slice(head(wch_more, 1)) |>
      pull(xmin)
    ro_x <- (ro_xmin + ro_xmax) / 2
    ro_y0 <- (gr |>
                slice(tail(wch_less, 1)) |>
                pull(y))
    ro_y1 <- ro_y0 + ro_arrow_length
    # Arrow data frame
    lbl <- tibble(x = ro_x,
                  xend = ro_x,
                  y = ro_y0,
                  yend = ro_y1,
                  label = paste0("R[0] (", ro_val, ")"))

    g <- g +
      geom_segment(data = lbl,
                   aes(x = x,
                       xend = xend,
                       y = y,
                       yend = yend),
                   arrow = arrow(type = "closed",
                                 ends = "first",
                                 length = unit(2.5, 'mm'))) +
      geom_text(data = lbl,
                aes(x = x + ro_text_nudge,
                    y = yend + extra_text_space,
                    label = label),
                size = ro_text_size,
                parse = TRUE)
  }

  g
}