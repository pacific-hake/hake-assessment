#' Create a set of barplots by year with the bars being observed
#' age comps and errorbars being the MCMC fits with 95% credible
#' interval
#'
#' @param model A model, created by [create_rds_file()]
#' @param type The type of fits/observations to plot. Must be either
#' `fishery` or `survey`
#' @param n_col The number of columns for the facets (years)
#' @param whisker_width The width (size) of the top and bottom bars on the
#' @param ages A vector of ages to show on the x-axis
#' @param x_breaks The x-axis breaks passed to [ggplot2::scale_x_discrete()]
#' @param axis_title_font_size The font size for the axis titles
#' @param axis_tick_font_size The font size for the axis tick labels
#' @param label_loc The x-y location of the labels in each panel (vector of 2)
#' @param label_font_size The label font size
#' @param point_size The point size. Passed to [ggplot2::geom_point()]ends of
#' the errorbar
#'
#' @export
plot_age_comp_fit <- function(model,
                              type = c("fishery", "survey"),
                              ages = `if`(type == "fishery", 1:15, 2:15),
                              n_col = 4,
                              x_breaks = seq(2, max(ages), by = 2),
                              axis_title_font_size = 14,
                              axis_tick_font_size = 12,
                              label_loc = c(ages[length(ages)] - 2, 0.45),
                              label_font_size = 4,
                              point_size = 1.5,
                              whisker_width = 0.15){

  type <- match.arg(type)

  if(type == "fishery"){
    d <- model$extra_mcmc$comp_fishery
  }else if(type == "survey"){
    d <- model$extra_mcmc$comp_survey
  }
  d <- d |>
    select(-c(Pearson_lower, Pearson_med, Pearson_upper)) |>
    mutate(Age = factor(Age))

  colors <- rev(plot_color(length(ages)))

  # `yr_vec` is used to set up downward column order for the facets instead
  # of the default left to right row order. This is done by putting the
  # years out of order and setting the facet year as a factor with levels
  # ordered this way. So `yr_vec` ends up being in an order where, when
  # ggplot draws the facets from left to right row by row, they appear
  # in a downward column order
  yr_vec <- sort(unique(d$Yr))

  if(length(yr_vec) %% n_col != 0){
    extras <- length(yr_vec) %% n_col
    full_cols_len <- length(yr_vec) - extras
    num_each_col <- full_cols_len / n_col
    col_lengths <- rep(num_each_col, n_col)
    for(i in 1:extras){
      col_lengths[i] <- col_lengths[i] + 1
    }
    if(sum(col_lengths) != length(yr_vec)){
      stop("Sanity check failed, column algorithm error",
           call. = FALSE)
    }
  }else{
    col_lengths <- rep(length(yr_vec) / n_col, n_col)
  }

  start_of_col <- 1
  yr_lst <- map(col_lengths, ~{
    ret <- yr_vec[start_of_col:(start_of_col + .x - 1)]
    start_of_col <<- start_of_col + .x
    ret
  })

  yr_vec <- map(seq_along(yr_lst[[1]]), function(yr_ind){
    map_dbl(yr_lst, function(lst_elem){
      lst_elem[yr_ind]
    })
  }) |>
    unlist()

  # `rot_vec` rotated the last element of a vector around to the beginning
  #  of the vector.
  # @param v A vector to be rotated
  # @param num_rot The number of rotations to do
  rot_vec <- function(v, num_rot = 1){
    for(i in seq_len(num_rot)){
      v <- c(v[length(v)], v[-length(v)])
    }
    v
  }

  # `cols` holds the colors of all the bars, 'rotated' so that
  # the same color is present for any given cohort between facets
  # (years). `yr_diffs` contains the number of years between
  # subsequent facets. For the survey, this can be 1, 2, or 3 years
  # and for the fishery it is always 1 but this allows for any
  # possibility
  x <- sort(unique(d$Yr))
  yr_diffs <- x[-1] - x[-length(x)]
  cols <- colors
  for(i in seq_along(yr_diffs)){
    colors <- rot_vec(colors, yr_diffs[i])
    cols <- c(cols, colors)
  }
  d <- d |>
    mutate(col = cols)

  g <- ggplot(d, aes(x = Age, y = Obs_med, group = Yr, fill = col)) +
    geom_bar(stat = "identity", width = 1, color = "black") +
    scale_fill_manual(values = colors) +
    geom_point(aes(x = Age, y = Exp_med),
               shape = 21,
               size = point_size,
               fill = "white",
               inherit.aes = FALSE) +
    geom_errorbar(aes(x = Age,
                      ymin = Exp_lower,
                      ymax = Exp_upper),
                  width = whisker_width,
                  inherit.aes = FALSE) +
    scale_x_discrete(breaks = x_breaks,
                     labels = x_breaks) +
    facet_wrap(~factor(Yr, levels = yr_vec),
               ncol = n_col) +
    geom_label(aes(label = Yr),
               x = label_loc[1],
               y = label_loc[2],
               size = label_font_size,
               fill = "transparent") +
    theme(strip.background = element_blank(),
          panel.spacing=unit(0, "cm"),
          strip.text.x = element_blank(),
          plot.margin = margin(12, 12, 0, 0),
          axis.text.x = element_text(color = "grey20",
                                     size = axis_tick_font_size),
          axis.title.x = element_text(color = "grey20",
                                      size = axis_title_font_size),
          axis.title.y = element_text(color = "grey20",
                                      size = axis_title_font_size),
          axis.ticks.length = unit(0.15, "cm"),
          legend.position = "none") +
    ylab("Proportion") +
    xlab("Age")

  g
}
