#' Plot recruitment from MCMC output for one or more models
#'
#' @param model_lst A list of models, each created by [create_rds_file()]
#' @param model_names A vector of model names,the same length as `models_lst`
#' @param inc_means Logical. If `TRUE`, include means for each year and
#' model as an X
#' @param xlim The year limits to plot
#' @param x_breaks The year value tick marks to show for the x axis
#' @param x_expansion Amount of space to leave either side on the x-axis
#' @param ylim The depletion limits to plot
#' @param y_breaks The depletion value tick marks to show for the y axis
#' @param y_labels The depletion labels to show for the y axis tick marks
#' @param y_colors The color vector for each label for the y axis tick marks
#' @param alpha The transparency for all ribbons
#' @param leg_pos The position of the legend inside the plot. If `NULL`,
#' `NA`, or `none`, the legend will not be shown
#' @param leg_ncol The number of columns to show in the legend
#' @param leg_font_size The legend font size
#' @param point_size Size of all points shownin plot
#' @param line_width Width of all lines on the plot
#' @param single_point_color Point color for the case where there is only
#' one model to plot
#' @param single_line_color Line color for the case where there is only
#' one model to plot
#' @param crossbar_width The width of the end bars (top and bottom) of the errorbar
#' lines. Default of zero removes them
#' @param dodge_val The amount to offset the lines from each other in the
#' case of multiple models
#' @param d_obj If not `NULL` this is a list which has been
#' pre-processed to contain all models in a format that is ready to plot.
#' Essentially the first steps of this function have been replicated
#' outside the function (The code inside the `if(is.null(d_obj))`)
#' is done to stop the Rmd process from taking forever
#'
#' @return a [ggplot2::ggplot()] object
#' @export
plot_recruitment <- function(model_lst = NULL,
                             model_names,
                             inc_means = FALSE,
                             xlim = c(1966,
                                      year(Sys.time())),
                             x_breaks = c(1966,
                                          seq(
                                            round(1966 + 10, -1),
                                            # Current decade, i.e. 2020
                                            round(year(Sys.time()) - 10, -1),
                                            by = 10),
                                          year(Sys.time())),
                             x_expansion = 3,
                             ylim = c(0, 40),
                             y_breaks = seq(ylim[1], ylim[2], by = 10),
                             y_labels = y_breaks,
                             y_colors = rep("black", length(y_breaks)),
                             alpha = 0.2,
                             leg_pos = c(0.65, 0.83),
                             leg_ncol = 1,
                             leg_font_size = 12,
                             point_size = 1.5,
                             line_width = 0.5,
                             single_point_color = "black",
                             single_line_color = "black",
                             crossbar_width = 0,
                             dodge_val = 0.5,
                             rev_colors = FALSE,
                             d_obj = NULL){

  if(is.null(d_obj)){
    if(is.null(model_lst[1]) || is.null(model_names[1])){
      stop("Either `d_obj` or both `model_lst` and `model_names` ",
           "must be supplied. Both are `NULL`",
           call. = FALSE)
    }
    d_obj <- create_group_df_recr(model_lst, model_names)
  }

  d <- d_obj[[1]]
  colors <- plot_color(length(unique(d$model)))
  line_colors <- colors
  if(rev_colors){
    colors <- rev(colors)
    line_colors <- rev(line_colors)
  }
  is_single_model <- length(unique(d$model)) == 1
  if(is_single_model){
    colors <- single_point_color
    line_colors <- single_line_color
    ro_vec <- model_lst[[1]]$mcmccalcs$rinit
    yrs <- c(seq(min(d$year) - x_expansion,
                 min(d$year) - 1), d$year)
    ro <- tibble(model = model_names[[1]],
                 year = yrs,
                 rlower = ro_vec[1],
                 rmed = ro_vec[2],
                 rupper = ro_vec[3]) |>
      mutate(model = factor(model))
  }

  # Remove projection years
  d <- d |>
    filter(year <= xlim[2])

  g <- ggplot(d,
              aes(x = year,
                  y = rmed,
                  ymin = rlower,
                  ymax = rupper,
                  group = model,
                  color = model,
                  fill = model)) +
    scale_color_manual(values = line_colors) +
    coord_cartesian(xlim = xlim,
                    ylim = ylim)

  if(is_single_model){
    ro_val <- ro$rmed[1]
    below <- which(ro_val > y_breaks)
    above <- which(ro_val < y_breaks)
    tmp <- y_breaks
    y_breaks <- c(tmp[below], ro_val, tmp[above])
    y_labels <- c(tmp[below], expression(R[0]), tmp[above])

    # Expand right side of ro so that ribbon covers whole plot
    ro_row <- ro[nrow(ro),]
    ro_next_yr <- ro_row$year
    for(yr in ro_next_yr:(ro_next_yr + x_expansion)){
      ro_row$year <- yr
      ro <- bind_rows(ro, ro_row)
    }

    g <- g +
      geom_point(size = point_size,
                 color = colors) +
      geom_hline(data = ro,
                 aes(yintercept = rmed),
                 linetype = "dashed") +
      geom_ribbon(data = ro,
                  aes(ymin = rlower,
                      ymax = rupper),
                  alpha = alpha,
                  linetype = "dotted") +
      geom_errorbar(size = line_width,
                    position = position_dodge(dodge_val),
                    width = crossbar_width,
                    alpha = 0.5,
                    color = line_colors) +
      geom_point(size = point_size,
                 position = position_dodge(dodge_val),
                 color = colors)
  }else{
    g <- g +
      geom_errorbar(size = line_width,
                    position = position_dodge(dodge_val),
                    width = crossbar_width) +
      geom_point(size = point_size,
                 position = position_dodge(dodge_val))
  }

  g <- g +
    geom_hline(yintercept = 0,
               color = "black",
               linetype = "solid",
               size = 0.5) +
    scale_x_continuous(expand = c(0, x_expansion),
                       breaks = x_breaks,
                       labels = x_breaks) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       labels = y_labels) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = leg_font_size),
          axis.text.y = element_text(color = y_colors),
          # plot.margin: top, right,bottom, left
          # Needed to avoid tick labels cutting off
          plot.margin = margin(12, 12, 0, 0)) +
    xlab("Year") +
    ylab("Age-0 recruits (billions)")

  if(inc_means){
    if(is_single_model){
      g <- g +
        geom_point(aes(y = rmean),
                   shape = 4,
                   color = colors)
    }else{
      g <- g +
        geom_point(aes(y = rmean),
                   shape = 4)
    }

  }
  if(is.null(leg_pos[1]) || is.na(leg_pos[1])){
    g <- g +
      theme(legend.position = "none")
  }else{
    g <- g +
      theme(legend.position = leg_pos) +
      guides(color = guide_legend(ncol = leg_ncol))
  }

  g
}