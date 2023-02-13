#' Create a plot of time-varying selectivity panels
#'
#' @details
#' The panels show the medians and the credible interval as
#' calculated in [load_extra_mcmc()].
#'
#' @param model A model list, as created by [create_rds_file()]
#' @param yr_lim A vector of two values representing the minimum and
#' maximum years to plot panels for
#' @param ages A vector of ages to include
#' @param n_col The number of columns to hold panels
#' @param rev Logical. If `TRUE`, reverse the order of the years
#' @param axis_title_font_size Size of the font for the X and Y axis labels
#' @param axis_tick_font_size Size of the font for the X and Y axis tick labels
#' @param border_width Thickness of the border line. If `NULL` no border will
#' be shown
#' @param border_color Color of the column borders
#' @param border_linetype Line type of the column borders
#' @param label_loc A vector of two (x, y) describing the label location inside
#' the panels
#' @param label_font_size The labels font size
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_multiple_tv_selex_unc <- function(model,
                                       yr_lim = c(1990, last_data_yr),
                                       ages = 1:8,
                                       n_col = 1,
                                       rev = FALSE,
                                       point_size = 1,
                                       point_fatten = 2,
                                       axis_title_font_size = 14,
                                       axis_tick_font_size = 12,
                                       border_width = 2,
                                       border_color = "black",
                                       border_linetype = "solid",
                                       show_panel_borders = FALSE,
                                       label_loc = c(1, 0.75),
                                       label_font_size = 4){

  yr_vec <- yr_lim[1]:yr_lim[2]

  lower <- model$extra.mcmc$sel_fishery_lower
  med <- model$extra.mcmc$sel_fishery_median
  upper <- model$extra.mcmc$sel_fishery_upper

  if(!is.null(yr_lim[1])){
    lower <- lower |>
      filter(Yr %in% yr_vec)
    med <- med |>
      filter(Yr %in% yr_vec)
    upper <- upper |>
      filter(Yr %in% yr_vec)
  }
  if(!is.null(ages)){
    lower <- lower |>
      select(Yr, all_of(as.character(ages)))
    med <- med |>
      select(Yr, all_of(as.character(ages)))
    upper <- upper |>
      select(Yr, all_of(as.character(ages)))
  }

  if(rev){
    yr_vec <- rev(yr_vec)
  }
  lower <- lower |>
    mutate(Yr = factor(Yr, levels = yr_vec)) |>
    mutate(quant = "lower")
  med <- med |>
    mutate(Yr = factor(Yr, levels = yr_vec)) |>
    mutate(quant = "med")
  upper <- upper |>
    mutate(Yr = factor(Yr, levels = yr_vec)) |>
    mutate(quant = "upper")

  d <- lower |>
    bind_rows(med) |>
    bind_rows(upper) |>
    select(quant, Yr, everything()) |>
    pivot_longer(-c(quant, Yr), names_to = "age", values_to = "prop") |>
    pivot_wider(names_from = "quant", values_from = "prop")

  # For Annotating each panel with the year
  labs <- tibble(Yr = unique(as.character(d$Yr)),
                 age = NA_character_,
                 med = NA_real_,
                 lower = NA_real_,
                 upper = NA_real_)

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

  g <- ggplot(d, aes(x = age, y = med, ymin = lower, ymax = upper, group = Yr)) +
    geom_line() +
    geom_pointrange(size = point_size, fatten = point_fatten) +
    geom_ribbon(alpha = 0.2, fill = "blue", color = "black",
                linetype = "dotted") +
    geom_label(data = labs,
               aes(label = Yr),
               x = label_loc[1],
               y = label_loc[2],
               size = label_font_size) +
    scale_x_discrete(expand = c(0, 0.5)) +
    facet_wrap(~factor(Yr,  levels = yr_vec),
               ncol = n_col) +
    theme(strip.background = element_blank(),
          panel.spacing=unit(0, "cm"),
          strip.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = margin(12, 12, 0, 0),
          axis.text.x = element_text(color = "grey20",
                                     size = axis_tick_font_size),
          axis.title.x = element_text(color = "grey20",
                                      size = axis_title_font_size),
          axis.title.y = element_text(color = "grey20",
                                      size = axis_title_font_size),
          axis.ticks.length = unit(0.15, "cm")) +
    ylab("Selectivity by year") +
    xlab("Age")

  if(!show_panel_borders){
    g <- g +
      theme(panel.border = element_blank())
  }
  if(!is.null(border_width)){
    gr <- ggplotGrob(g)

    # Add basic black rectangle round the columns
    g_lst <- map(seq_len(n_col), ~{
      rectGrob(gp = gpar(col = border_color,
                         lty = border_linetype,
                         lwd = border_width,
                         fill = NA))
    })

    yr_col_lengths <- yr_lst |> map_dbl(~{length(!.x[!is.na(.x)])})
    t_extent <- 7
    l_extent <- map_dbl(seq_len(n_col), ~{
      5 + (.x - 1) * 4
    })

    bot <- gr$layout |> filter(name == "ylab-l") |> pull(b)
    b_extent <- bot
    if(n_col > 1){
      tmp <- map_dbl(seq_len(n_col - 1), ~{
        bot - ifelse(yr_col_lengths[.x + 1] < yr_col_lengths[1], 1, 0)
      })
      b_extent <- c(b_extent, tmp)
    }
    gt <- gtable::gtable_add_grob(gr,
                                  grobs = g_lst,
                                  t = 7,
                                  b = b_extent,
                                  l = l_extent)
    grid.newpage()
    return(grid.draw(gt))
  }

  g
}

