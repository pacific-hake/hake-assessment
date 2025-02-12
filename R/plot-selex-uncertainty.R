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
#' @param point_size The point size
#' @param point_fatten The fatness of the point. Needed when using
#' [ggplot2::geom_errorbar()]
#' @param border_width Thickness of the border line. If `NULL` no border will
#' be shown
#' @param border_color Color of the column borders
#' @param border_linetype Line type of the column borders
#' @param show_panel_borders Logical. If `TRUE`, show borders around the panels
#' @param label_loc A vector of two (x, y) describing the label location inside
#' the panels
#' @param label_font_size The labels font size
#' @param pad_top Add blank cells across the top as "padding"
#' @param pad_bottom Add blank cells across the bottom as "padding"
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_selex_uncertainty <- function(model,
                                   yr_lim = c(1990, last_data_yr),
                                   ages = 1:8,
                                   n_col = 1,
                                   rev = FALSE,
                                   point_size = 1,
                                   point_fatten = 2,
                                   border_width = 2,
                                   border_color = "black",
                                   border_linetype = "solid",
                                   show_panel_borders = FALSE,
                                   label_loc = c(1, 0.75),
                                   label_font_size = 4,
                                   pad_top = TRUE,
                                   pad_bottom = TRUE){

  yr_vec <- yr_lim[1]:yr_lim[2]

  lower <- model$extra_mcmc$sel_fishery_lo
  med <- model$extra_mcmc$sel_fishery_med
  upper <- model$extra_mcmc$sel_fishery_hi

  if(!is.null(yr_lim[1])){
    lower <- lower |>
      dplyr::filter(yr %in% yr_vec)
    med <- med |>
      dplyr::filter(yr %in% yr_vec)
    upper <- upper |>
      dplyr::filter(yr %in% yr_vec)
  }
  if(!is.null(ages)){
    lower <- lower |>
      select(yr, all_of(as.character(ages)))
    med <- med |>
      select(yr, all_of(as.character(ages)))
    upper <- upper |>
      select(yr, all_of(as.character(ages)))
  }

  if(rev){
    yr_vec <- rev(yr_vec)
  }
  lower <- lower |>
    mutate(yr = factor(yr, levels = yr_vec)) |>
    mutate(quant = "lower")
  med <- med |>
    mutate(yr = factor(yr, levels = yr_vec)) |>
    mutate(quant = "med")
  upper <- upper |>
    mutate(yr = factor(yr, levels = yr_vec)) |>
    mutate(quant = "upper")

  d <- lower |>
    bind_rows(med) |>
    bind_rows(upper) |>
    select(quant, yr, everything()) |>
    pivot_longer(-c(quant, yr), names_to = "age", values_to = "prop") |>
    pivot_wider(names_from = "quant", values_from = "prop")

  # For Annotating each panel with the year
  labs <- tibble(yr = unique(as.character(d$yr)),
                 age = NA_character_,
                 med = NA_real_,
                 lower = NA_real_,
                 upper = NA_real_)

  # `extras` is the number of extra cells to add to `yr_vec` to make the total
  # number of years divisible by the number of columns (`n_col`)
  leftover <- length(yr_vec) %% n_col
  extras <- ifelse(leftover, n_col - leftover, 0)
  # Add elements with different numbers of spaces to `yr_vec` to make it
  # divisible. There has to be a different number of spaces because later
  # these become levels when year is a factor for the plot
  post_empty_cells <- seq_len(extras) |>
    map_chr(~{
      paste(rep(" ", .x), collapse = "")
    })
  yr_vec <- c(yr_vec, post_empty_cells)
  col_lengths <- rep(length(yr_vec) / n_col, n_col)

  start_of_col <- 1
  yr_lst <- map(col_lengths, ~{
    ret <- yr_vec[start_of_col:(start_of_col + .x - 1)]
    start_of_col <<- start_of_col + .x
    ret
  })

  if(pad_top){
    # pad the top with empty cells
    yr_lst <- yr_lst |>
      imap(~{
        head_cell_val <- paste(rep(" ", .y + n_col + 1), collapse = "")
        c(head_cell_val, .x)
      })
  }
  if(pad_bottom){
    # pad the top with empty cells
    yr_lst <- yr_lst |>
      imap(~{
        head_cell_val <- paste(rep(" ", .y + n_col * 2 + 1), collapse = "")
        c(.x, head_cell_val)
      })
  }

  yr_vec <- map(seq_along(yr_lst[[1]]), function(yr_ind){
      map_chr(yr_lst, function(lst_elem){
        lst_elem[yr_ind]
      })
  }) |>
    unlist()

  # Add space-based elements to the data frame
  spc <- grep("\\s+", yr_vec, v=T)
  ages <- d$age |> unique()
  d_spc <- expand.grid(spc, ages) |>
    as_tibble() |>
    setNames(c("yr", "age"))
  d_spc_therest <- tibble(lower = NA_real_,
                          med = NA_real_,
                          upper = NA_real_)
  row <- d_spc_therest
  for(i in seq_len(nrow(d_spc) - 1)){
    d_spc_therest <- d_spc_therest |>
      bind_rows(row)
  }
  d_spc <- d_spc |>
    bind_cols(d_spc_therest)

  d <- d |>
    bind_rows(d_spc)
  d[["age"]] <- factor(d[["age"]], labels = ages, levels = as.character(ages))
  g <- ggplot(d,
              aes(x = age,
                  y = med,
                  ymin = lower,
                  ymax = upper,
                  group = yr)) +
    geom_line() +
    geom_pointrange(size = point_size,
                    fatten = point_fatten) +
    geom_ribbon(alpha = 0.2,
                fill = "blue",
                color = "black",
                linetype = "dotted") +
    geom_text(data = labs,
              aes(label = yr),
              x = label_loc[1],
              y = label_loc[2],
              hjust = 0.3,
              size = label_font_size) +
    scale_x_discrete(expand = c(0, 0.5)) +
    facet_wrap(~factor(yr,  levels = yr_vec),
               ncol = n_col) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(0, "cm"),
          strip.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
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

    yr_col_lengths <- yr_lst |> lengths()
    t_extent <- 7
    l_extent <- map_dbl(seq_len(n_col), ~{
      5 + (.x - 1) * 4
    })

    bot <- gr$layout |>
      dplyr::filter(name == "ylab-l") |>
      pull(b)
    b_extent <- bot
    if(n_col > 1){
      tmp <- map_dbl(seq_len(n_col - 1), ~{
        bot - ifelse(yr_col_lengths[.x + 1] < yr_col_lengths[1], 1, 0)
      })
      b_extent <- c(b_extent, tmp)
    }
    gt <- gtable_add_grob(gr,
                          grobs = g_lst,
                          t = 7,
                          b = b_extent,
                          l = l_extent)
    grid.newpage()
    return(grid.draw(gt))
  }

  g
}

