plot_selex_mountains <- function(model,
                                 yrs = NULL,
                                 ages = 1:8,
                                 scale = 1,
                                 fill_num_colors = 10,
                                 fill_col1 = "white",
                                 fill_col2 = "royalblue"){

  color_func <- colorRampPalette(colors = c(fill_col1, fill_col2))
  gradient_cols <- color_func(n = fill_num_colors)
  fill_reduc_prop <- 1 - (1 / fill_num_colors)
  prop_seq <- seq(0, fill_reduc_prop, 1 / fill_num_colors)

  sel_med <- model$extra_mcmc$sel_fishery_med |>
    select(-iter)

  if(!is.null(yrs)){
    sel_med <- sel_med |>
      filter(yr %in% yrs)
  }

  if(!is.null(ages)){
    sel_med <- sel_med |>
      select(yr, all_of(as.character(ages)))
  }

  d <- sel_med |>
    pivot_longer(-yr, names_to = "age", values_to = "prop") |>
    mutate(prop = prop * scale) |>
    mutate(age = as.numeric(age)) |>
    mutate(yr = factor(yr, levels = rev(sort(unique(yr)))))

  # Needed for adding a second y-axis
  guide_axis_label_trans <- function(label_trans = identity, ...) {
    axis_guide <- guide_axis(...)
    axis_guide$label_trans <- rlang::as_function(label_trans)
    class(axis_guide) <- c("guide_axis_trans", class(axis_guide))
    axis_guide
  }

  # Needed for adding a second y-axis
  guide_train.guide_axis_trans <- function(x, ...) {
    trained <- NextMethod()
    trained$key$.label <- x$label_trans(trained$key$.label)
    trained
  }

  g <- ggplot(d,
              aes(x = age,
                  y = yr,
                  height = prop * scale,
                  group = yr,
                  fill = prop)) +
    #geom_ridgeline(fill = "#0072B250") +
    geom_ridgeline(fill = "transparent",
                   size = 0)

  gr_data <- ggplot_build(g)$data[[1]] |>
    as_tibble() |>
    transmute(age = x, yr = y, ymin, ymax) |>
    mutate(across(everything(), ~{as.numeric(.x)})) |>
    mutate(yr = d$yr) |>
    mutate(which_ribbon = 1)

  gr <- add_extra_ribbon_data(gr_data, prop_seq) |>
    mutate(ymax = ymin + ymax) |>
    mutate(yr = as.character(yr))

  ggplot(gr,
         aes(x = age,
             y = yr,
             group = yr,
             color = yr)) +
    geom_ribbon(aes(x = age,
                    ymax = ymax,
                    ymin = ymin,
                    group = which_ribbon,
                    fill = which_ribbon),
                alpha = 0.5,
                inherit.aes = FALSE) +
    facet_wrap(~ymin, ncol = 1) +
    theme(strip.background = element_blank(),
          panel.spacing = unit(-2, "cm"),
          strip.text.x = element_blank(),
          #axis.text.y = element_blank(),
          #axis.ticks.y = element_blank(),
          plot.margin = margin(12, 12, 0, 0),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent"),
          legend.position = "none") +
    scale_x_continuous(expand = c(0, 0),
                       breaks = ages) +
    xlab("Age") +
    ylab("Selectivity by year") #+
    #gginnards::geom_debug()

 }

add_extra_ribbon_data <- function(gr_data, prop_seq){

  prop_seq <- prop_seq[prop_seq != 0]
  prop_seq <- rev(sort(prop_seq))

  gr_data |>
    split(~yr) |>
    map(~{
      out <- .x
      for(i in seq_along(prop_seq)){
        tmp <- .x |>
          mutate(ymax = ymax * prop_seq[i]) |>
          mutate(which_ribbon = i + 1)
        out <- out |>
          bind_rows(tmp)
      }
      out
    }) |>
    map_df(~{.x})
}