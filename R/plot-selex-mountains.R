plot_selex_mountains <- function(model,
                                 yrs = NULL,
                                 ages = 1:8,
                                 scale = 1){

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

  g <- ggplot(d, aes(x = age,
                y = yr,
                height = prop * scale,
                group = yr,
                fill = prop)) +
    geom_ridgeline(fill = "#0072B250") +
    scale_x_continuous(breaks = ages) +
    xlab("Age") +
    ylab("Selectivity by year") #+
    #guides(y.sec = guide_axis_label_trans())

  g
}