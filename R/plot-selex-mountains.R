plot_selex_mountains <- function(model,
                                 yrs = NULL,
                                 ages = 1:8,
                                 scale = 1,
                                 fill_granularity = 0.001,
                                 fill_col1 = "white",
                                 fill_col2 = "royalblue"){

  color_func <- colorRampPalette(colors = c(fill_col1, fill_col2))
  seg_cols <- color_func(n = ceiling(1 / fill_granularity * (length(ages) - 1)))

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
                   size = 0) +
    scale_x_continuous(breaks = ages) +
    xlab("Age") +
    ylab("Selectivity by year") #+
    #gginnards::geom_debug()

  k <- ggplot(data.frame(x = 1, y = 1, xend = 2, yend = 2),
              aes(x = x, y = y, xend = xend, yend = yend)) + geom_segment()
  kr_data <- ggplot_build(k)$data[[1]] |>
    as_tibble()

  gr_data <- ggplot_build(g)$data[[1]] |>
    as_tibble()
  ymax <- max(gr_data$height)

  browser()

  gr_maxage <- gr_data |>
    split(~y) |>
    map_df(~{
      j <- .x |>
        # Remove final ages as there will be no colored segments after them
        filter(x == max(ages))
    })

  gr_data <- gr_data |>
    # Rename these columns so the code that follows is not so confusing
    rename(age = x,
           yr = y)

  gr_maxage <- gr_data |>
    # Split up the data frame by year to get a list of data frames by maximum
    # age for each year.
    split(~yr) |>
    map_df(\(yr_df){
      j <- yr_df |>
        # Remove final ages as there will be no colored segments after them
        filter(age == max(ages))
    })

  gr <- gr_data |>
    # Split up the data frame by year to get a list of data frames by age for
    # each year. The data frames will have `length(ages)` rows, one row for
    # each age
    split(~yr) |>
    imap(\(yr_df, yr_ind){
      #browser()
      j <- yr_df |>
        # Remove final ages as there will be no colored segments after them
        filter(age != max(ages)) |>
        # Split up the data frame of age rows into individual 1-row
        # data frames, one for each age except the last
        split(~age) |>
        map(\(age_df){
          # Add a bunch of rows for segments, based on the slope of the line
          # between the current age and the next one
          #browser()
          height_at_age <- age_df$height + age_df$ymin
          height_at_next_age <- gr_data |>
            filter(yr == age_df$yr) |>
            filter(age == age_df$age + 1) |>
            pull(height)
          height_at_next_age <- height_at_next_age + age_df$ymin
          xvals <- seq(age_df$age,
                       #max(ages) - 0.001,
                       age_df$age + 1 - fill_granularity,
                       fill_granularity)
          #browser()
          yvals <- seq(height_at_age,
                       height_at_next_age,
                       length = length(xvals))
          for(i in seq_along(yvals)){
            # Add a new row for each interval
            row <- kr_data
            # Horizontal line from the new x value to ...
            row$x <- xvals[i]
            # .. the next age
            #row$xend <- age_df$age + 1
            row$xend <- max(ages)
            row$y <- yvals[i]
            row$yend <- yvals[i]
            row$group <- yr_ind
            if(i == 1){
              out <- row
            }else{
              out <- out |>
                bind_rows(row)
            }
          }
          out
        }) |> map_df(~{.x}) |>
        mutate(colour = seg_cols)
    }) |>
    map_df(~{.x})
browser()

  g <- g + geom_segment(data = gr,
               aes(x = x,
                   xend = xend,
                   y = y,
                   yend = yend,
                   colour = colour,
                   group = group),
               alpha = 0.3,
               inherit.aes = FALSE) +
    scale_color_identity() #+
    # geom_ridgeline(fill = "transparent",
    #                size = 1.5)

  g
}