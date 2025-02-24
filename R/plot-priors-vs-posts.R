#' Make a grid of posterior plots with priors and initial values
#'
#' @param model The model output from Stock Synthesis as loaded by
#'   [create_rds_file()].
#' @param x_range A string specifying the method to limit the range of each
#'   x axis in the panels. Typically, all geoms used in the figure dictate the
#'   range. But, with the use of [ggh4x::facetted_pos_scales()] we can specify
#'   which data set should specify the range limits. Setting this argument to
#'   `"posterior"`, instead of the default `"prior"`, will limit the ranges to
#'   only the realized values in the posterior. This can be helpful when the
#'   prior is quite vague and the posterior only covers a small range of the
#'   parameter space.
#' @param median_line_color The color to use for the vertical median lines
#' @param initial_value_color The color to use for the initial value points
#' @param prior_line_color The color to use for the density lines
#' representing the prior distributions
#' @param facet_title_font_size The font size to use for the panel titles (the
#' parameter names at the top of each plot)
#' @param ... Parameters to be passed to [ggplot2::facet_wrap()]. For example,
#'   `labeller = label_parsed_space`, which is available in this package, will
#'   remove the spaces from the strings and implement
#'   [ggplot2::label_parsed()]. And, `ncol` and `nrow` can be used to direct
#'   the output or you can let {ggplot2} figure it out.
#'
#' @return A {ggplot2} object.
#' @export
plot_priors_vs_posts <- function(model,
                                 x_range = c("posterior", "prior"),
                                 median_line_color = "darkred",
                                 initial_value_color = "green",
                                 prior_line_color = "black",
                                 facet_title_font_size = axis_title_font_size,
                                 ...){
  x_range <- match.arg(x_range)

  priors <- model$parameter_priors
  priors <- priors[names(priors) != "Fishery recruitment deviations"]
  priors <- priors[names(priors) != "Age 1 extra SD"]
  titles <- names(priors)

  posts <- model$parameter_posts
  names(posts) <- titles

  posts_long <- posts |>
    pivot_longer(everything(), names_to = "param") |>
    mutate(param = factor(param, levels = titles)) |>
    dplyr::filter(!is.na(value))

  priors_long <- map(priors, "prior_random") |>
    as_tibble() |>
    pivot_longer(everything(), names_to = "param") |>
    mutate(param = factor(param, levels = titles))

  priors_init <- map(priors, "initval") |>
    unlist() |>
    enframe(name = "param") |>
    mutate(param = factor(param, levels = titles))

  # `ggplot2::label_parsed()` does not work with spaces in the labels,
  # so we replace them with tildes. These are converted to spaces by
  # the labeller function `ggplot2::label_parsed()`
  titles <- gsub(" ", "~", titles)
  # This line is necessary to make `ggplot2::label_parsed()` work properly.
  # If you leave it out, all labels will be `NA`
  names(titles) <- levels(posts_long$param)
  names(titles) <- gsub(" +", "~", names(titles))
  posts_long <- posts_long |>
    mutate(param = as.character(param)) |>
    mutate(param = gsub(" +", "~", param)) |>
    mutate(param = factor(param, levels = titles))
  priors_long <- priors_long |>
    mutate(param = as.character(param)) |>
    mutate(param = gsub(" +", "~", param)) |>
    mutate(param = factor(param, levels = titles))
  priors_init <- priors_init |>
    mutate(param = as.character(param)) |>
    mutate(param = gsub(" +", "~", param)) |>
    mutate(param = factor(param, levels = titles))

  g <- ggplot() +
    geom_histogram(data = posts_long,
                   mapping = aes(value, after_stat(density)),
                   fill = main_fill,
                   alpha = main_alpha,
                   col = "black",
                   bins = 30) +
    geom_vline(data = group_by(posts_long, param) |>
                 summarize(median = median(value)),
               mapping = aes(xintercept = median),
               linetype = "solid",
               linewidth = 2,
               color = "white") +
    geom_vline(data = group_by(posts_long, param) |>
                 summarize(median = median(value)),
               mapping = aes(xintercept = median),
               linetype = "solid",
               linewidth = 1.5,
               color = median_line_color) +
    geom_density(data = priors_long,
                 mapping = aes(value, after_stat(density)),
                 col = prior_line_color,
                 linewidth = 1.2) +
    geom_point(data = priors_init,
               mapping = aes(x = value, y = 0),
               col = initial_value_color,
               pch = 17,
               size = 4) +
    # facet_wrap(~param,
    #            scales = "free") +
    # facet_wrap(~param,
    #            scales = "free",
    #            labeller = labeller(param = my_label_parsed),
    #            ...) +
    facet_wrap(~param,
               scales = "free",
               labeller = labeller(param = titles,
                                   .default = label_parsed),
               ...) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = facet_title_font_size,
                                    color = "black")) +
    xlab("") +
    ylab("")

  # xlim for each panel will be based on both the posterior and the prior
  # unless `ggh4x` is called b/c `ggplot2` doesn't allow for manipulation
  # of the axes by panel only across all panels with scales
  if(x_range == "posterior") {
    posts_ranges <- map(posts,
                        ~scale_x_continuous(limits = range(.x)))
    g <- g +
      facetted_pos_scales(x = posts_ranges)
  }

  g
}
