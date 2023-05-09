#' Make a grid of posterior plots with priors and initial values
#'
#' @param model The model output from Stock Synthesis as loaded by
#'   [create_rds_file()].
#' @param posterior_regex A vector of regular expressions that can be matched
#'   to parameter names. Order the vector in the same order as you want the
#'   figures to be plotted but make sure you also have `titles` ordered
#'   appropriately as well otherwise the names will not line up.
#' @param titles A vector of titles for the plots. The titles can be used as is
#'   or they can be transformed via a labeller passed to
#'   [ggplot2::facet_wrap()] via `...`. The default value of `NULL` leads to
#'   each panel being labelled with a lower-case letter enclosed in open
#'   brackets, e.g., `(a)`. If you truly do not want a label, just pass the
#'   argument a vector of empty strings, e.g., `c("", "", ...)`, with the same
#'   length as the result of parameters that are found via `posterior_regex`.
#' @param x_range A string specifying the method to limit the range of each
#'   x axis in the panels. Typically, all geoms used in the figure dictate the
#'   range. But, with the use of [ggh4x::facetted_pos_scales()] we can specify
#'   which data set should specify the range limits. Setting this argument to
#'   `"posterior"`, instead of the default `"prior"`, will limit the ranges to
#'   only the realized values in the posterior. This can be helpful when the
#'   prior is quite vague and the posterior only covers a small range of the
#'   parameter space.
#' @param ... Parameters to be passed to [ggplot2::facet_wrap()]. For example,
#'   `labeller = label_parsed_space`, which is available in this package, will
#'   remove the spaces from the strings and implement
#'   [ggplot2::label_parsed()]. And, `ncol` and `nrow` can be used to direct
#'   the output or you can let {ggplot2} figure it out.
#'
#' @return A {ggplot2} object.
#' @export
plot_priors_vs_posts <- function(model,
                                 titles = NULL,
                                 x_range = c("posterior", "prior"),
                                 ...){
  x_range <- match.arg(x_range)

  titles <- if(is.null(titles)){
    glue("({letters[seq_along(posts)]})")
  }else{
    titles
  }
  browser()
  priors <- model$parameter_priors
  posts <- model$parameter_posts
  names(priors) <- titles

  posts_long <- posts |>
    pivot_longer(everything(), names_to = "param")

  priors_long <- map(priors, "prior_random") |>
    as_tibble() |>
    pivot_longer(everything(), names_to = "param")

  # Make long data frames for ggplot2
  posteriors_long <- enframe(posts) |>
    unnest(cols = "value") |>
    mutate(parameter = factor(name, levels = titles))
  posteriors_ranges <- map(
    posts, ~ scale_x_continuous(limits = range(.x)))
  priors_long <- map(priors, "prior_random") |>
    enframe() |>
    unnest(cols = "value") |>
    mutate(parameter = factor(name, levels = titles)) |>
    filter(!is.na(value))
  priors_init <- map(priors, "initval") |>
    unlist() |>
    enframe() |>
    mutate(parameter = factor(name, levels = titles))
  gg <- ggplot() +
    geom_histogram(data = posteriors_long,
                   mapping = aes(value, after_stat(density)),
                   fill = "gray60",
                   bins = 30) +
    geom_density(data = priors_long,
                 mapping = aes(value, after_stat(density)),
                 col = "black",
                 linewidth = 1.2) +
    geom_point(data = priors_init,
               mapping = aes(x = value, y = 0),
               col = "red",
               pch = 17) +
    geom_vline(data = group_by(posteriors_long, parameter) |>
                 summarize(median = median(value)),
               mapping = aes(xintercept = median),
               linetype = 2,
               col = rgb(0, 0, 0, 0.5)) +
    facet_wrap("parameter", scales = "free", ...) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    xlab("") +
    ylab("")

  # xlim for each panel will be based on both the posterior and the prior
  # unless `ggh4x` is called b/c `ggplot2` doesn't allow for manipulation
  # of the axes by panel only across all panels with scales
  if (x_range == "posterior") {
    gg <- gg +
      ggh4x::facetted_pos_scales(x = posteriors_ranges)
  }

  return(gg)
}
