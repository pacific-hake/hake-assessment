#' Plot the selectivity parameter estimates from all posteriors
#' as many lines with an overlaid errorbar for each age
#'
#' @param model A model list, created by [create_rds_file()]
#' @param type One of 1 or 2 where 1 = Fishery and 2 = Acoustic age 2+ survey
#' @param n_posts The number of posterior lines to plot. The lines are
#' randomly drawn from all posteriors. if this is larger than the number
#' of posteriors, all posterior lines will be shown
#' @param probs A vector of two quantiles to include for the uncertainty
#' @param age_range A vector of two for the min and max ages to plot. If `NULL`,
#' all ages in the data will be plotted
#' @param show_xlab Logical. If `TRUE`, show the X-axis label. If `FALSE`,
#' omit it
#' @param post_line_width Thickness of the posterior lines
#' @param post_line_color Color of the posterior lines
#' @param post_line_alpha Transparency of the posterior lines
#' @param post_med_line_width Thickness of the median posterior line
#' @param post_med_line_color Color of the median posterior line
#' @param post_med_line_alpha Transparency of the median posterior line
#' @param post_med_point_size Size of the points on the median posterior line
#' @param unc_line_width Thickness of the uncertainty lines
#' @param unc_line_color Color of the uncertainty lines and points
#' @param unc_line_alpha Transparency for the uncertainty lines and points
#' @param unc_point_size Size of the points
#' @param glow Logical. If `TRUE`, add a white glow around the lines so that
#' they can more easily be seen in contrast to the background posterior lines
#' @param glow_offset The amount to add to the thickness of the lines and
#' diameter of the points to create the glow effect
#' @param glow_color Color of the glow effect
#' @param glow_alpha Transparency of the glow effect
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_selex_posteriors <- function(
    model = NULL,
    type = c("fishery", "survey"),
    n_posts = NULL,
    age_range = NULL,
    show_xlab = TRUE,
    post_line_width = 0.1,
    post_line_color = "black",
    post_line_alpha = 0.1,
    post_med_line_width = 1,
    post_med_line_color = "blue3",
    post_med_line_alpha = 1,
    post_line_gap = ts_linegap,
    post_med_point_size = 3,
    unc_line_width = 1,
    unc_line_color = "blue3",
    unc_line_alpha = 1,
    unc_point_size = 3,
    glow = FALSE,
    glow_offset = 0.5,
    glow_color = "white",
    glow_alpha = 1){

  type <- match.arg(type)
  if(type == "fishery"){
    selex <- model$extra_mcmc$sel_fishery_end_yr
  }else if(type == "survey"){
    selex <- model$extra_mcmc$sel_survey_end_yr
  }
  ages <- as.numeric(names(selex))

  quants <- tibble(iter = 1,
                   age = factor(ages, levels = ages),
                   lower = apply(selex, 2, quantile, prob = probs[1]),
                   med = apply(selex, 2, quantile, prob = probs[2]),
                   upper = apply(selex, 2, quantile, prob = probs[3]))

  # Total number of samples available, and the number of posteriors requested
  # which, if `NULL` becomes the number of samples. Adapted from plot_survey_fit_mcmc().
  n_samp <- model$extra_mcmc$num_posts
  n_posts <- n_posts %||% n_samp
  if(n_samp < n_posts){
    warning("the number of posteriors available (", n_samp, ") is less than ",
            "the requested number of samples to use in the plot (", n_posts,
            "). Using all available samples (", n_samp, ")")
    n_posts <- n_samp
  }
  subsample <- sample(n_samp, n_posts)

  selex <- selex |>
    mutate(iter = row_number()) |>
    # Take the random subsample of posterior numbers
    dplyr::filter(iter %in% subsample) |>
    select(iter, everything())

  sel <- selex |>
    pivot_longer(-iter, names_to = "age", values_to = "med") |>
    mutate(age = factor(age, ages))

  if(!is.null(age_range)){
    if(!all(age_range[1]:age_range[2] %in% ages)){
      stop("`age_range` is out of range of the data. You must choose values ",
           "between ", min(ages), " and ", max(ages))
    }
    sel <- sel |>
      dplyr::filter(age %in% age_range[1]:age_range[2])
    quants <- quants |>
      dplyr::filter(age %in% age_range[1]:age_range[2])
  }

  g <- ggplot(sel,
              aes(x = age, y = med, group = iter)) +
    # Thin posterior lines (no glow for these)
    geom_line(linewidth = post_line_width,
              color = post_line_color,
              alpha = post_line_alpha)

  if(glow){
    # Glow - Error bars only - no points here
    g <- g +
      geom_errorbar(data = quants,
                    aes(ymin = lower,
                        ymax = upper),
                    linewidth = unc_line_width + glow_offset,
                    color = glow_color,
                    alpha = glow_alpha,
                    lineend = "round",
                    size = unc_point_size + glow_offset,
                    width = 0)
  }

  g <- g +
    # Error bars only - no points here
    geom_errorbar(data = quants, aes(ymin = lower,
                                     ymax = upper),
                  linewidth = unc_line_width,
                  color = unc_line_color,
                  alpha = unc_line_alpha,
                  lineend = "round",
                  size = unc_point_size,
                  width = 0)

  if(glow){
    g <- g +
      # Glow 1 - Median posterior point size the same as the "real" point size.
      # This is needed because the lines connecting the points become shorter
      # when the points are larger for the glow effect, so the glow layer
      # will not fully encapsulate the ends on the in-between lines. This
      # fixes the glow at those line ends
      geom_pointpath(data = quants,
                     linewidth = post_med_line_width + glow_offset,
                     color = glow_color,
                     alpha = glow_alpha,
                     mult = post_line_gap,
                     size = post_med_point_size) +
      # Glow 2 - Median posterior line and points
      geom_pointpath(data = quants,
                     linewidth = post_med_line_width + glow_offset,
                     color = glow_color,
                     alpha = glow_alpha,
                     mult = post_line_gap,
                     size = post_med_point_size + glow_offset)
  }

  g <- g +
    # Median posterior line and points
    geom_pointpath(data = quants,
                   linewidth = post_med_line_width,
                   color = post_med_line_color,
                   mult = post_line_gap,
                   size = post_med_point_size) +
    xlab(ifelse(show_xlab, "Age", "")) +
    ylab("Selectivity")

  g
}
