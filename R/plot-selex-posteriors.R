#' Plot the selectivity parameter estimates from all posteriors
#' as many lines with an overlaid errorbar for each age
#'
#' @param model A model list, created by [create_rds_file()]
#' @param type One of 1 or 2 where 1 = Fishery and 2 = Acoustic age 2+ survey
#' @param probs A vector of two quantiles to include for the uncertainty
#' @param age_range A vector of two for the min and max ages to plot. If `NULL`,
#' all ages in the data will be plotted
#' @param line_color Color of the posterior lines
#' @param line_thickness Thickness of the posterior lines
#' @param line_color_unc Color of the uncertainty lines and points
#' @param line_thickness_unc Thickness of the uncertainty lines
#' @param point_size_unc Size of the points
#' @param line_alpha Transparency of the posterior lines
#' @param glow Logical. If `TRUE`, add a white glow around the lines so that
#' they can more easily be seen in contrast to the background posterior lines
#' @param glow_color Color of the glow effect
#' @param glow_alpha Transparency of the glow effect
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_selex_posteriors <- function(model = NULL,
                                  type = c("fishery", "survey"),
                                  probs = c(0.025, 0.5, 0.975),
                                  age_range = NULL,
                                  line_color = "black",
                                  line_alpha = 0.1,
                                  line_thickness = 0.1,
                                  line_color_unc = "red",
                                  line_thickness_unc = 1,
                                  point_size_unc = 3,
                                  glow = TRUE,
                                  glow_color = "white",
                                  glow_alpha = 0.5){

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

  selex <- selex |>
    mutate(iter = row_number()) |>
    select(iter, everything())
  sel <- selex |>
    pivot_longer(-iter, names_to = "age", values_to = "med") |>
    mutate(age = factor(age, ages))

  if(!is.null(age_range)){
    if(!all(age_range[1]:age_range[2] %in% ages)){
      stop("`age_range` is out of range of the data. You must choose values ",
           "between ", min(ages), " and ", max(ages),
           call. = FALSE)
    }
    sel <- sel |>
      filter(age %in% age_range[1]:age_range[2])
    quants <- quants |>
      filter(age %in% age_range[1]:age_range[2])
  }

  g <- ggplot(sel,
              aes(x = age, y = med, group = iter)) +
    geom_line(linewidth = line_thickness,
              color = line_color,
              alpha = line_alpha) +
    geom_errorbar(data = quants, aes(ymin = lower,
                                     ymax = upper),
                  color = glow_color,
                  alpha = glow_alpha,
                  lineend = "round",
                  linewidth = line_thickness_unc + 0.5,
                  width = 0) +
    geom_errorbar(data = quants, aes(ymin = lower,
                                     ymax = upper),
                  color = line_color_unc,
                  lineend = "round",
                  linewidth = line_thickness_unc,
                  width = 0) +
    geom_line(data = quants,
              color = glow_color,
              alpha = glow_alpha,
              lineend = "round",
              linewidth = line_thickness_unc + 0.5) +
    geom_line(data = quants,
              color = line_color_unc,
              linewidth = line_thickness_unc) +
    geom_point(data = quants,
               color = glow_color,
               alpha = glow_alpha,
               size = point_size_unc + 1) +
    geom_point(data = quants,
               color = line_color_unc,
               size = point_size_unc) +
    xlab("Age") +
    ylab("Selectivity")

  g
}
