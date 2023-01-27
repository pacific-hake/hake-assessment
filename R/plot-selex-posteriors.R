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
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_selex_posteriors <- function(model = NULL,
                                  type = c("fishery", "survey"),
                                  probs = c(0.025, 0.975),
                                  age_range = NULL,
                                  line_color = "black",
                                  line_thickness = 0.05,
                                  line_color_unc = "red",
                                  line_thickness_unc = 1,
                                  point_size_unc = 3){

  type <- match.arg(type)
  if(type == "fishery"){
    selex <- base_model$extra.mcmc$sel_endyr_fishery
  }else if(type == "survey"){
    selex <- base_model$extra.mcmc$sel_endyr_survey
  }
  ages <- as.numeric(names(selex))

  quants <- tibble(iter = 1,
                   age = factor(ages, levels = ages),
                   lower = apply(selex, 2, quantile, prob = probs[1]),
                   med = apply(selex, 2, median),
                   upper = apply(selex, 2, quantile, prob = probs[2]))

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
  }

  g <- ggplot(sel,
              aes(x = age, y = med, group = iter)) +
    geom_line(linewidth = line_thickness,
              color = line_color) +
    geom_line(data = quants,
              color = line_color_unc,
              linewidth = line_thickness_unc) +
    geom_point(data = quants,
               color = line_color_unc,
               size = point_size_unc) +
    geom_errorbar(data = quants, aes(ymin = lower,
                                     ymax = upper),
                  color = line_color_unc,
                  linewidth = line_thickness_unc,
                  width = 0.01) +
    xlab("Age") +
    ylab("Selectivity") +
    coord_cartesian(xlim = age_range)

  g
}
