#' Make the retrospective recruitment deviations plot (AKA 'squid plot')
#'
#' @param model Model with retrospectives
#' @param model_nms A vector of the model names for the retrospectives. This
#' must be the same length as the number of retrospectives found in the
#' `model$retros` list
#' @param subplot 1 = Recruitment deviations, 2 = Rec dev strength relative
#' to most recent estimate
#' @param cohorts Vector of cohort years to plot (for labels)
#' @param plot_mcmc If `TRUE` plot the MCMC version based on medians. If
#' `FALSE` plot MLE version.
#' @param getdevs A logical value specifying if the plot should be of
#' recruitment deviations, which is the default. If `FALSE`, then
#' the squid plot will be made using absolute recruitment instead of
#' deviations.
#'
#' @export
plot_squid <- function(model,
                       show_ci = FALSE,
                       ci_alpha = 0.2,
                       ci_yrs = NULL,
                       cols = c("blue",
                                "green",
                                "orange",
                                "cyan",
                                "red")){

  model_lst <- map(model$retros, ~{.x})
  end_yr <- model$endyr

  # End years are different for all the models, which messes up the plot
  # so set them all to the base model end year
  # end_yr <- model_lst[[1]]$endyr
  # model_lst <- map(model_lst, function(mdl){
  #   mdl$endyr <- end_yr
  #   mdl
  # })

  cohorts <- map_dbl(model_lst, \(mdl){
    mdl$endyr
  }) |>
    sort()
  model_yrs <- seq(end_yr, end_yr - length(cohorts) + 1)

  d_obj <- create_group_df_recr(model_lst,
                                model_yrs,
                                devs = TRUE)
  d <- d_obj$d |>
    mutate(model = as.numeric(as.character(model))) |>
    filter(year >= min(cohorts)) |>
    filter(year <= model) |>
    mutate(age = model - year) |>
    select(model, year, age, everything())

  # col_func <- colorRampPalette(cols)
  # colors <- col_func(length(unique(d$year)))

browser()
  g <- ggplot(d,
              aes(x = age,
                  y = devmed,
                  color = factor(year))) #+
    # scale_color_manual(values = colors) +
    # scale_fill_manual(values = colors)

  if(show_ci){
    dat <- d
    if(!is.null(ci_yrs[1])){
      dat <- dat |>
        filter(year %in% ci_yrs)
    }
    g <- g +
      geom_ribbon(data = dat,
                  aes(ymin = devlower,
                      ymax = devupper,
                      x = age,
                      fill = factor(year)),
                  alpha = ci_alpha,
                  linetype = "dotted",
                  linewidth = 0.5) +
      geom_segment(data = dat |>
                     filter(model == end_yr) |>
                     mutate(year = factor(year)) |>
                     mutate(model = factor(model)),
                   aes(x = age,
                       xend = age,
                       y = devlower,
                       yend = devupper,
                       color = year),
                   linetype = "dotted",
                   linewidth = 0.5,
                   inherit.aes = FALSE)
  }

  g <- g +
    geom_line(linewidth = 1.5) +
    geom_point(size = 3) +
    scale_x_continuous(breaks = 0:11)

  g
}
