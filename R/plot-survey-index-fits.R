#' Plot survey index fits from MCMC output for one or more models,
#' for one of the acoustic surveys
#'
#' @param model_lst A list of models, each created by [create_rds_file()]
#' @param model_names A vector of model names,the same length as `models_lst`
#' @param survey_type The type of survey, must be one of `age1` or `age2`.
#' `age2` means age 2+ acoustic survey and `age1``is the age 1 acoustic survey
#' @param xlim The year limits to plot
#' @param x_breaks The year value tick marks to show for the x axis
#' @param ylim The depletion limits to plot
#' @param y_breaks The depletion value tick marks to show for the y axis
#' @param y_labels The depletion labels to show for the y axis tick marks
#' @param alpha The transparency for all ribbons
#' @param leg_pos The position of the legend inside the plot. If `NULL`,
#' `NA`, or `none`, the legend will not be shown
#' @param leg_font_size The legend font size
#' @param point_size Size of all points shownin plot
#' @param line_width Width of all lines on the plot
#' @param dodge_val The amount to offset the lines from each other in the
#' case of multiple models
#' @param d_obj If not `NULL` this is a list which has been
#' pre-processed to contain all models in a format that is ready to plot.
#' Essentially the first steps of this function have been replicated
#' outside the function (The code inside the `if(is.null(d_obj))`)
#' is done to stop the Rmd process from taking forever
#'
#' @return a [ggplot2::ggplot()] object
#' @export
plot_survey_index_fits <- function(
    model_lst,
    model_names,
    survey_type = c("age1",
                    "age2"),
    xlim = c(1995, 2021),
    x_breaks = seq(1995, 2021, by = 5),
    ylim = c(0, 3),
    y_breaks = seq(ylim[1], ylim[2], by = 0.5),
    y_labels = expression("0", "0.5", "1", "1.5", "2", "2.5", "3"),
    alpha = 0.1,
    leg_pos = c(0.65, 0.83),
    leg_font_size = 12,
    point_size = 1.5,
    line_width = 0.5,
    dodge_val = 0.5,
    d_obj = NULL){

  survey_type <- match.arg(survey_type)
  fleet <- ifelse(survey_type == "age2", 2, 3)

  if(is.null(d_obj)){
    obs <- model_lst[[1]]$dat$CPUE |>
      as_tibble() |>
      filter(index == fleet) |>
      select(-seas, -se_log, -index) |>
      setNames(c("year", "index.med")) |>
      mutate(year = as.numeric(year)) |>
      mutate(model = "Observed") |>
      mutate(index.025 = index.med,
             index.975 = index.med) |>
      select(model, year, index.025, index.med, index.975)

    d <- bind_cols(extract_survey_index_fits(model_lst,
                                             model_names,
                                             survey_type,
                                             "index.025", TRUE),
                   extract_survey_index_fits(model_lst,
                                             model_names,
                                             survey_type,
                                             "index.med"),
                   extract_survey_index_fits(model_lst,
                                             model_names,
                                             survey_type,
                                             "index.975")) |>
      bind_rows(obs) |>
      mutate(model = factor(model, levels = c(model_names, "Observed")),
             year = as.numeric(year)) |>
      mutate_at(vars(index.025, index.med, index.975),
                ~{.x / 1e6})
  }else{
    d <- d_obj[[1]]
  }

  colors <- plot_color(length(unique(d$model)))

  g <- ggplot(d,
              aes(x = year,
                  y = index.med,
                  ymin = index.025,
                  ymax = index.975,
                  group = model,
                  color = model,
                  fill = model)) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    coord_cartesian(xlim = xlim,
                    ylim = ylim) +
    geom_hline(yintercept = 0,
               color = "blue",
               linetype = "dotted",
               size = 1) +
    geom_point(size = point_size,
               position = position_dodge(dodge_val)) +
    geom_line(size = line_width,
              position = position_dodge(dodge_val)) +
    geom_errorbar(size = line_width,
                  position = position_dodge(dodge_val)) +
    scale_x_continuous(expand = c(0, 0),
                       breaks = x_breaks,
                       labels = x_breaks) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       labels = y_breaks) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = leg_font_size),
          # plot.margin: top, right,bottom, left
          # Needed to avoid tick labels cutting off
          plot.margin = margin(12, 12, 0, 0)) +
    xlab("Year") +
    ylab("Biomass (million t)")

  if(is.null(leg_pos[1]) || is.na(leg_pos[1])){
    g <- g +
      theme(legend.position = "none")
  }else{
    g <- g +
      theme(legend.position = leg_pos)
  }

  g
}