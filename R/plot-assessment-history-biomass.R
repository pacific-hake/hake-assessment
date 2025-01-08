#' Title
#'
#' @param base_model A model object, created by [create_rds_file()]
#' @param history_df A data frame, previously read in from the file
#' `assessment-history.csv`
#' @param x_labs_mod Value for major X-axis tick marks. Every Nth tick
#' will be longer and have a label. The first and last will be shown
#' regardless of what this number is
#' @param ylim A vector of two representing the minimum and maximum values to
#' show on the y-axis
#' @param y_breaks A vector of the values to show on the y-axis
#' @param y_major_lab_adj A value to move the major tick abels closer/further
#' away from the major tick marks. Reducing this moves them closer, increasing
#' it moves them further away
#' @param leg_pos The position of the legend inside the plot. If `NULL`,
#' `NA`, or `none`, the legend will not be shown
#' @param leg_ncol The number of columns to show in the legend
#' @param leg_font_size The legend font size
#' @param axis_title_font_size Size of the font for the X and Y axis labels
#' @param axis_tick_font_size Size of the font for the X and Y axis tick labels
#' @param axis_label_color Color for the axis labels and tick labels
#'
#' @return a [ggplot2::ggplot()] object
#' @export
plot_assessment_history_biomass <- function(base_model,
                                            history_df,
                                            x_labs_mod = 10,
                                            ylim = c(0, 7),
                                            y_breaks = ylim[1]:ylim[2],
                                            y_major_lab_adj = 45,
                                            tick_prop = 1,
                                            vjust_x_labels = -0.25,
                                            leg_pos = c(0.75, 0.8),
                                            leg_ncol = 3,
                                            leg_font_size = 8){

  base_ssb_df <- tibble(yr = as.numeric(names(base_model$mcmccalcs$smed)),
                        lo = base_model$mcmccalcs$slower,
                        med = base_model$mcmccalcs$smed,
                        hi = base_model$mcmccalcs$supper) |>
    dplyr::filter(yr <= base_model$endyr + 1)

  base_ssb_last_yr_df <- base_ssb_df |>
    mutate(label = paste0(base_model$endyr + 1, " Base")) |>
    select(-c(lo, hi)) |>
    transmute(label, yr, value = med)

  j <- history_df |>
    dplyr::filter(value == "SB million mt") |>
    dplyr::filter(is.na(model) | model != "TINSS STAR update") |>
    dplyr::filter(is.na(model) | model != "TINSS Post-STAR") |>
    dplyr::filter(is.na(model) | model != "Base lowCI") |>
    dplyr::filter(is.na(model) | model != "Base highCI") |>
    mutate(model = ifelse(model == "TINSS SSC Final",
                          "TINSS",
                          model)) |>
    mutate(label = ifelse(is.na(model),
                          yr,
                          paste0(yr, " ", model))) |>
    # In case someone entered the current year into the file already,
    # we want control over adding it because it has CIs as well and
    # needs to be black with a thicker line
    dplyr::filter(yr <= base_model$endyr)

  j <- j |>
    select(-c(fishery_independent_cpue,
              author,
              model,
              value,
              yr,
              model_type)) |>
    select(label, everything()) |>
    pivot_longer(-label, names_to = "yr", values_to = "value") |>
    mutate(yr = as.numeric(yr)) |>
    # Add current year median so that it will appear in the legend
    bind_rows(base_ssb_last_yr_df) |>
    mutate(label = factor(label)) |>
    dplyr::filter(!is.na(value))

  # Colors and shapes of lines and points , not including the
  # current base model
  colors <- rev(rich_colors_short(length(unique(j$label))))
  shapes <- rep_len(c(0:6, 15:19), length.out = length(unique(j$label)))
  # Need to have the custom colors and shapes in its own data frame
  # (aesthetics map data frame)
  scale_map <- tibble(label = unique(j$label),
                      color = colors,
                      shape = shapes) |>
    mutate(shape = ifelse(label == paste0(base_model$endyr + 1, " Base"),
                          NA,
                          shape))
  # For vertical lines on ends of current year biomass ribbon
  vert_lines_dat <- base_ssb_df |>
    dplyr::filter(yr %in% c(min(yr), max(yr)))

  # Change `label`to `Assessment` for the legend title
  j <- j |>
    rename(Assessment = label)
  scale_map <- scale_map |>
    rename(Assessment = label)

  # X-axis breaks and tick placement ----
  xlim <- c(min(j$yr) - 2, max(j$yr) + 2)
  x_breaks <-  xlim[1]:xlim[2]
  # Remove labels for the minor x-axis ticks
  x_labels <- NULL
  for(i in x_breaks){
    if(i %% x_labs_mod == 0){
      x_labels <- c(x_labels, i)
    }else{
      x_labels <- c(x_labels, "")
    }
  }

  # Major tick mark lengths adjusted here
  x_breaks_nth <- x_breaks[x_breaks %% x_labs_mod == 0]
  top_y_pos <- ylim[1]
  bot_y_pos <- ylim[1] - (ylim[2] - ylim[1]) / y_major_lab_adj
  custom_ticks <- tibble(group = x_breaks_nth,
                         y_end = bot_y_pos)

  g <- ggplot(j,
              aes(x = yr,
                  y = value)) +
    # Add the current year's median biomass and CI
    geom_ribbon(data = base_ssb_df,
                aes(x = yr,
                    ymin = lo,
                    ymax = hi),
                color = "black",
                linetype = "dotted",
                alpha = 0.3,
                inherit.aes = FALSE) +
    geom_line(data = base_ssb_df,
              aes(x = yr,
                  y = med),
              linewidth = 1,
              color = "black",
              inherit.aes = FALSE) +
    # Add a vertical line at the ends to close the ribbon nicely
    geom_segment(data = vert_lines_dat,
                 aes(x = yr,
                     xend = yr,
                     y = lo,
                     yend = hi),
                 linetype = "dotted",
                 inherit.aes = FALSE) +
    geom_point(aes(color = Assessment,
                   shape = Assessment),
               size = 1) +
    geom_line(aes(color = Assessment),
              linewidth = 0.5) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks) +
    scale_x_continuous(expand = c(0, 0),
                       breaks = x_breaks,
                       labels = x_labels) +
    scale_color_manual(values = scale_map$color,
                       labels = scale_map$Assessment,
                       name = "Assessment") +
    scale_shape_manual(values = scale_map$shape,
                       labels = scale_map$Assessment,
                       name = "Assessment") +
    coord_cartesian(xlim = xlim,
                    ylim = ylim,
                    clip = "off") +
    xlab("Year") +
    ylab("Female spawning biomass (Mt)") +
    theme(legend.key.size = unit(0.2, 'cm'),
          legend.text = element_text(size = leg_font_size),
          legend.text.align = 0,
          axis.text.x = element_text(vjust = vjust_x_labels),
          axis.title.x = element_text(vjust = vjust_x_labels),
          axis.title.y = element_text(vjust = 2),
          # plot.margin: top, right,bottom, left
          plot.margin = margin(12, 14, 6, 6)) +
    # Add major tick marks
    geom_linerange(data = custom_ticks,
                   aes(x = group,
                       ymax = top_y_pos,
                       ymin = y_end),
                   size = 0.5,
                   inherit.aes = FALSE)

  if(is.null(leg_pos[1]) || is.na(leg_pos[1])){
    g <- g +
      theme(legend.position = "none")
  }else{
    g <- g +
      theme(legend.position = leg_pos) +
      guides(fill = guide_legend(ncol = leg_ncol),
             color = guide_legend(ncol = leg_ncol))
  }

  # Add a major tick mark every `x_labs_mod` years
  g <- g |>
    add_major_ticks(x_breaks = x_breaks,
                    modulo = x_labs_mod,
                    prop = tick_prop)

  g
}
