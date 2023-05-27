#' Title
#'
#' @param base_model
#' @param history_df
#' @param x_labs_mod Value for major X-axis tick marks. Every Nth tick
#' will be longer and have a label. The first and last will be shown
#' regardless of what this number is
#' @param y_breaks A vector of the values to show on the y-axis
#' @param clip_cover There is a white rectangle drawn on top of the plot
#' to cover any of the plot that made it outside the plot area. `clip` has to
#' be set to `off` for the major x-axis tick marks to work, So, this is required.
#' If you make the plot in a grid, the rectangle may overwrite some of the plot
#' above it, and this number will have to be changed through trial and error
#' until you cannot see the white rectangle anymore.
#' @param axis_title_font_size Size of the font for the X and Y axis labels
#' @param axis_tick_font_size Size of the font for the X and Y axis tick labels
#' @param axis_label_color Color for the axis labels and tick labels
#'
#' @return
#' @export
#'
#' @examples
plot_assessment_history_biomass <- function(base_model,
                                            history_df,
                                            x_labs_mod = 5,
                                            ylim = c(0, 7),
                                            y_breaks = ylim[1]:ylim[2],
                                            clip_cover = 5,
                                            axis_title_font_size = 14,
                                            axis_tick_font_size = 11,
                                            axis_label_color = "black"){

  base_ssb_df <- tibble(yr = as.numeric(names(base_model$mcmccalcs$smed)),
                        lo = base_model$mcmccalcs$slower,
                        med = base_model$mcmccalcs$smed,
                        hi = base_model$mcmccalcs$supper) |>
    filter(yr <= base_model$endyr + 1)

  base_ssb_last_yr_df <- base_ssb_df |>
    mutate(label = paste0(base_model$endyr + 1, " Base")) |>
    select(-c(lo, hi)) |>
    transmute(label, yr, value = med)

  j <- history_df |>
    filter(value == "SB million mt") |>
    filter(is.na(model) | model != "TINSS STAR update") |>
    filter(is.na(model) | model != "TINSS Post-STAR") |>
    filter(is.na(model) | model != "Base lowCI") |>
    filter(is.na(model) | model != "Base highCI") |>
    mutate(model = ifelse(model == "TINSS SSC Final",
                          "TINSS",
                          model)) |>
    mutate(label = ifelse(is.na(model),
                          yr,
                          paste0(yr, " ", model))) |>
    # In case someone entered the current year into the file already,
    # we want control over adding it because it has CIs as well and
    # needs to be black with a thicker line
    filter(yr <= base_model$endyr)

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
    filter(!is.na(value))

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
    filter(yr %in% c(min(yr), max(yr)))

  # X-axis breaks and tick placement ----
  xlim <- c(min(j$yr), max(j$yr))
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
  bot_y_pos <- ylim[1] - (ylim[2] - ylim[1]) / 25
  custom_ticks <- tibble(group = x_breaks_nth,
                         y_end = bot_y_pos)
browser()
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
              linewidth = 2,
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
    geom_point(aes(color = label,
                   shape = label),
               size = 3) +
    geom_line(aes(color = label),
              linewidth = 0.5) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks) +
    scale_x_continuous(expand = c(0, 0),
                       breaks = x_breaks,
                       labels = x_labels) +
    scale_color_manual(values = scale_map$color,
                       labels = scale_map$label,
                       name = "label") +
    scale_shape_manual(values = scale_map$shape,
                       labels = scale_map$label,
                       name = "label") +
    coord_cartesian(xlim = xlim,
                    ylim = ylim,
                    clip = "off") +
    xlab("Year") +
    ylab("Female spawning biomass (Mt)") +
    theme(axis.text.x = element_text(color = axis_label_color,
                                     size = axis_tick_font_size,
                                     angle = 0,
                                     hjust = 0.5,
                                     vjust = -0.25,
                                     face = "plain"),
          axis.text.y = element_text(color = axis_label_color,
                                     size = axis_tick_font_size,
                                     hjust = 1,
                                     vjust = 0.5,
                                     face = "plain"),
          axis.title.x = element_text(color = axis_label_color,
                                      size = axis_title_font_size,
                                      angle = 0,
                                      vjust = 0,
                                      face = "plain"),
          axis.title.y = element_text(color = axis_label_color,
                                      size = axis_title_font_size,
                                      angle = 90,
                                      face = "plain"),
          # plot.margin: top, right,bottom, left
          plot.margin = margin(12, 6, 6, 6)) +
    # Add major tick marks
    geom_linerange(data = custom_ticks,
                   aes(x = group,
                       ymax = top_y_pos,
                       ymin = y_end),
                   size = 0.5,
                   inherit.aes = FALSE)


  # Draw a white rectangle over the top of the plot, obscuring any
  # unclipped plot parts. Clipping has to be off to allow different size
  # tick marks. `grid` package used here
  g <- g +
    annotation_custom(grob = rectGrob(gp = gpar(col = NA, fill = "white")),
                      xmin = xlim[1],
                      xmax = xlim[2],
                      ymin = ylim[2],
                      ymax = ylim[2] + clip_cover)

  g
}

