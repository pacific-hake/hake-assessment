#' Make the retrospective recruitment deviations plot (squid plot)
#'
#' @param model Model with retrospectives
#' @param relative Logical. If `TRUE`, plot the deviaitons relative to the
#' most recent estimate for each cohort
#' @param show_ci Logical. If `TRUE`, plot the credible interval ribbons
#' around the median lines
#' @param ci_alpha The a transparency value for the credible interval
#' ribbon fill
#' @param ci_yrs A vector of years to include credible intervals for.
#'   If `NULL`, all will be shown. Only used if `show_ci` is `TRUE`
#' @param cohorts A vector of years so as to plot only those cohorts if available (useful for
#'   talks). If `NULL` (the default) plots all available cohorts.
#' @param year_label_font_size Size of the font for the year labels
#' @param x_lim A vector to specify x limits, default is to span what is being
#' plotted.
#' @param y_lim A vector to specify y limits, default is to span what is being
#'   plotted. If `full` then automatically span all the
#'   credible intervals even when not showing them all (useful
#'   for showing sequential plots in a talk). Not tested with `relative = TRUE`
#'   and `show_ci = TRUE` as we do not currently use those.
#' @param surv_point_type The point shape type for ages in age-1 index years
#' @param reg_point_type The point shape type for ages in non-age-1-index years
#' @param color_offset A value to offset the colors to compared to the ten
#' retrospective years. This may be used when comparing model where only five
#' retrospective years were run with those that have ten.
#' @export
plot_squid <- function(model,
                       relative = FALSE,
                       show_ci = FALSE,
                       ci_alpha = 0.2,
                       ci_yrs = NULL,
                       year_label_font_size = 4,
                       y_lim = c(NA, NA),
                       x_lim = c(NA, NA),
                       surv_point_type = 17,
                       reg_point_type = 19,
                       cohorts = NULL,
                       color_offset = 0){

  # Extract a data frame of long-format recruitment deviations containing all
  # the models in the model list
  d <- model$retrospectives$recdevs_df$d

  if(!is.null(cohorts)){
    cohorts <- intersect(cohorts,
                         as.numeric(levels(d$model)) - 1) # only use those available
    } else {
      cohorts <- as.numeric(levels(d$model)) - 1
    }

  # Colors of lines and fill - add black to the beginning and remove end color
  # The B3 here is the transparency, B3 = 179 decimal out of possible 255 (FF),
  # So B3 means it is about 70% opaque
  # colors <- c("#000000B3",
  #             rev(rich_colors_short(length(cohorts))[-1]))

  colors <- c("#000000B3",
              rev(rich_colors_short(10)[-1]))

  colors <- colors[color_offset:(color_offset + length(cohorts) - 1)]

  # Add flag for whether the year was a survey year or not.
  d <- d |>
    mutate(point_type = ifelse(model %in% survey_age1_yrs,
                               surv_point_type,
                               reg_point_type))

  # Add the retrospective ages to the long-format data frame extracted above
  # "This is where the magic happens"
  d <- d |>
    # Need to do this to convert from factor to numeric
    mutate(model = as.numeric(as.character(model))) |>
    dplyr::filter(year %in% cohorts) |>
    # Create a list of data frames, one for each cohort
    split(~year) |>
    set_names(NULL) |>
    # For each cohort, add the ages to the data frame based on the
    # year of assessment and the estimated year
    imap(~{
      .x |>
        # Add color column
        bind_cols(tibble(clr = colors[.y])) |>
        dplyr::filter(year <= model) |>
        mutate(age = model - year)
    }) |>
    # Glue the list of data frames back into a single data frame
    map_df(~{.x})

  # Set all NAs to zero, as they represent recdevs which are fixed
  # (new for 2025)
  d <- d |>
    map_df(~{ifelse(is.na(.x), 0, .x)})

  if(relative){
    # Subtract the last year's deviate values, which is in the first row
    # for each cohort group from the deviate values for all other ages in the
    # cohort group. The line shape is the same as in the non-relative plot,
    # just shifted up or down
    d <- d |>
      split(~year) |>
      # Create a list of data frames, one for each cohort
      map_dfr(~{
        first_row <- .x |>
          slice(1)
        .x <- .x |>
          # For each row in the current cohort data frame ...
          pmap(~{
            cols <- list(...)
            # Set values to the values minus the values in the first row
            cols$devlower <- cols$devlower - first_row$devlower
            cols$devmed <- cols$devmed - first_row$devmed
            cols$devupper <- cols$devupper - first_row$devupper
            cols
        })
      })
  }

  # Make y_lim cover full range of intervals even when showing only some intervals
  if(length(y_lim) == 1){
    if(y_lim == "full"){
      y_lim <- c(min(d$devlower),
                 max(d$devupper))
    }
  }

  g <- ggplot(d,
              aes(x = age,
                  y = devmed,
                  color = clr))

  # Show the credible interval ribbons if requested
  if(show_ci){
    ribbon_dat <- d
    if(!is.null(ci_yrs[1])){
      ribbon_dat <- ribbon_dat |>
        dplyr::filter(year %in% ci_yrs)
    }
    vert_lines_dat <- ribbon_dat |>
      dplyr::filter(model %in% min(model):max(model)) |>
      mutate(year = factor(year)) |>
      mutate(model = factor(model))

    g <- g +
      geom_ribbon(data = ribbon_dat,
                  aes(ymin = devlower,
                      ymax = devupper,
                      x = age,
                      fill = clr),
                  alpha = ci_alpha,
                  linetype = "dotted",
                  linewidth = 0.5) +
      # Add a vertical line at the ends to close the ribbon nicely
      geom_segment(data = vert_lines_dat,
                   aes(x = age,
                       xend = age,
                       y = devlower,
                       yend = devupper,
                       color = clr),
                   linetype = "dotted",
                   linewidth = 0.5,
                   inherit.aes = FALSE) +
      scale_fill_identity()

  }

  # A data frame of the text labels showing the year.
  # Used in geom_text_repel() below
  text_df <- d |>
    split(~year) |>
    map_df(~{
      .x |>
        slice(ifelse(relative, nrow(.x), 1))
    })

  g <- g +
    geom_hline(yintercept = 0,
               linewidth = 0.25) +
    geom_hline(yintercept = c(-2, 2),
               linetype = "dashed",
               linewidth = 0.1) +
    geom_line(linewidth = 1.5) +
    # Add white border around points so that they stand out from the line more
    geom_point(size = 4,
               stat = "identity",
               shape = d$point_type,
               color = "white") +
    geom_point(size = 3,
               stat = "identity",
               shape = d$point_type) +
    geom_text_repel(data = text_df,
                    aes(x = age,
                        y = devmed,
                        label = year),
                    seed = 1,
                    size = year_label_font_size,
                    # -1 multiplier places label to left or right depending
                    # on if relative or not
                    nudge_x = 0.5 * ifelse(relative, -1, 1),
                    nudge_y = -0.25,
                    direction = "both") +
    scale_x_continuous(breaks = seq(0, 20),
                       limits = x_lim) +
    scale_y_continuous(breaks = seq(-10, 10),
                       limits = y_lim) +
    scale_color_identity() +
    xlab("Age") +
    ylab(ifelse(relative,
                "Recruitment deviation relative to recent estimate",
                "Recruitment deviation")) +
  theme(legend.position = "none",
        # plot.margin: top, right,bottom, left
        plot.margin = margin(0, 6, 6, 6))

  g
}
