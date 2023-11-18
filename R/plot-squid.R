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
#' If `NULL`, all will be shown. Only used if `show_ci` is `TRUE`
#' @param year_label_font_size Size of the font for the year labels
#'
#' @export
plot_squid <- function(model,
                       relative = FALSE,
                       show_ci = FALSE,
                       ci_alpha = 0.2,
                       ci_yrs = NULL,
                       year_label_font_size = 4){

  # Create a list of models, with the core model being first, followed by
  # all the retrospectives located inside it
  #model_lst <- c(list(model), map(model$retrospectives, ~{.x}))
  #end_yr <- model$endyr

  # Extract the cohort list from the list of models
  #cohorts <- map_dbl(model_lst, \(mdl){
  #  mdl$endyr
  #}) |>
  #  sort()

  # Extract a data frame of long-format recruitment deviations containing all
  # the models in the model list
  d <- model$retrospectives$recdevs_df$d
  cohorts <- as.numeric(levels(d$model)) - 1

  # Colors of lines and fill - add black to the beginning and remove end color
  # The B3 here is the transparency, B3 = 179 decimal out of possible 255 (FF),
  # So B3 means it is about 70% opaque
  colors <- c("#000000B3",
              rev(rich_colors_short(length(cohorts))[-1]))

  # Add the retrospective ages to the long-format data frame extracted above
  # "This is where the magic happens"
  d <- d |>
    # Need to do this to convert from factor to numeric
    mutate(model = as.numeric(as.character(model))) |>
    filter(year %in% cohorts) |>
    # Create a list of data frames, one for each cohort
    split(~year) |>
    set_names(NULL) |>
    # For each cohort, add the ages to the data frame based on the
    # year of assessment and the estimated year
    imap(~{
      .x |>
        # Add color column
        bind_cols(tibble(clr = colors[.y])) |>
        filter(year <= model) |>
        mutate(age = model - year)
    }) |>
    # Glue the list of data frames back into a single data frame
    map_df(~{.x})


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

  g <- ggplot(d,
              aes(x = age,
                  y = devmed,
                  color = clr))

  # Show the credible interval ribbons if requested
  if(show_ci){
    ribbon_dat <- d
    if(!is.null(ci_yrs[1])){
      ribbon_dat <- ribbon_dat |>
        filter(year %in% ci_yrs)
    }
    vert_lines_dat <- ribbon_dat |>
      filter(model %in% c(min(model), max(model))) |>
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
    geom_point(size = 3) +
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
    scale_x_continuous(breaks = c(seq_along(cohorts),
                                  length(cohorts) + 1) - 1) +
    scale_y_continuous(breaks = seq(-3, 3)) +
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
