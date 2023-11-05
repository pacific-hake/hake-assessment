#' Create a plot of the maturity ogives used in the assessment. The plot
#' shows two of them; above and below the line if latitude at Point
#' Conception (34.44° latitude)
#'
#' @param model A model, created by [create_rds_file()]
#' @param d  The data frame which is read in from
#' @param colors A vector of two colors for North and South of 34 degrees
#' respectively
#' @param point_alpha Transparency of the bubbles
#' @param line_alpha Transparency of the lines
#' `inst/extdata/data/hake-maturity-data.csv`
#' @param alpha The transparency for all ribbons
#' @param leg_pos The position of the legend inside the plot. If `NULL`,
#' `NA`, or `none`, the legend will not be shown
#' @param leg_ncol The number of columns to show in the legend
#' @param leg_font_size The legend font size
#' @param ret_df Logical. If `TRUE`, don't plot, instead return the
#' calculation of `d` which may be needed as part of the fecundity calculation
#' elsewhere
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_maturity_ogives <- function(model,
                                 d,
                                 colors =c("blue", "red"),
                                 point_alpha = 0.3,
                                 line_alpha = 0.3,
                                 leg_pos = c(0.85, 0.15),
                                 leg_ncol = 1,
                                 leg_font_size = 16,
                                 ret_df = FALSE){

  age_max <- max(model$agebins)

  d <- d |>
    filter(!is.na(age)) |>
    # Put older fish in the plus group for maturity (age 15+)
    mutate(age = ifelse(age >= age_max, age_max, age))

  # Split up the data frame by the column `n_or_s_of_34_44` which is if the
  # sample if north or south of that latitude, then split each of those two
  # data frames into N data frames, one for each unique age and store the
  # mean of the `functional_maturity`, which are all 1's or 0's, so the mean
  # will be the total number of 1's, or mature fish in the sample for each
  # age. These are re-spliced back into a single data frame (using the two
  # `map_df` calls). The new columns `y` and `n_samp` are present in the new
  # data frame
  d <- d |>
    split(~n_or_s_of_34_44) |>
    map_df(\(n_or_s){
      n_or_s |>
        split(~age) |>
        map_df(\(ag){
          # %>% needed here instead of |> because the next line uses the
          # dot (`.`) reference for the current data frame
          ag %>%
            mutate(frac_mature = mean(functional_maturity),
                   num_samp = nrow(.))
        })
    }) |>
    transmute(area = n_or_s_of_34_44,
              age,
              frac_mature,
              num_samp)

  if(ret_df){
    return(d)
  }
  # Extract the first row from each of the above categories for the
  #  maturity plot
  mat_d <- d |>
    split(~area) |>
    map_df(\(n_or_s){
      n_or_s |>
        split(~age) |>
        map_df(\(ag){
          ag |>
            slice(1)
        })
    }) |>
    # There are missing area/age combos. This inserts `NA` for those so
    # that ggplot will not connect the line from one valid age across two
    # ages to the next valid one
    complete(area, age) |>
    split(~area) |>
    # Move text labels for number of samples above or below the circles
    # depending on if they are north or south and less or greater than 70
    imap(\(ar, ind){
      if(ind == "S"){
        ar <- ar |>
          mutate(text_place = ifelse(num_samp < 70,
                                     frac_mature + 0.05,
                                     frac_mature))

      }else if(ind == "N"){
        ar <- ar |>
          mutate(text_place = ifelse(num_samp < 70,
                                     frac_mature - 0.05,
                                     frac_mature))
      }
      ar
    }) |>
    map_df(~{.x}) |>
    mutate(area = ifelse(area == "N",
                         "North of 34.44°",
                         "South of 34.44°"))

  # Inflate to plot to the max age in the model (not the max age of the
  # maturity samples which may be less than that
  age_max_model <- model$wtatage |>
    names() %>%
    grep("^\\d", ., value = TRUE) |>
    as.numeric() |>
    max()
  if(age_max_model > age_max){
    num_yrs_to_add <- age_max_model - age_max
    end_rows <- mat_d |>
      split(~area) |>
      map_df(\(ar){
        ar %>%
          slice((nrow(.) - num_yrs_to_add + 1):nrow(.)) |>
          mutate(frac_mature = last(frac_mature)) |>
          mutate_at(vars(-area, -frac_mature), ~{NA}) |>
          mutate(age = seq(age_max + 1, age_max_model))
      })

    mat_d <- mat_d |>
      bind_rows(end_rows)
  }

  d <- d |>
    mutate(area = factor(area, levels = unique(area)))

  x_breaks <- seq_len(age_max)
  x_labels <- x_breaks
  x_labels[length(x_labels)] <- paste0(x_labels[length(x_labels)], "+")
  y_breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0)

  g <- ggplot(mat_d,
              aes(x = age,
                  y = frac_mature,
                  group = area,
                  color = area,
                  fill = area,
                  size = num_samp)) +
    geom_hline(yintercept = y_breaks,
               linewidth = 0.5,
               alpha = 0.5,
               linetype = "dashed") +
    geom_line(linewidth = 1, alpha = line_alpha) +
    geom_point(shape = 21, color = "black", alpha = point_alpha) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    scale_size(range = c(0, 20)) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels) +
    scale_y_continuous(breaks = y_breaks) +
    geom_text(aes(x = age,
                  y = text_place,
                  label = num_samp,
                  color = area),
              inherit.aes = FALSE,
              show.legend = FALSE,
              size = 5) +
    guides(size = "none",
           color = "none",
           fill = guide_legend("")) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = leg_font_size)) +
    labs(x = "Age",
         y = "Proportion mature")

  if(is.null(leg_pos[1]) || is.na(leg_pos[1])){
    g <- g +
      theme(legend.position = "none")
  }else{
    g <- g +
      theme(legend.position = leg_pos) +
      guides(fill = guide_legend(ncol = leg_ncol,
                                 override.aes = list(size = 5)),
             color = guide_legend(ncol = leg_ncol))
  }
  g
}
