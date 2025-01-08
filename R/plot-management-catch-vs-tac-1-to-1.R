#' Plot the TAC vs realized catch on a 1-to-1 plot
#'
#' @param d The data as read in using [readr::read_csv()] from the file
#' "catch-targets.csv"
#' @param yr_font_size The font size for the year labels
#' @param yrs_nudge_right A vector of years to nudge the labels to the right
#' instead of left for. If `NULL`, none will be modified
#' @param x_lim Limits for the x-axis
#' @param y_lim Limits for the y-axis
#' @param leg_xmin The x-axis value for the left side of the legend
#' @param leg_xmax The x-axis value for the right side of the legend
#' @param leg_ymin The x-axis value for the bottom side of the legend
#' @param leg_ymax The x-axis value for the top side of the legend
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_management_catch_vs_tac_1_to_1 <- function(d,
                                                yr_font_size = 3,
                                                yrs_nudge_right = NULL,
                                                x_lim = c(0, 1000),
                                                y_lim = c(0, 800),
                                                leg_xmin = 20,
                                                leg_xmax = 620,
                                                leg_ymin = 700,
                                                leg_ymax = 780){


  d <- d |>
    mutate(`Realized catch` = `Realized catch` / 1000,
           `Total TAC` = `Total TAC` / 1000,
           `Default HCR TAC` = `Default HCR TAC` / 1000) |>
    dplyr::filter(!is.na(`Default HCR TAC`)) |>
    mutate(Year = factor(Year))

  num_unq_yrs <- d$Year |> unique() |> length()
  cols <- plot_color(num_unq_yrs)
  # Change the yellow ones, as they cannot be seen very well on screen
  cols <- gsub("FFF", "AAA", cols)

  # Make new data frame for right nudged years
  use_nudged_yrs <- FALSE

  if(!is.null(yrs_nudge_right[1])){
    use_nudged_yrs <- TRUE
    d_right <- d |>
      dplyr::filter(Year %in% yrs_nudge_right)
    d_left <- d |>
      dplyr::filter(!Year %in% yrs_nudge_right)
  }

  g <- ggplot(d) +
    geom_abline(linetype = "dotted") +
    geom_point(aes(x = `Default HCR TAC`,
                   y = `Total TAC`),
               size = 3,
               color = "black",
               inherit.aes = FALSE) +
    geom_point(aes(x = `Default HCR TAC`,
                   y = `Realized catch`),
               shape = 15,
               size = 3.75,
               color = "black",
               inherit.aes = FALSE,
               show.legend = FALSE) +
    geom_point(aes(x = `Default HCR TAC`,
                   y = `Realized catch`,
                   group = Year,
                   color = Year),
               shape = 15,
               size = 3,
               inherit.aes = FALSE,
               show.legend = FALSE) +
    geom_segment(aes(x = `Default HCR TAC`,
                     xend = `Default HCR TAC`,
                     y = `Realized catch`,
                     yend = `Total TAC`),
                 color = "black",
                 inherit.aes = FALSE)
  if(use_nudged_yrs){
    g <- g +
      geom_text(data = d_left,
                aes(x = `Default HCR TAC`,
                    y = `Realized catch`,
                    group = Year,
                    color = Year,
                    label = Year),
                size = yr_font_size,
                nudge_x = -30,
                nudge_y = -30,
                inherit.aes = FALSE,
                show.legend = FALSE) +
      geom_text(data = d_right,
                aes(x = `Default HCR TAC`,
                    y = `Realized catch`,
                    group = Year,
                    color = Year,
                    label = Year),
                size = yr_font_size,
                nudge_x = 30,
                nudge_y = -30,
                #check_overlap = TRUE,
                inherit.aes = FALSE,
                show.legend = FALSE)
  }else{
    g <- g +
      geom_text(aes(x = `Default HCR TAC`,
                    y = `Realized catch`,
                    group = Year,
                    color = Year,
                    label = Year),
                size = yr_font_size,
                nudge_x = -30,
                nudge_y = -30,
                #check_overlap = TRUE,
                inherit.aes = FALSE,
                show.legend = FALSE)
  }

  g <- g +
    scale_x_continuous(breaks = seq(x_lim[1], x_lim[2], 200),
                       expand = c(0, 0),
                       limits = x_lim) +
    scale_y_continuous(breaks = seq(y_lim[1], y_lim[2], 200),
                       expand = c(0, 0),
                       limits = y_lim) +
    scale_color_manual(values = cols) +
    xlab("Median TAC from harvest rule (kt)") +
    ylab("Catch or TAC (kt)") +
    # White rectangle acts as legend background
    geom_rect(aes(xmin = leg_xmin,
                  xmax = leg_xmax,
                  ymin = leg_ymin,
                  ymax = leg_ymax),
              fill = "white") +
    annotate("point",
             x = leg_xmin + 10,
             y = leg_ymax - 10,
             shape = 16,
             color = "black",
             size = 3) +
    annotate("point",
             x = leg_xmin + 10,
             y = leg_ymax - 50,
             shape = 0,
             color = "black",
             size = 3) +
    annotate("text",
             x = leg_xmin + 30,
             y = leg_ymax - 10,
             label = "TAC implemented by management",
             hjust = 0) +
    annotate("text",
             x = leg_xmin + 30,
             y = leg_ymax - 50,
             label = "Realized catch",
             hjust = 0)

  g
}
