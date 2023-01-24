#' Title
#'
#' @param model A model, created by [create_rds_file()] that has a
#' `forecasts` object (forecasts have been run)
#' @param forecast_yrs A vector of forecast years to use
#' @param fore_yr Forecast year for the probabilities
#' @param colors A vector of colors, one for each probability to be plotted
#' @param shapes A vector of point shapes, one for each probability to be
#' plotted
#' @param leg_font_size The legend font size
#' @param leg_pos The position of the legend inside the plot. If `NULL`,
#' `NA`, or `none`, the legend will not be shown
#' @param remove_x_val A cevtor of values to remove from the x-axis due to
#' overlapping. First run this function, then select the values and pass them
#' to the function when plotting a second time
#' @param short Logical. If `TRUE`, plot a version with only P(YR<0.4B0),
#' P(YR<0.1B0), and P(YR<YR+1)
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_fore_compare <- function(model,
                              forecast_yrs,
                              fore_yr,
                              colors = c("black",
                                         "blue",
                                         "green",
                                         "orange",
                                         "red",
                                         "tan"),
                              shapes = c(16, 17, 17, 17, 15, 18),
                              leg_font_size = 8,
                              leg_pos = NULL,
                              remove_x_val = NULL,
                              short = FALSE){

  prob_dat <- model$risks[fore_yr == forecast_yrs][[1]] |>
    as_tibble()


  # Sort the table by catches
  forecatch_col <- paste0("ForeCatch_", fore_yr)
  forecatch_col_sym <- sym(forecatch_col)
  prob_dat <- prob_dat[order(prob_dat |> pull(forecatch_col)), ]
  # Divide all the percentages by 100 to get probabilities
  prob_dat <- prob_dat |>
    mutate_at(vars(-forecatch_col), ~{.x / 100}) |>
    mutate(!!forecatch_col_sym := !!forecatch_col_sym / 1000)

  # Remove MSY columns, which are DFO reference points
  wch_msy <- grep("MSY", names(prob_dat))
  if(length(wch_msy)){
    prob_dat <- prob_dat |>
      select(-all_of(wch_msy))
  }

  if(ncol(prob_dat) != 7){
    stop("The table does not have 7 columns. This is required for this function",
         call. = FALSE)
  }

  ct_colname <- paste0("Catch in ", fore_yr, " (1,000 t)")
  ct_sym <- sym(ct_colname)
  names(prob_dat) <- c(
    ct_colname,
    paste0("P(B", fore_yr + 1, "<B", fore_yr, "): Stock declines in ", fore_yr + 1),
    paste0("P(B", fore_yr + 1, "<B40%)"),
    paste0("P(B", fore_yr + 1, "<B25%)"),
    paste0("P(B", fore_yr + 1, "<B10%)"),
    paste0("P(", fore_yr, " relative fishing intensity > 100%)"),
    paste0("P(", fore_yr + 1, " default harvest policy catch < ", fore_yr, " catch)"))

  if(short){
    pat_decline <- paste0("B", fore_yr + 1, "<B", fore_yr)
    wch_decline_col <- grep(pat_decline, names(prob_dat))
    if(!length(wch_decline_col)){
      stop("No column name contains `", pat_decline, "`",
           call. = FALSE)
    }
    pat_40 <- paste0("B", fore_yr + 1, "<B40%")
    wch_40_col <- grep(pat_40, names(prob_dat))
    if(!length(wch_40_col)){
      stop("No column name contains `", pat_40, "`",
           call. = FALSE)
    }
    pat_10 <- paste0("B", fore_yr + 1, "<B10%")
    wch_10_col <- grep(pat_10, names(prob_dat))
    if(!length(wch_40_col)){
      stop("No column name contains `", pat_10, "`",
           call. = FALSE)
    }
    prob_dat <- prob_dat |>
      select(!!ct_sym,
             wch_decline_col,
             wch_40_col,
             wch_10_col)

  }

  lvls <- names(prob_dat)[-1]

  df <- prob_dat |>
    pivot_longer(cols = -ct_colname,
                 names_to = "Probability",
                 values_to = "value") |>
    mutate(Probability = factor(Probability, levels = lvls),
           !!ct_sym := round(!!ct_sym, 0))

  x_breaks <- df |> pull(!!ct_colname) |> unique()
  if(!is.null(remove_x_val[1])){
    x_breaks <- x_breaks[!x_breaks %in% remove_x_val]
  }

  g <- ggplot(df,
              aes(x = !!ct_sym,
                  y = value,
                  group = Probability,
                  color = Probability,
                  shape = Probability)) +
    geom_point(size = 3) +
    geom_line(linetype = "dashed", linewidth = 1) +
    scale_color_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    labs(y = "Probability") +
    coord_cartesian(ylim = c(0, 1)) +
    scale_x_continuous(expand = c(0, 2),
                       breaks = x_breaks,
                       labels = x_breaks) +
    theme(legend.title=element_blank(),
          axis.text.x = element_text(color = "grey20",
                                     size = 10,
                                     angle = 90,
                                     hjust = 1,
                                     vjust = 0.5,
                                     face = "plain"),
          axis.text.y = element_text(color = "grey20",
                                     size = 10,
                                     hjust = 1,
                                     vjust = 0.5,
                                     face = "plain"),
          axis.title.x = element_text(color = "grey20",
                                      size = 14,
                                      angle = 0,
                                      face = "plain"),
          axis.title.y = element_text(color = "grey20",
                                      size = 14,
                                      angle = 90,
                                      face = "plain"))

  if(is.null(leg_pos[1]) || is.na(leg_pos[1])){
    g <- g +
      theme(legend.position = "none")
  }else if(leg_pos[1] != "out"){
    g <- g +
      theme(legend.position = leg_pos,
            legend.text = element_text(size = leg_font_size)) +
      guides(fill = guide_legend(ncol = leg_ncol),
             color = guide_legend(ncol = leg_ncol))
  }else{
    g <- g +
      theme(legend.text = element_text(size = leg_font_size))
  }

  g
}
