#' Create a plot of forecast value comparisons with the catch level on
#' the x-axis and probability as the y axis value
#'
#' @rdname plot_biomass
#' @param fore_yr The forecast year to use in the plot. Must be in
#' `forecast_yrs`
#' @param colors Colors to use for the lines in the plot
#' @param shapes Shapes to use for the points in the plot. Must be the same
#' length as `colors`
#' @param remove_x_val A vector of values to remove labels for on the x axis.
#' Plot first leaving this as `NULL`, then add values to this that overlap
#' others
#' @param show_50_line Logical. If `TRUE`, draw a horizontal line at 50\\%
#' @param short Logical. If `TRUE`, draw only 3 lines (decline, under B40,
#' under B10)
#'
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
                              x_expansion = 40,
                              leg_font_size = 8,
                              leg_pos = NULL,
                              leg_ncol = 1,
                              remove_x_val = NULL,
                              show_50_line = TRUE,
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
    stop("The table does not have 7 columns. This is required for ",
         "this function")
  }

  ct_colname <- paste0("Catch in ", fore_yr, " (kt)")
  ct_sym <- sym(ct_colname)
  names(prob_dat) <- c(
    ct_colname,
    paste0("P(B", fore_yr + 1, "<B", fore_yr, "): Stock declines in ",
           fore_yr + 1),
    paste0("P(B", fore_yr + 1, "<B40%)"),
    paste0("P(B", fore_yr + 1, "<B25%)"),
    paste0("P(B", fore_yr + 1, "<B10%)"),
    paste0("P(", fore_yr, " relative fishing intensity > 100%)"),
    paste0("P(", fore_yr + 1, " default harvest policy catch < ",
           fore_yr, " catch)"))

  if(short){
    pat_decline <- paste0("B", fore_yr + 1, "<B", fore_yr)
    wch_decline_col <- grep(pat_decline, names(prob_dat))
    if(!length(wch_decline_col)){
      stop("No column name contains `", pat_decline, "`")
    }
    pat_40 <- paste0("B", fore_yr + 1, "<B40%")
    wch_40_col <- grep(pat_40, names(prob_dat))
    if(!length(wch_40_col)){
      stop("No column name contains `", pat_40, "`")
    }
    pat_10 <- paste0("B", fore_yr + 1, "<B10%")
    wch_10_col <- grep(pat_10, names(prob_dat))
    if(!length(wch_40_col)){
      stop("No column name contains `", pat_10, "`")
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
  x_labels <- x_breaks
  if(!is.null(remove_x_val[1])){
    x_labels[x_breaks %in% remove_x_val] <- ""
  }

  g <- ggplot(df,
              aes(x = !!ct_sym,
                  y = value,
                  group = Probability,
                  color = Probability,
                  shape = Probability)) +
    geom_point(size = 3) +
    geom_line(linetype = "dashed",
              linewidth = 0.5) +
    scale_color_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    labs(y = "Probability") +
    coord_cartesian(ylim = c(0, 1)) +
    scale_x_continuous(expand = c(0, x_expansion),
                       breaks = x_breaks,
                       labels = x_labels) +
    theme(legend.title = element_blank(),
          legend.text.align = 0,
          axis.text.x = element_text(angle = 90,
                                     # Need vjust to move the tick labels
                                     # to the right due to angle being 90
                                     # causing them to be offset
                                     vjust = 0.35))

  if(show_50_line){
    g <- g +
      geom_hline(yintercept = 0.5,
                 linetype = "dotted",
                 color = "black")
  }
  if(is.null(leg_pos[1]) || is.na(leg_pos[1])){
    g <- g +
      theme(legend.position = "none")
  }else if(leg_pos[1] != "out"){
    g <- g +
      theme(legend.position = leg_pos,
            #legend.box.background = element_rect(color = "white"),
            legend.text = element_text(size = leg_font_size)) +
      guides(fill = guide_legend(ncol = leg_ncol,
                                 label.hjust = 0),
             color = guide_legend(ncol = leg_ncol,
                                  label.hjust = 0))
  }else{
    g <- g +
      theme(legend.text = element_text(size = leg_font_size))
  }

  g
}
