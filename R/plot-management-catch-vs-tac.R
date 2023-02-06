#' Make a time series plot of the management-chosen TAC, assessment
#' estimated TAC, and realized catch
#'
#' @param d The data as read in using [readr::read_csv()] from the file
#' "catch-targets.csv"
#' @param curr_assess_biomass Current year's assessment-estimated biomass
#' (or any value). If `NULL` it will not be shown
#' @param connect_vals_linetype Which linetype to use for connecting the
#' values with vertical lines
#' @param connect_vals_color Which color to use for connecting the values
#' with vertical lines
#' @param connect_vals_color Which alpha level (0-1) to use for connecting
#' the values with vertical lines
#' @param connect_vars Logical. Connent the TACs and realized catches to
#' each other
#' @param connect_vars_linetype If `connect_vars` is TRUE, which line type
#' to use
#' @param connect_vars_alpha If `connect_vars` is TRUE, which alpha level
#' (0-1) to use
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_management_catch_vs_tac <- function(d,
                                         curr_assess_biomass = NULL,
                                         connect_vals_linetype = "dashed",
                                         connect_vals_color = "darkgrey",
                                         connect_vals_alpha = 0.5,
                                         connect_vars = FALSE,
                                         connect_vars_linetype = "dashed",
                                         connect_vars_alpha = 0.5,
                                         leg_pos = c(0.65, 0.83),
                                         leg_ncol = 1,
                                         leg_font_size = 12){

  d <- d |>
    select(-c(Depletion, `Biomass estimate`))
  if(!is.null(curr_assess_biomass)){
    new_row <- c(max(d$Year) + 1, NA, NA, curr_assess_biomass)
    names(new_row) <- c("Year", "Realized catch", "TAC", "Default HCR TAC")
    d <- bind_rows(d, new_row)
  }

  dd <- melt(d, id.vars = "Year") |>
    mutate(value = value / 1e3)

  g <- ggplot(dd, aes(x = Year, y = value, color = variable, shape = variable)) +
    geom_point(size = 3) +
    geom_line(aes(group = Year),
              linetype = connect_vals_linetype,
              color = connect_vals_color,
              alpha = connect_vals_alpha) +
    labs(y = "Catch or TAC (1,000 t)") +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.text = element_text(size = leg_font_size)) +
    scale_y_continuous(labels = comma,
                       limits = c(0, NA)) +
    scale_x_continuous(breaks = seq(0, 3000, 1))

  if(connect_vars){
    g <- g +
      geom_line(aes(group = variable, color = variable),
                linetype = connect_vars_linetype,
                alpha = connect_vars_alpha)
  }

  if(is.null(leg_pos[1]) || is.na(leg_pos[1])){
    g <- g +
      theme(legend.position = "none")
  }else{
    g <- g +
      theme(legend.position = leg_pos) +
      guides(fill = guide_legend(ncol = leg_ncol),
             color = guide_legend(ncol = leg_ncol,
                                  reverse = TRUE,
                                  label.hjust = 0),
             shape = guide_legend(reverse = TRUE,
                                  label.hjust = 0))
  }

  g
}
