#' Make a time series plot of the management-chosen TAC, assessment-estimated TAC,
#' and realized catch
#'
#' @param d The data as read in using [readr::read_csv()] from the file "catch-targets.csv"
#' @param curr_assess_biomass Current year's assessment-estimated biomass (or any value). If NULL it will not be shown
#' @param connect_vals_linetype Which linetype to use for connecting the values with vertical lines
#' @param connect_vals_color Which color to use for connecting the values with vertical lines
#' @param connect_vals_color Which alpha level (0-1) to use for connecting the values with vertical lines
#' @param connect_vars Logical. Connent the TACs and realized catches to each other
#' @param connect_vars_linetype If `connect_vars` is TRUE, which line type to use
#' @param connect_vars_alpha If `connect_vars` is TRUE, which alpha level (0-1) to use
#'
#' @return A [ggplot2::ggplot()] object
#' @export
#' @examples
#' management_catch_vs_tac_plot(catch.targets, connect_vars = TRUE, connect_vars_linetype = "solid", connect_vars_alpha = 0.2, curr_assess_biomass = base_model$catch.default.policy[1])
management_catch_vs_tac_plot <- function(d,
                                         curr_assess_biomass = NULL,
                                         connect_vals_linetype = "dashed",
                                         connect_vals_color = "darkgrey",
                                         connect_vals_alpha = 0.5,
                                         connect_vars = FALSE,
                                         connect_vars_linetype = "dashed",
                                         connect_vars_alpha = 0.5){

  d <- d %>%
    select(-c(Depletion, `Biomass estimate`))
  if(!is.null(curr_assess_biomass)){
    new_row <- c(max(d$Year) + 1, NA, NA, curr_assess_biomass)
    names(new_row) <- c("Year", "Realized catch", "TAC", "Default HCR TAC")
    d <- bind_rows(d, new_row)
  }
  dd <- melt(d, id.vars = "Year") %>%
    mutate(value = value / 1e3)
  g <- ggplot(dd, aes(x = Year, y = value, color = variable, shape = variable)) +
    geom_point(size = 3) +
    geom_line(aes(group = Year),
              linetype = connect_vals_linetype,
              color = connect_vals_color,
              alpha = connect_vals_alpha) +
    labs(y = "Catch or TAC (1,000 t)") +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle=45, hjust=1)) +
    scale_y_continuous(labels = comma,
                       limits = c(0, NA)) +
    scale_x_continuous(breaks = seq(0, 3000, 1)) +
    guides(shape = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE))

  if(connect_vars){
    g <- g +
      geom_line(aes(group = variable, color = variable),
                linetype = connect_vars_linetype,
                alpha = connect_vars_alpha)
  }
  g
}

#' Plot the TAC vs realized catch on a 1-to-1 plot
#'
#' @param d The data as read in using [readr::read_csv()] from the file "catch-targets.csv"
#' @param top_yrs Year to place labels on top for (remainder will be labels below points)
#' @param xlim See [base::plot()]
#' @param ylim  See [base::plot()]
#' @param color_darken A value between 0 and 1 for darkening of points and text. Smaller values will
#'   make it darker
#' @examples
#' management_catc_vs_tac_1_to_1(catch.targets, top_yrs = c(2004, 2006,2012, 2016, 2018))
management_catch_vs_tac_1_to_1 <- function(d,
                                           top_yrs = NULL,
                                           xlim = c(0, 1000),
                                           ylim = c(0, 800),
                                           color_darken = 0.9){
  oldpar <- par("mfrow", "mar", "mgp", "cex.lab")
  on.exit(par(oldpar))
  # location is bottom unless points overlap too much
  d <- d %>%
    mutate(color = rev(rich.colors.short(n = nrow(d), alpha = 1)))

  d$color <- d$color %>%
    map_chr(function(x){
      tmp <- col2rgb(x) / 255
      rgb(tmp[1] * color_darken,
          tmp[2] * color_darken,
          tmp[3] * color_darken)
    })

  bottom <- d
  if(!is.null(top_yrs[1])){
    top <- d %>%
      filter(Year %in% top_yrs)
    bottom <- d %>%
      filter(!Year %in% top_yrs)
  }

  par(mfrow = c(1, 1),
      mar = c(3.5, 4.6, 1, 1))
  plot(0,
       type = 'n',
       las = 1,
       ylab = "",
       xlab = "",
       xlim = xlim,
       ylim = ylim)

  ## Add points for realized catch
  points(d$`Default HCR TAC` / 1000,
         d$`Realized catch` / 1000,
         pch = 22,
         bg = d$color,
         col = "black",
         cex = 1.1)
  ## Add points for TAC
  points(d$`Default HCR TAC` / 1000,
         d$TAC / 1000,
         pch = 16,
         col = "black",
         cex = 1.1)

  text(x = bottom$`Default HCR TAC` / 1000,
       y = bottom$`Realized catch` / 1000,
       label = substring(bottom$Year, 3),
       pch = 0,
       col = bottom$color,
       cex = 0.7,
       srt = 0,
       adj = c(0.5, 2.0))

  text(x = top$`Default HCR TAC` / 1000,
       y = top$`Realized catch` / 1000,
       label = substring(top$Year, 3),
       pch = 0,
       col = top$color,
       cex = 0.7,
       srt = 0,
       adj = c(1.5, 0.5))

  segments(d$`Default HCR TAC` / 1000,
           d$TAC / 1000,
           d$`Default HCR TAC` / 1000,
           d$`Realized catch` / 1000 + 10,
           lwd = 1.5,
           alpha = 0.6)
  abline(a = 0,
         b = 1,
         col = gray(0.5))
  title(xlab = "Median TAC from harvest rule (thousand t)",
        mgp = c(2.1, 1, 0),
        cex.lab = 1.1)
  title(ylab = "Catch or TAC (1,000 t)",
        mgp = c(3.1, 1, 0),
        cex.lab = 1.1)
  legend("topleft",
         legend = c("TAC implemented by management",
                    "Realized catch"),
         pch = c(16, 0),
         bty = "n",
         cex = 1.2)

}
