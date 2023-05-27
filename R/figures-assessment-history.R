#' Make an interquartile dispersion plot fir historic assessments
#'
#' @param base This year's base model
#' @param assessment.history.disp A data frame read in from the assessment history SSBdispersion file
#'
#' @return A base R plot
#' @export
make.assessment.history.disp.plot <- function(base,
                                              assessment.history.disp){

  xx <- assess_history_disp_df

  par(mfrow = c(1, 2),
      mar=c(4, 4, 1, 1),
      oma = c(1, 1, 0, 0))

  plot(xx$InterQuartileRange~xx$Assessment_Year,
       type = "o",
       ylim = c(0, 2000),
       ylab = "Interquartile Range of Spawning Biomass ('000s)",
       xlab = "Assessment Year",
       cex.lab = 0.85,
       col = "black",
       cex = 0.85,
       lwd = 2,
       las = 1)

  plot(xx$QuartileCoeffDispersion~xx$Assessment_Year,
       type = "o",
       ylim = c(0, 0.6),
       ylab = "Quartile Coefficient of Dispersion",
       xlab = "Assessment Year",
       cex.lab = 0.85,
       col = "black",
       cex = 0.85,
       lwd = 2,
       las = 1)
}