#' Make a bar plot of the catchability parameter for a survey
#'
#' @param model The model to plot bars for
#' @param model2 A model to draw a median line for
#' @param type Either "age2plus" or "age1" for those two surveys
#' @param hist_color Bar color
#' @param hist_alpha Bar transparency
#' @param med_color Median line color
#' @param model2_med_color Model 2 median line color
#' @param model2_mle_color Model 2 median line transparency
#' @export
make.mcmc.catchability.plot <- function(model,
                                        model2 = NULL,
                                        type = "age2plus",
                                        hist_color = "grey60",
                                        hist_alpha = 0.5,
                                        med_color = "royalblue",
                                        #mle_color = "royalblue",
                                        model2_med_color = "red",
                                        model2_mle_color = "red"){
  hist_color <- get.shade(hist_color, (1 - hist_alpha) * 100)
  par(mar = c(3, 3, 1, 1))
  if(type == "age2plus"){
    vec <- model$extra_mcmc$Q_vector
    if(!is.null(model2)){
      vec2 <- model2$extra_mcmc$Q_vector
    }
  }else if(type == "age1"){
    vec <- model$extra_mcmc$Q_vector_age1
    if(!is.null(model2)){
      vec2 <- model2$extra_mcmc$Q_vector_age1
    }
  }else{
    stop("type must be either 'age2plus' or 'age1'")
  }
  hist(vec,
       breaks = seq(0, 1.1 * max(vec), 0.05),
       xlab = "Acoustic survey catchability (Q)",
       col = hist_color,
       border = hist_color,
       xaxs = 'i',
       yaxs = 'i',
       main = "")
  abline(v = median(vec),
         col = med_color,
         lwd = 2,
         lty = 1)
  #abline(v = model$cpue$Calc_Q[1],
  #       col = mle_color,
  #       lwd = 2,
  #       lty = 2)
  if(!is.null(model2)){
    abline(v = median(vec2),
           col = model2_med_color,
           lwd = 2,
           lty = 1)
    #abline(v = model2$cpue$Calc_Q[1],
    #       col = model2_mle_color,
    #       lwd = 2,
    #       lty = 2)
  }
  abline(h = 0)
}
