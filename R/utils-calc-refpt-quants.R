#' Calculations for the reference points table
#'
#' @param df The data frame containing the posteriors with the column names
#' being the parameter names and rows being the posteriors
#' @param param The name of the parameter which corresponds to a name in the
#' posterior output data frame (posteriors.sso)
#' @param scale A value to divide the values by before applying the quantiles
#' @param digits The number of decimal points to include in the return values
#' @param perc Logical. If `TRUE`, multiply the values by 100 to give a
#' percentage instead of a proportion. Include a latex-escaped percentage sign
#'
#' @return The quantile vector formatted for the reference points tables
#' @export
calc_refpt_quants <- function(df,
                              param,
                              scale = 1,
                              digits = 0,
                              perc = FALSE){

  vec <- df |>
    select(matches(paste0("^", param,"$"))) |>
    pull()
  vec <- vec / scale
  out <- f(ifelse(perc, 100, 1) * quantile(vec, probs = probs),
           digits)
  if(perc){
    return(paste0(out, "\\%"))
  }

  out
}

