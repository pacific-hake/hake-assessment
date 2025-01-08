#' Fit a length-weight model
#'
#' @details
#' Uses a modified version of [stats::nls()] that uses [minpack.lm::nls.lm()]
#' for fitting. See [minpack.lm::nlsLM()] for details.
#'
#' @param d Data frame containing the columns `length` and `weight`
#' @param tol Model tolerance. See [minpack.lm::nls.lm.control()]
#' @param maxiter Model maximum iterations. See [minpack.lm::nls.lm.control()]
#'
#' @return The [stats::coefficients()] for the model fit
fit_lw <- function(d,
                   tol = 0.1,
                   maxiter = 1e3){

  d <- d |>
    dplyr::filter(!is.na(length), !is.na(weight))

  if(!nrow(d)){
    return(c(NA, NA))
  }
  w <- d$weight
  l <- d$length

  fit <- nlsLM(w ~ lw_alpha * l ^ lw_beta,
               start = c(lw_alpha = 0.5, lw_beta = 2.0),
               control = list(tol = tol, maxiter = maxiter))

  coefficients(fit)
}
