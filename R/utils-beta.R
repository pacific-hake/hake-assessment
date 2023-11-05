#' The Beta distribution
#'
#' @details
#' Random generation for the Beta distribution with parameters based on the
#' mean and standard deviation as well as the ranges as implemented in
#' CASAL. This formulation is also available on [Wikipedia](
#' https://en.wikipedia.org/wiki/Beta_distribution#Mean_and_variance).
#' This function is a direct copy of the code available from [Wetzel and Punt](
#' https://github.com/chantelwetzel-noaa/XSSS/blob/4dce917ee06d52416b58dd9440796ba1f7357943/R/rbeta_ab_fn.R).
#'
#' @param prior Prior value
#' @param sd Standard deviation
#' @param min Minimum value
#' @param max Maximum value
#' @param n The number of random values you want in the returned vector.
#'
#' @return A vector of doubles, where the length of the vector depends on the
#' length of `x` value passed to `n`.
#' `dbeta_ab` returns density estimates for each input value and `rbeta_ab`
#' provides random deviates from the Beta distribution.
#'
#' @export
rbeta_ab <- function(n, prior, sd, min, max) {
  # CASAL's Beta
  mu <- (prior - min) / (max - min)
  tau <- (prior - min) * (max - prior) / (sd ^ 2) - 1.0
  alpha <- tau * mu
  beta <- tau * (1 - mu)
  b_std <- rbeta(n, alpha, beta)

  # linear transformation from beta(0, 1) to beta(a, b)
  (max - min) * b_std + min
}

#' @param x X value for distribution
#' @rdname rbeta_ab
#' @export
dbeta_ab <- function(x, prior, sd, min, max) {

  p_const <- 0.0001
  mu <- (prior - min) / (max - min) # CASAL's v
  tau <- (prior - min) * (max - prior) / (sd^2) - 1.0
  b_prior <- tau * mu
  # CASAL's m and n
  a_prior <- tau * (1 - mu)
  if(b_prior <= 1.0 | a_prior <= 1.0){
    warning("Bad Beta prior")
  }

  (1.0 - b_prior) * log(p_const + x - min) +
    (1.0 - a_prior) * log(p_const + max - x) -
    (1.0 - b_prior) * log(p_const + prior - min) -
    (1.0 - a_prior) * log(p_const + max - prior)
}


#' Calculate the two shape parameters from the mean (`mu`) and `cv`
#' which is what is in the SS files.
#'
#' @details
#' To make a plot of the Beta function based on values from SS3 which are
#' labelled `prior` and `pr_sd` in the control file, do this:
#' `s1 <- beta_get_shape_params(prior, pr_sd)[1]`
#' `s2 <- beta_get_shape_params(prior, pr_sd)[2]`
#' `ggplot() +`
#' `geom_function(fun = stats::dbeta,`
#'               `args = list(shape1 = s1, shape2 = s2, log = FALSE))`
#'
#' @param mu The mean of the Beta distribution
#' @param cv The CV of the Beta distribution
#'
#' @return A vector of two values, the two shape parameters, also know as
#' `shape1` and `shape2` by the `stats::dbeta()` function
#' @export
beta_get_shape_params <- function(mu, cv) {
  sd <- mu * cv
  alpha <- ((1 - mu) / sd ^ 2 - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  c(alpha = alpha, beta = beta)
}

