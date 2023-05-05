#' Coerce an MCMC object to a time series
#'
#' @details
#' Copied from the `coda` package, it is the un-exported function
#' `as.ts.mcmc()``
#'
#' @param x An MCMC object [coda::mcmc()]
#' @param ... Unused arguments for compatibility with generic `as.ts`
#'
#' @return A [`stats::ts`] object
as_ts_mcmc <- function(x, ...){

  x <- as.mcmc(x)
  y <- ts(x,
          start = start(x),
          end = end(x),
          deltat = thin(x))
  attr(y, "mcpar") <- NULL

  y
}
