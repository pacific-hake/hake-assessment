% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-fit-lw.R
\name{fit_lw}
\alias{fit_lw}
\title{Fit a length-weight model}
\usage{
fit_lw(d, tol = 0.1, maxiter = 1000)
}
\arguments{
\item{d}{Data frame containing the columns \code{length} and \code{weight}}

\item{tol}{Model tolerance. See \code{\link[minpack.lm:nls.lm.control]{minpack.lm::nls.lm.control()}}}

\item{maxiter}{Model maximum iterations. See \code{\link[minpack.lm:nls.lm.control]{minpack.lm::nls.lm.control()}}}
}
\value{
The \code{\link[stats:coef]{stats::coefficients()}} for the model fit
}
\description{
Fit a length-weight model
}
\details{
Uses a modified version of \code{\link[stats:nls]{stats::nls()}} that uses \code{\link[minpack.lm:nls.lm]{minpack.lm::nls.lm()}}
for fitting. See \code{\link[minpack.lm:nlsLM]{minpack.lm::nlsLM()}} for details.
}
