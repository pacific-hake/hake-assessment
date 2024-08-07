% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-calc-refpt-quants.R
\name{calc_refpt_quants}
\alias{calc_refpt_quants}
\title{Calculations for the reference points table}
\usage{
calc_refpt_quants(df, param, scale = 1, digits = 0, perc = FALSE)
}
\arguments{
\item{df}{The data frame containing the posteriors with the column names
being the parameter names and rows being the posteriors}

\item{param}{The name of the parameter which corresponds to a name in the
posterior output data frame (posteriors.sso)}

\item{scale}{A value to divide the values by before applying the quantiles}

\item{digits}{The number of decimal points to include in the return values}

\item{perc}{Logical. If \code{TRUE}, multiply the values by 100 to give a
percentage instead of a proportion. Include a latex-escaped percentage sign}
}
\value{
The quantile vector formatted for the reference points tables
}
\description{
Calculations for the reference points table
}
