% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table-above-avg-cohorts-yrs.R
\name{table_above_avg_cohorts_yrs}
\alias{table_above_avg_cohorts_yrs}
\title{Create a table containing some years' estimated recruitment values
for specific cohorts}
\usage{
table_above_avg_cohorts_yrs(
  cohorts = large_cohorts,
  yrs = c(last_assess_yr, assess_yr)
)
}
\arguments{
\item{cohorts}{A vector of years, representing the cohorts to use in the
table}

\item{yrs}{A vector of assessment years to include in the table}
}
\value{
A \code{\link[kableExtra:kbl]{kableExtra::kbl()}} table
}
\description{
Create a table containing some years' estimated recruitment values
for specific cohorts
}
\details{
This is a table typically used in the assessment presentation, so that
recruitment estimates of string cohorts
}
