% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table-yearly-model-compare.R
\name{table_yearly_model_compare}
\alias{table_yearly_model_compare}
\title{Create a table comparing this year's biomass estimate and uncertainty to
this year's estimate of last year's biomass, and last year's estimate of
last year's biomass}
\usage{
table_yearly_model_compare(
  ly_model,
  cy_model,
  last_yr,
  curr_yr,
  digits = 3,
  font_size = 8
)
}
\arguments{
\item{ly_model}{Last year's model object (typically \code{ly_model})}

\item{cy_model}{This year's model object 9typically \code{base_model})}

\item{last_yr}{Last year (typically \code{last_assess_yr})}

\item{curr_yr}{Current year (typically \code{assess_yr})}

\item{digits}{The number of decimal places to report in the table}

\item{font_size}{Size of the font for cell values}
}
\value{
a \code{\link[kableExtra:kbl]{kableExtra::kbl()}} table
}
\description{
Create a table comparing this year's biomass estimate and uncertainty to
this year's estimate of last year's biomass, and last year's estimate of
last year's biomass
}
