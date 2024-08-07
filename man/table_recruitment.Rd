% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table-recruitment.R
\name{table_recruitment}
\alias{table_recruitment}
\title{Create a table of absolute recruitment and relative spawning biomass
medians and quantiles}
\usage{
table_recruitment(
  model,
  start_yr,
  end_yr,
  digits = 1,
  digits_dev = 3,
  scale = 1000,
  font_size = 8,
  header_font_size = 10,
  header_vert_spacing = 12,
  header_vert_scale = 1.2,
  ...
)
}
\arguments{
\item{model}{A model, created by \code{\link[=create_rds_file]{create_rds_file()}}}

\item{start_yr}{Start year in table}

\item{end_yr}{End year in table}

\item{digits}{Number of decimal places to round recruitment values to}

\item{digits_dev}{Number of decimal places to round recruitment deviation
values to}

\item{scale}{A scale factor to divide the recruitment by}

\item{font_size}{The table data and header font size in points}

\item{header_font_size}{The font size for the headers only. If \code{NULL},
the headers will have the same font size as the table cell data}

\item{header_vert_spacing}{The vertical spacing between newlines for
this font. If \code{NULL} this will be calculated as
\code{header_font_size * header_vert_scale}}

\item{header_vert_scale}{Scale factor to create the vertical spacing value.
See \code{header_vert_spacing}}

\item{...}{Arguments passed to \code{\link[knitr:kable]{knitr::kable()}}}
}
\value{
An \code{\link[knitr:kable]{knitr::kable()}} object
}
\description{
Create a table of absolute recruitment and relative spawning biomass
medians and quantiles
}
