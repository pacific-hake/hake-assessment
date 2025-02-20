% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table-survey-history.R
\name{table_survey_history}
\alias{table_survey_history}
\title{Create a table of the acoustic survey history}
\usage{
table_survey_history(
  model,
  d,
  vessel_sep_lines = FALSE,
  digits = 3,
  font_size = 10,
  header_font_size = 10,
  header_vert_spacing = 12,
  header_vert_scale = 1.2,
  ...
)
}
\arguments{
\item{model}{A model, created by \code{\link[=create_rds_file]{create_rds_file()}}}

\item{d}{A data frame containing the survey history as read in from
the file \code{survey-history.csv}}

\item{vessel_sep_lines}{Logical. If \code{TRUE}, place a line under the
vessels in each cell as a way to separate them vertically from the
years above and below}

\item{digits}{Number of decimal places}

\item{font_size}{The table data and header font size in points}

\item{header_font_size}{The font size for the headers only. If \code{NULL},
the headers will have the same font size as the table cell data}

\item{header_vert_scale}{Scale factor to create the vertical spacing value.
See \code{header_vert_spacing}}

\item{...}{Arguments passed to \code{\link[knitr:kable]{knitr::kable()}}}

\item{vert_spacing}{The vertical spacing between newlines for this font.
If \code{NULL} this will be calculated as \code{header_font_size * header_vert_scale}}
}
\value{
A \code{\link[knitr:kable]{knitr::kable()}} object
}
\description{
Create a table of the acoustic survey history
}
\details{
The vessel names need to be fixed. They are seperated by spaces, and may
or may not have dashes in their names. The dashes will be replaced with
spaces, and the spaces will be replaced by newlines in the output
}
