% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table-decision.R
\name{table_decision}
\alias{table_decision}
\title{Create the Executive Summary decision tables. \code{rows_to_label} and
\code{row_labels} need changing each year.}
\usage{
table_decision(
  model,
  type = c("biomass", "spr"),
  forecast_inds = seq_along(model$forecasts[[length(model$forecasts)]]),
  rows_to_show = NULL,
  letter_df = tribble(~let, ~row1_text, ~row2_text, "a", "", "", "b", "", "", "c", "",
    "", "d", "10\\\\\% reduction", "each year", "e", paste0(model$endyr, " catch"), "",
    "f", "", "", "g", "10\\\\\% reduction", "each year", "h", "", "", "i",
    "10\\\\\% reduction", "each year", "j", "", "", "k", paste0(model$endyr - 1, " TAC"),
    "", "l", paste0(model$endyr, " TAC"), "", "m", "Fishing intensity", "at 100\\\\\%",
    "n", "Default HR", paste0("(", fspr_40_10_for_latex_table, ")"), "o", "Equal catch",
    paste0("($\\\\mathrm{C}_{", model$endyr + 
     1, "} \\\\approxeq \\\\mathrm{C}",
    "_{", model$endyr + 2, "}$)")),
  bold_letters = TRUE,
  digits = 2,
  left_col_cm = 1,
  right_cols_cm = 1,
  font_size = 10,
  header_font_size = 10,
  header_vert_spacing = 12,
  header_vert_scale = 1.2,
  ...
)
}
\arguments{
\item{model}{A model, created by \code{\link[=create_rds_file]{create_rds_file()}}}

\item{type}{One of \code{biomass} or \code{spr}}

\item{forecast_inds}{The indices to use in the \code{forecasts} object which
itself is an object of the \code{model} list}

\item{rows_to_show}{A vector of letters of rows to show. If \code{NULL}, show
all rows}

\item{letter_df}{A data frame with three columns called \code{let}, \code{row1_text},
and \code{row2_text} where \code{let} contains the letter shown in the table for
each row, \code{row1_text} is the first line of text directly beneath the
letter, and \code{row2_text} is the second line of text directly below the
first line of text. \code{row1_text} and \code{row2_text} can be empty strings, in
that case no text will be shown in the cell, only the letter}

\item{bold_letters}{Logical. If \code{TRUE}, the letters labelling each row
will be boldface}

\item{digits}{The number of decimal points to show in the table}

\item{left_col_cm}{Number of centimeters wide to make the leftmost column.
This needs to be changed when the font size is changed}

\item{right_cols_cm}{Number of centimeters wide to make the 3 rightmost
columns wide. This needs to be changed when the font size is changed}

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
An \code{\link[knitr:kable]{knitr::kable()}} object
}
\description{
Create the Executive Summary decision tables. \code{rows_to_label} and
\code{row_labels} need changing each year.
}
