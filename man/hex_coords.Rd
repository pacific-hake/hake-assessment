% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-hexcoords.R
\name{hex_coords}
\alias{hex_coords}
\title{A cleaned-up copy of hexbins::hexcoords()}
\usage{
hex_coords(dx, dy = NULL, n = 1, sep = NULL)
}
\arguments{
\item{dx}{vertical width of the hexagon(s)}

\item{n}{number of hexagon “repeats”}

\item{sep}{separator value to be put between coordinates of different
hexagons. The default, \code{NULL} doesn't use a separator}
}
\value{
A vector of six coordinates
}
\description{
A cleaned-up copy of hexbins::hexcoords()
}
