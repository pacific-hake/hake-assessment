% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-density.R
\name{plot_density}
\alias{plot_density}
\title{Create a density plot for a particular parameter}
\usage{
plot_density(model, post_regex, fill = "royalblue", alpha = 0.3)
}
\arguments{
\item{model}{A model object, created by \code{\link[=create_rds_file]{create_rds_file()}}}

\item{post_regex}{A regular expression that matches one parameter name
and nothing else}

\item{fill}{The fill color under the density curve}

\item{alpha}{The alpha value for under the density curve}
}
\value{
A \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} object
}
\description{
Create a density plot for a particular parameter
}
