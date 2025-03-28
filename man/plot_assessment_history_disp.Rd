% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-assessment-history-disp.R
\name{plot_assessment_history_disp}
\alias{plot_assessment_history_disp}
\title{Create Interquartile range or quartile coefficient of dispersion plots
for the assessment history spawning biomass estimates}
\usage{
plot_assessment_history_disp(
  model,
  type = c("inter", "coeff"),
  y_lim = c(0, 2000),
  x_breaks = seq(start_yr, end_yr, 2),
  point_shape = ts_pointshape,
  point_size = ts_pointsize,
  point_color = ts_single_model_pointcolor,
  line_width = ts_linewidth,
  line_color = ts_single_line_color,
  line_type = ts_single_model_linetype,
  ...
)
}
\arguments{
\item{model}{A model object, created by \code{\link[=create_rds_file]{create_rds_file()}}}

\item{type}{One of 'inter' or 'coeff' for Inter-quartile range or
Quartile coefficient of dispersion respectively}

\item{x_breaks}{A vector of the values to show on the x-axis. The defaults
\code{start_yr} and \code{end_yr} are assigned in the first few lines of the
function to be the limits of the data}

\item{point_shape}{A numeric point shape code}

\item{point_size}{Size of the points}

\item{point_color}{Color of the points}

\item{line_width}{Width of the line}

\item{line_color}{Color of the line}

\item{line_type}{Type of the line, e.g. "dashed", "solid", "dotted"}

\item{...}{Arguments to pass to \code{calc_retro_stats()} (namely, \code{yr})}

\item{ylim}{A vector of two representing the minimum and maximum values to
show on the y-axis}
}
\value{
a \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} object
}
\description{
Create Interquartile range or quartile coefficient of dispersion plots
for the assessment history spawning biomass estimates
}
\details{
Note that if you expect this function to work correctly outside of the
hake repository directories (e.g. in a testing directory you got to by
running \code{gotest()}), you must supply the full-path filename for the
'assessment-history-dispersion.csv' file using the arg \code{fn} to this
function. It will be passed via \code{...} to \code{calc_retro_stats()}.
}
