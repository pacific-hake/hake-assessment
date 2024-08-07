% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post-process-convert-section-headers.R
\name{post_process_convert_section_headers}
\alias{post_process_convert_section_headers}
\title{Replace chapter with section for all sections, and prepend a "sub" to
each section, subsection etc to lower each by one level}
\usage{
post_process_convert_section_headers(x, ...)
}
\arguments{
\item{x}{Tex code, as a vector of lines read in from a TeX file by
\code{\link[=readLines]{readLines()}}}

\item{...}{Absorbs arguments meant for other functions}
}
\value{
The modified Tex code, as a vector
}
\description{
Replace chapter with section for all sections, and prepend a "sub" to
each section, subsection etc to lower each by one level
}
