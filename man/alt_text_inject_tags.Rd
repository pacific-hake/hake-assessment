% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-alt-text-inject-tags.R
\name{alt_text_inject_tags}
\alias{alt_text_inject_tags}
\title{Inject the \code{tagpdf} code around the figure chunk given by the knitr
chunk \code{label}}
\usage{
alt_text_inject_tags(x, label, alt_text)
}
\arguments{
\item{x}{Tex code, as a vector of lines read in from a TeX file by
\code{\link[=readLines]{readLines()}}}

\item{label}{The label used in the knitr chunk}

\item{alt_text}{The alternative text to inject}
}
\value{
The modified Tex code, as a vector
}
\description{
Inject the \code{tagpdf} code around the figure chunk given by the knitr
chunk \code{label}
}
