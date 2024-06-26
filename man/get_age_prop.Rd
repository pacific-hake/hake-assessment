% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-get-age-prop.R
\name{get_age_prop}
\alias{get_age_prop}
\title{Create the age prop and the age itself for the ranking of age proportions}
\usage{
get_age_prop(vec, ranking = 1)
}
\arguments{
\item{vec}{A vector of age proportions}

\item{ranking}{1 = max, 2 = second highest, etc.}
}
\value{
The age proportion and the age itself for the ranking of age
proportion
}
\description{
Create the age prop and the age itself for the ranking of age proportions
}
\details{
Think of the question "Which is the second-highest number in
this vector and what is its index in the vector?" This function returns
a vector of those two numbers.
}
