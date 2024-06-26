% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load-extra-mcmc-biomass.R,
%   R/load-extra-mcmc-catage-biomass.R, R/load-extra-mcmc-sel.R,
%   R/load-extra-mcmc-selwt.R
\name{load_extra_mcmc_biomass}
\alias{load_extra_mcmc_biomass}
\alias{load_extra_mcmc_catage_biomass}
\alias{load_extra_mcmc_sel}
\alias{load_extra_mcmc_selwt}
\title{Load the biomass outputs from the extra MCMC output}
\usage{
load_extra_mcmc_biomass(
  reps,
  verbose = TRUE,
  start_yr = NULL,
  end_yr = NULL,
  progress_n,
  ...
)

load_extra_mcmc_catage_biomass(reps, catage, wtatage, verbose)

load_extra_mcmc_sel(
  reps,
  progress_n,
  verbose = TRUE,
  start_yr = NULL,
  end_yr = NULL,
  type = c("fishery", "survey"),
  ...
)

load_extra_mcmc_selwt(reps, verbose = TRUE, head_beg_pat, head_end_pat, ...)
}
\arguments{
\item{reps}{A list of vectors representing one element per report file}

\item{verbose}{Logical. Show messages}

\item{start_yr}{Filter years earlier than this out of the results. If
\code{NULL}, no filtering will occur for the start year}

\item{end_yr}{End year in the model}

\item{progress_n}{Report every time this many list items are processed.
Consider how many posteriors there are, this should be a fairly large
proportion of that (around 1/8th) or there will be too much output and
it will run really slow}

\item{...}{Absorbs arguments meant for other functions}

\item{catage}{A catch-at-age data frame as created by
\code{\link[=load_extra_mcmc_atage]{load_extra_mcmc_atage()}}}

\item{wtatage}{A weight-at-age data frame}
}
\value{
A list of outputs

A data frame with the medians of the biomass-at-age in catch
}
\description{
Load the biomass outputs from the extra MCMC output

Calculate the biomass-at-age in catch

Load the selectivity outputs from the extra MCMC output

Load the vulnerability outputs from the extra MCMC output
}
