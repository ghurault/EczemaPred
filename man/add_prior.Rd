% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{add_prior}
\alias{add_prior}
\title{Add prior to the list serving as input to the Stan sampler}
\usage{
add_prior(data_stan, prior, prefix = "prior_")
}
\arguments{
\item{data_stan}{List}

\item{prior}{A named list of corresponding to parameters' prior}

\item{prefix}{Prefix to add to the names of prior}
}
\value{
data_stan with additional items corresponding to the prior.
}
\description{
Used internally
}
