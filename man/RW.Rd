% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RW.R
\name{RW}
\alias{RW}
\alias{default_prior_RW}
\alias{fit_RW}
\alias{sample_prior_RW}
\title{Random walk model}
\usage{
default_prior_RW()

fit_RW(
  train,
  test = NULL,
  max_score,
  discrete = FALSE,
  prior = default_prior_RW(),
  ...
)

sample_prior_RW(
  N_patient = 1,
  t_max = c(2),
  max_score,
  discrete = FALSE,
  prior = default_prior_RW(),
  ...
)
}
\arguments{
\item{train}{Training dataframe (details of the format in \code{\link[=prepare_data_lgtd]{prepare_data_lgtd()}})}

\item{test}{Testing dataframe (details of the format in \code{\link[=prepare_data_lgtd]{prepare_data_lgtd()}})}

\item{max_score}{Maximum value that the score can take}

\item{discrete}{Whether to use a discrete normal distribution (only relevant for testing)}

\item{prior}{A named list of corresponding to parameters' prior.
Priors are defined for the parameter \code{sigma}, where
\code{sigma / max_score ~ normal(x1, x2)}
and the element \code{sigma} of the list is a vector of length two containing x1 and x2.
NB: usually x1=0 to define a half-normal distribution (sigma is constraint to be positive) and
x2 should be positive.}

\item{...}{Arguments to be pass to \code{\link[rstan:stanmodel-method-sampling]{rstan::sampling()}}}

\item{N_patient}{Number of patients}

\item{t_max}{Vector of size N_patient indicating the time-series length}
}
\value{
\itemize{
\item \code{default_prior_RW}: named list defining a default prior for \code{sigma} (see details)
\item \code{fit_RW}: Stanfit object
\item \code{sample_prior_RW}: Stanfit object
}
}
\description{
\itemize{
\item \code{default_prior_RW} returns the default prior
\item \code{fit_RW} fits the random walk model
\item \code{sample_prior_RW} samples the prior predictive distribution
}
}
\details{
\itemize{
\item The default prior translates to a width of the predictive distribution to be at most \code{max_score}.
\item The model is naive as it is trained with a non-truncated, not discretised distribution.
As a result, sampling from the prior predictive distribution can be challenging if the score is near the bounds
and the variance is sufficiently large.
\item For more details see the \url{vignette}.
}
}
\section{Model's parameters}{

\itemize{
\item \code{sigma}: Standard deviation of the random walk
\item \code{y_mis}: Missing values
}

See \code{list_parameters(model = "RW")}.
}
