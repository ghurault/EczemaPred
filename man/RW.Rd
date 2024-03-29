% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_doc.R
\name{RW}
\alias{RW}
\title{Random walk model}
\arguments{
\item{max_score}{Maximum value that the score can take}

\item{discrete}{Whether to use a discrete normal distribution.
This will be used to check whether the data is discrete or not, and for rounding predictions (cf. testing).}

\item{prior}{Named list of the model's priors. If \code{NULL}, uses the default prior for the model (see \code{\link[=default_prior]{default_prior()}}).}
}
\description{
Random walk model
}
\details{
\itemize{
\item Details of the model are available in the \href{#}{paper}.
\item The model takes as input a continuous score defined between 0 and \code{max_score}.
\item The model is naive as the likelihood is non-truncated and not discretised (when \code{discrete = TRUE}).
As a result, sampling from the prior predictive distribution can be challenging if the score is near the bounds
and the variance is sufficiently large.
\item For more details see the \href{https://ghurault.github.io/EczemaPred/articles/ContinuousModels.html}{vignette}.
}
}
\section{Parameters}{

\itemize{
\item \code{sigma}: Standard deviation of the random walk
\item \code{y_mis}: Missing values
}

See \code{list_parameters(model = "RW")} for more details.
}

\section{Priors}{

The priors are passed as a named list with element \code{sigma}
specifying priors for the corresponding parameter, where
\verb{sigma / max_score ~ normal+(x1, x2)}
and the element \code{sigma} of the list is a vector of length two containing x1 and x2.
NB: usually x1=0 to define a half-normal distribution (sigma is constraint to be positive) and
x2 should be positive.
}

\section{Default priors}{

The default prior for \code{sigma} translates to a width of the predictive distribution to be at most \code{max_score}.
}

\examples{
EczemaModel("RW", max_score = 100, discrete = FALSE)
}
