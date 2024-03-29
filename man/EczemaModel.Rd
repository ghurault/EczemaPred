% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_class.R
\name{EczemaModel}
\alias{EczemaModel}
\title{EczemaModel constructor}
\usage{
EczemaModel(
  model_name = c("BinRW", "OrderedRW", "BinMC", "RW", "Smoothing", "AR1", "MixedAR1",
    "MC"),
  max_score = NULL,
  K = NULL,
  discrete = FALSE,
  prior = NULL
)
}
\arguments{
\item{model_name}{Name of the model to create}

\item{max_score}{Maximum value that the score can take. Required for all models except "MC".}

\item{K}{Number of categories. Only required for "MC" model.}

\item{discrete}{Whether the model is discrete or not. Only required for "RW".}

\item{prior}{Named list of the model's priors.
It uses the default priors (see \code{\link[=default_prior]{default_prior()}}) if \code{NULL} and for the parameters that are not provided.}
}
\value{
An object (list) of class \code{model_name} and EczemaModel, with elements:
\itemize{
\item \code{model_name}: Name of the model
\item \code{stanmodel}: Name of the Stan model.
Used internally to locate the compiled code.
It can also be used to store the Stan code filepath.
\item \code{discrete}: Whether the model is discrete or not.
\item \code{max_score}: Maximum value that the score can take (when applicable)
\item \code{K}: Number of categories (when applicable)
\item \code{prior}: List of parameters' priors
}
}
\description{
EczemaModel constructor
}
\examples{
EczemaModel("BinRW", max_score = 10)
}
