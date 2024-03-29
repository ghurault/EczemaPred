% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_class.R, R/model_inference.R
\name{EczemaFit}
\alias{EczemaFit}
\alias{EczemaFit.EczemaModel}
\title{Fit an EczemaModel}
\usage{
EczemaFit(model, train, test, ...)

\method{EczemaFit}{EczemaModel}(model, train, test = NULL, ...)
}
\arguments{
\item{model}{Object}

\item{train}{Training dataframe (see details below)}

\item{test}{Testing dataframe (see details below)}

\item{...}{Arguments to pass to \code{\link[rstan:stanmodel-method-sampling]{rstan::sampling()}}}
}
\value{
Stanfit object
}
\description{
Fit an EczemaModel
}
\section{Data format}{

\subsection{All models except "MC"}{
\itemize{
\item \code{train} and \code{test} should have the columns \code{Patient} (patient ID), \code{Time} (timepoint) and \code{Score} (score to model).
\item \code{Patient} should take integer values between 1 and the number of patients in the training set.
\item \code{Time} should take integer (so discrete) values and starts with one for every patient.
\item \code{Score} should take values between 0 and max_score.
\item Missing values are not allowed (but Time values are not necessarily consecutive,
for example if Score at t=5 is missing, but not at t=4 and t=6, just remove t=5).
}
}

\subsection{"MC" model}{
\itemize{
\item \code{train} and \code{test} should have columns \code{y0} (for the current state), \code{y1} (for the next state) and
\code{dt} (for the time delay between states).
\item \code{y0} and \code{y1} should take integer values between 1 and K.
\item \code{dt} should take integer values greater than or equal to 1.
\item Missing values are not allowed.
}
}
}

\examples{
\dontrun{
model <- EczemaModel("BinRW", max_score = 100)
train <- data.frame(Patient = 1, Time = 1:10, Score = rbinom(10, 100, .5))
EczemaFit(model, train)
}
}
