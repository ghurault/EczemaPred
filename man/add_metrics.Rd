% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{add_metrics}
\alias{add_metrics}
\alias{add_metrics1_d}
\alias{add_metrics1_c}
\alias{add_metrics2_d}
\alias{add_metrics2_c}
\title{Append lpd and (C)RPS to (test) dataframe}
\usage{
add_metrics1_d(df, fit)

add_metrics1_c(df, fit)

add_metrics2_d(df, support, add_samples = support)

add_metrics2_c(df, add_samples = NULL, bw = NULL)
}
\arguments{
\item{df}{Dataframe to add the metrics to
\itemize{
\item For \code{\link[=add_metrics1_c]{add_metrics1_c()}}, it must contain a column "Score".
\item For \code{\link[=add_metrics2_c]{add_metrics2_c()}} and \code{\link[=add_metrics2_d]{add_metrics2_d()}}, it must contain the columns "Samples" and "Score".
}}

\item{fit}{Stanfit object with parameters "lpd", and for \code{\link[=add_metrics1_d]{add_metrics1_d()}} "cum_err".}

\item{support}{Support of the distribution}

\item{add_samples}{Numeric vector used to initialise the distribution when computing the lpd and (C)RPS.
For example, this can be used to add a uniform distribution to the vector of samples,
to avoid problems at the tail of the distribution.
If \code{NULL}, the empirical pmf is not changed.
Default to the uniform distribution (i.e. \code{support}) for \code{\link[=add_metrics2_d]{add_metrics2_d()}} and \code{NULL} for \code{\link[=add_metrics2_c]{add_metrics2_c()}}.
The column "Samples" is not modified when \code{add_samples} is not NULL.}

\item{bw}{Bandwidth, for calculating lpd, see \code{\link[scoringRules:scores_sample_univ]{scoringRules::logs_sample()}}.
Useful to set the "resolution" of the distribution.}
}
\value{
Dataframe \code{df} appended by the columns "lpd", "RPS"
(or "CRPS" for \code{\link[=add_metrics1_c]{add_metrics1_c()}} and \code{\link[=add_metrics2_d]{add_metrics2_d()}}).
}
\description{
\itemize{
\item \code{\link[=add_metrics1_d]{add_metrics1_d()}} and \code{\link[=add_metrics1_c]{add_metrics1_c()}} extracts the lpd and RPS from the Stanfit object
\item \code{\link[=add_metrics2_d]{add_metrics2_d()}} and \code{\link[=add_metrics2_c]{add_metrics2_c()}} calculates the lpd and (C)RPS from the empirical pmf
\item The metrics in \code{\link[=add_metrics2_c]{add_metrics2_c()}} and the CRPS of \code{\link[=add_metrics1_c]{add_metrics1_c()}} are
calculated using the \code{scoringRules} package.
}
}
\seealso{
\code{\link[=extract_lpd]{extract_lpd()}}, \code{\link[=extract_RPS]{extract_RPS()}}, \code{\link[HuraultMisc:compute_RPS]{HuraultMisc::compute_RPS()}},
\code{\link[scoringRules:scores_sample_univ]{scoringRules::logs_sample()}}, \code{\link[scoringRules:scores_sample_univ]{scoringRules::crps_sample()}}.
}
