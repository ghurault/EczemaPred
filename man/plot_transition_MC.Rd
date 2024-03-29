% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_fit.R
\name{plot_transition_MC}
\alias{plot_transition_MC}
\title{Markov Chain expected transition matrix}
\usage{
plot_transition_MC(fit, max_scale = NA, show_text = FALSE)
}
\arguments{
\item{fit}{Stanfit object corresponding to the Markov Chain model}

\item{max_scale}{Maximum value that the legend display.
If NA, this chosen automatically.}

\item{show_text}{Whether to display the probability as text in the heatmap.}
}
\value{
Ggplot
}
\description{
Markov Chain expected transition matrix
}
