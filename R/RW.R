# Documentation -----------------------------------------------------------

#' Random walk model
#'
#' - `default_prior_RW` returns the default prior
#' - `fit_RW` fits the random walk model
#' - `sample_prior_RW` samples the prior predictive distribution
#'
#' @param train Training dataframe (details of the format in [prepare_data_lgtd()])
#' @param test Testing dataframe (details of the format in [prepare_data_lgtd()])
#' @param max_score Maximum value that the score can take
#' @param discrete Whether to use a discrete normal distribution (only relevant for testing)
#' @param prior A named list of corresponding to parameters' prior.
#' Priors are defined for the parameter `sigma`, where
#' `sigma / max_score ~ normal(x1, x2)`
#' and the element `sigma` of the list is a vector of length two containing x1 and x2.
#' NB: usually x1=0 to define a half-normal distribution (sigma is constraint to be positive) and
#' x2 should be positive.
#' @param ... Arguments to be pass to [rstan::sampling()]
#' @param N_patient Number of patients
#' @param t_max Vector of size N_patient indicating the time-series length
#'
#' @return
#' - `default_prior_RW`: named list defining a default prior for `sigma` (see details)
#' - `fit_RW`: Stanfit object
#' - `sample_prior_RW`: Stanfit object
#'
#' @section Model's parameters:
#'
#' - `sigma`: Standard deviation of the random walk
#' - `y_mis`: Missing values
#'
#' See `list_parameters(model = "RW")`.
#'
#' @details
#' - The default prior translates to a width of the predictive distribution to be at most `max_score`.
#' - The model is naive as it is trained with a non-truncated, not discretised distribution.
#' As a result, sampling from the prior predictive distribution can be challenging if the score is near the bounds
#' and the variance is sufficiently large.
#' - For more details see the [vignette]().
#'
#' @name RW
NULL

## Update url

# Prior ---------------------------------------------------------------

#' Stop if the prior input is not valid for the Random Walk model
#'
#' @param prior A named list of corresponding to parameters' prior. See [default_prior_RW()].
#'
#' @return NULL if all statements are TRUE, otherwise an error message
#'
#' @seealso [base::stopifnot()]
#'
#' @noRd
stopifnot_prior_RW <- function(prior) {
  stopifnot(is.list(prior),
            length(prior) == 1,
            "sigma" %in% names(prior),
            all(is.numeric(prior$sigma)),
            length(prior$sigma) == 2,
            prior$sigma[2] > 0)
}

#' @rdname RW
#' @export
default_prior_RW <- function() {
  list(sigma = c(0, 0.1))
}

# Fit ---------------------------------------------------------------------

#' @rdname RW
#' @export
fit_RW <- function(train, test = NULL, max_score, discrete = FALSE, prior = default_prior_RW(), ...) {
  fit_all(train = train, test = test, max_score = max_score, discrete = discrete, model = "RW", prior = prior, ...)
}

#' @rdname RW
#' @export
sample_prior_RW <- function(N_patient = 1, t_max = c(2), max_score, discrete = FALSE, prior = default_prior_RW(), ...) {
  sample_prior_all(N_patient = N_patient, t_max = t_max, max_score = max_score, discrete = discrete, model = "RW", prior = prior, ...)
}
