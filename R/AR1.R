# Documentation -----------------------------------------------------------

#' Autoregressive model (order 1)
#'
#' - `default_prior_AR1` returns the default prior
#' - `fit_AR1` fits the AR1 model
#' - `sample_prior_AR1` samples the prior predictive distribution
#'
#' @param train Training dataframe (details of the format in [prepare_data_lgtd()])
#' @param test Testing dataframe (details of the format in [prepare_data_lgtd()])
#' @param max_score Maximum value that the score can take
#' @param prior A named list with elements `sigma`, `y_inf`, `alpha` specifying priors
#' for the corresponding parameters, such as:
#'
#' - `sigma / max_score ~ normal(x1, x2)`.
#' The element `sigma` of the list should be a vector of length 2 containing x1 and x2.
#' NB: usually x1=0 to define a half-normal distribution (sigma is constraint to be positive) and
#' x2 should be positive.
#' - `y_inf / max_score ~ N(x1, x2)`.
#' The element `y_inf` of the list should be a vector of length 2 containing x1 and x2, x2 should be positive.
#' - `alpha ~ beta(x1, x2)`.
#' The element `alpha` of the list should be a vector of length 2 containing x1 and x2, both positives.
#'
#' @param ... Arguments to be pass to [rstan::sampling()]
#' @param N_patient Number of patients
#' @param t_max Vector of size N_patient indicating the time-series length
#'
#' @return
#' - `default_prior_AR1`: named list defining the default priors (see details)
#' - `fit_AR1`: Stanfit object
#' - `sample_prior_AR1`: Stanfit object
#'
#' @section Model's parameters:
#'
#' - `sigma`: Standard deviation of the autoregression
#' - `alpha`: Autocorrelation parameter
#' - `b`: Intercept
#' - `y_inf`: Autoregression mean
#' - `y_mis`: Missing values
#'
#' See `list_parameters(model = "AR1")`.
#'
#' @details
#' - The default prior for `sigma` translates to a width of the predictive distribution to be at most `max_score`.
#' - The default prior for `y_inf` covers the full range of the score.
#' - The default prior for `alpha` is uniform in 0-1.
#' - The model is naive as it is trained with a non-truncated distribution
#' - For more details see the [vignette](https://ghurault.github.io/EczemaPred/articles/ContinuousModels.html).
#'
#' @name AR1
NULL

# Prior -------------------------------------------------------------------

#' Stop if the prior input is not valid for the AR1 model
#'
#' @param prior A named list of corresponding to parameters' prior. See [default_prior_AR1()].
#'
#' @return NULL if all statements are TRUE, otherwise an error message
#'
#' @seealso [base::stopifnot()]
#'
#' @noRd
stopifnot_prior_AR1 <- function(prior) {
  stopifnot(
    is.list(prior),
    length(prior) == 3,
    all(c("sigma", "alpha", "y_inf") %in% names(prior)),
    all(sapply(prior, is.numeric)),
    all(sapply(prior, function(x) {length(x) == 2})),
    prior$sigma[2] > 0,
    prior$y_inf[2] > 0,
    all(prior$alpha > 0)
  )
}

#' @rdname AR1
#' @export
default_prior_AR1 <- function() {
  list(
    sigma = c(0, 0.1),
    alpha = c(1, 1),
    y_inf = c(0.5, 0.25)
  )
}

# Fit ---------------------------------------------------------------------

#' @rdname AR1
#' @export
fit_AR1 <- function(train, test = NULL, max_score, prior = default_prior_AR1(), ...) {
  fit_continuous(train = train, test = test, max_score = max_score, model = "AR1", prior = prior, ...)
}

#' @rdname AR1
#' @export
sample_prior_AR1 <- function(N_patient = 1, t_max = c(2), max_score, prior = default_prior_AR1(), ...) {
  sample_prior_continuous(N_patient = N_patient, t_max = t_max, max_score = max_score, model = "AR1", prior = prior, ...)
}
