# Documentation -----------------------------------------------------------

#' Exponential smoothing model
#'
#' - `default_prior_Smoothing` returns the default prior
#' - `fit_Smoothing` fits the exponential smoothing model
#' - `sample_prior_Smoothing` samples the prior predictive distribution
#'
#' @param train Training dataframe (details of the format in [prepare_data_lgtd()])
#' @param test Testing dataframe (details of the format in [prepare_data_lgtd()])
#' @param max_score Maximum value that the score can take
#' @param prior A named list with elements `sigma` and `tau` specifying priors for the corresponding parameters:
#'
#' - `sigma / max_score ~ normal(x1, x2)`.
#' The element `sigma` of the list should be a vector of length 2 containing x1 and x2.
#' NB: usually x1=0 to define a half-normal distribution (sigma is constraint to be positive) and
#' x2 should be positive.
#' - `tau ~ lognormal(x1, x2)`.
#' The element `tau` of the list should be a vector of length 2 containing x1 and x2.
#' x2 should be positive.
#'
#' @param ... Arguments to be pass to [rstan::sampling()]
#' @param N_patient Number of patients
#' @param t_max Vector of size N_patient indicating the time-series length
#'
#' @return
#' - `default_prior_Smoothing`: named list defining the default priors (see details)
#' - `fit_Smoothing`: Stanfit object
#' - `sample_prior_Smoothing`: Stanfit object
#'
#' @section Model's parameters:
#'
#' - `sigma`: Standard deviation of the random walk
#' - `alpha`: Smoothing factor
#' - `tau`: Time constant associated with the smoothing factor
#' - `y_mis`: Missing values
#'
#' See `list_parameters(model = "Smoothing")`.
#'
#' @details
#' - The default prior for `sigma` translates to a width of the predictive distribution to be at most `max_score`.
#' - The default prior for `tau` assumes it could range from less a 1 to 100 (time units).
#' - The model is naive as it is trained with a non-truncated
#' - For more details see the [vignette](https://ghurault.github.io/EczemaPred/articles/ContinuousModels.html).
#'
#' @name Smoothing
NULL

# Prior -------------------------------------------------------------------

#' Stop if the prior input is not valid for the exponential smoothing model
#'
#' @param prior A named list of corresponding to parameters' prior. See [default_prior_Smoothing()].
#'
#' @return NULL if all statements are TRUE, otherwise an error message
#'
#' @seealso [base::stopifnot()]
#'
#' @noRd
stopifnot_prior_Smoothing <- function(prior) {
  stopifnot(
    is.list(prior),
    length(prior) == 2,
    all(c("sigma", "tau") %in% names(prior)),
    all(sapply(prior, is.numeric)),
    all(sapply(prior, function(x) {length(x) == 2})),
    prior$sigma[2] > 0,
    prior$tau[2] > 0
  )
}

#' @rdname Smoothing
#' @export
default_prior_Smoothing <- function() {
  list(
    sigma = c(0, 0.1),
    tau = c(0.5 * log(10), 0.75 * log(10))
  )
}

# Fit ---------------------------------------------------------------------

#' @rdname Smoothing
#' @export
fit_Smoothing <- function(train, test = NULL, max_score, prior = default_prior_Smoothing(), ...) {
  fit_continuous(train = train, test = test, max_score = max_score, model = "Smoothing", prior = prior, ...)
}

#' @rdname Smoothing
#' @export
sample_prior_Smoothing <- function(N_patient = 1, t_max = c(2), max_score, prior = default_prior_Smoothing(), ...) {
  sample_prior_continuous(N_patient = N_patient, t_max = t_max, max_score = max_score, model = "Smoothing", prior = prior, ...)
}
