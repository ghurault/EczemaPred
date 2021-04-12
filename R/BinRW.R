# Documentation -----------------------------------------------------------

#' Binomial random walk model
#'
#' This is a state-space model defined by a Binomial measurement error and a latent random walk.
#'
#' - `default_prior_BinRW` returns the default prior
#' - `fit_BinRW` fits the Binomial random walk model
#' - `sample_prior_BinRW` samples the prior predictive distribution
#'
#' @param train Training dataframe (details of the format in [prepare_data_lgtd()])
#' @param test Testing dataframe (details of the format in [prepare_data_lgtd()])
#' @param max_score Maximum value that the score can take
#' @param prior A named list with elements `sigma`, `mu_logit_y0` and `sigma_logit_y0`
#' specifying priors for the corresponding parameters.
#' Each element of the list should be a vector of length 2, containing values for x1 and x2, x2 > 0, such as:
#'
#' - `sigma ~ normal(x1, x2)`.
#' - `mu_logit_y0 ~ normal(x1, x2)`
#' - `sigma_logit_y0 ~ normal(x1, x2)`
#'
#' NB: For `sigma` and `sigma_logit_y0`, usually x1=0 to define a half-normal distribution
#' since the parameters are constrained to be positive.
#'
#' @param ... Arguments to be pass to [rstan::sampling()]
#' @param N_patient Number of patients
#' @param t_max Vector of size N_patient indicating the time-series length
#'
#' @return
#' - `default_prior_BinRW`: named list defining the default priors
#' - `fit_BinRW`: Stanfit object
#' - `sample_prior_BinRW`: Stanfit object
#'
#' @section Model's parameters:
#'
#' Population parameters:
#'
#' - `sigma`: Standard deviation of the random walk (in logit scale)
#' - `mu_logit_y0`: Population mean of the initial condition (in logit scale)
#' - `sigma_logit_y0`: Population standard deviation of the initial condition (logit scale)
#'
#' Patient-dependent parameters:
#'
#' - `logit_y0`: Logit of the initial condition
#'
#' Observation-dependent (patient- and time-dependent) parameters:
#'
#' - `y_lat`: Latent score (probability)
#' - `logit_lat`: logit of `y_lat`
#'
#' See `list_parameters(model = "BinRW")`.
#'
#' @details
#' - The default prior for `sigma` translates to an odd ratio increment of at most 5 (~ 2 * upper bound of prior).
#' - The default priors for `mu_logit_y0` and `sigma_logit_y0` translates to an approximately uniform prior on `y0`.
#' - For more details see the [vignette]().
#'
#' @name BinRW
NULL

# Prior ---------------------------------------------------------------

#' Stop if the prior input is not valid for the Binomial random Walk model
#'
#' @param prior A named list of corresponding to parameters' prior. See [default_prior_BinRW()].
#'
#' @return NULL if all statements are TRUE, otherwise an error message
#'
#' @seealso [base::stopifnot()]
#'
#' @noRd
stopifnot_prior_BinRW <- function(prior) {
  stopifnot(
    is.list(prior),
    length(prior) == 3,
    all(c("sigma", "mu_logit_y0", "sigma_logit_y0") %in% names(prior)),
    all(sapply(prior, is.numeric)),
    all(sapply(prior, function(x) {length(x) == 2})),
    all(sapply(prior, function(x) {x[2] > 0}))
  )
}

#' @rdname BinRW
#' @export
default_prior_BinRW <- function() {
  list(
    sigma = c(0, 0.25 * log(5)),
    mu_logit_y0 = c(0, 1),
    sigma_logit_y0 = c(0, 1.5)
  )
}

# Fit ---------------------------------------------------------------------

#' @rdname BinRW
#' @export
fit_BinRW <- function(train, test = NULL, max_score, prior = default_prior_BinRW(), ...) {
  fit_discrete(train = train, test = test, max_score = max_score, model = "BinRW", prior = prior, ...)
}

#' @rdname BinRW
#' @export
sample_prior_BinRW <- function(N_patient = 1, t_max = c(2), max_score, prior = default_prior_BinRW(), ...) {
  sample_prior_discrete(N_patient = N_patient, t_max = t_max, max_score = max_score, model = "BinRW", prior = prior, ...)
}
