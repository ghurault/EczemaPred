# Documentation -----------------------------------------------------------

#' Mixed effect autoregressive model (order 1)
#'
#' - `default_prior_MixedAR1` returns the default prior
#' - `fit_MixedAR1` fits the mixed AR1 model
#' - `sample_prior_MixedAR1` samples the prior predictive distribution
#'
#' @param train Training dataframe (details of the format in [prepare_data_lgtd()])
#' @param test Testing dataframe (details of the format in [prepare_data_lgtd()])
#' @param max_score Maximum value that the score can take
#' @param prior A named list with elements `sigma`, `mu_logit_alpha`, `sigma_logit_alpha`, `mu_inf`, `sigma_inf`
#' specifying priors for the corresponding parameters.
#' Each element of the list should be a vector of length 2, containing values for x1 and x2, x2 > 0, such as:
#'
#' - `sigma / max_score ~ normal(x1, x2)`.
#' - `mu_logit_alpha ~ normal(x1, x2)`.
#' - `sigma_logit_alpha ~ normal(x1, x2)`.
#' - `mu_inf / max_score ~ normal(x1, x2)`.
#' - `sigma_inf / max_score ~ normal(x1, x2)`.
#'
#' NB: For `sigma`, `sigma_logit_alpha` and `sigma_inf`, usually x1=0 to define a half-normal distribution
#' since the parameter is constrained to be positive.
#'
#' @param ... Arguments to be pass to [rstan::sampling()]
#' @param N_patient Number of patients
#' @param t_max Vector of size N_patient indicating the time-series length
#'
#' @return
#' - `default_prior_MixedAR1`: named list defining the default priors (see details).
#' - `fit_MixedAR1`: Stanfit object
#' - `sample_prior_MixedAR1`: Stanfit object
#'
#' @section Model's parameters:
#'
#' Population parameters:
#'
#' - `sigma`: Standard deviation of the autoregression
#' - `mu_logit_alpha`: Population mean of the logit of `alpha`
#' - `sigma_logit_alpha`: Population standard deviation of the logit of `alpha`
#' - `mu_inf`: Population mean of `y_inf`
#' - `sigma_inf`: Population standard deviation of `y_inf`
#'
#' Patient-dependent parameters:
#'
#' - `alpha`: Autocorrelation parameter
#' - `y_inf`: Autoregression mean
#' - `b`: Intercept
#'
#' Other parameters:
#'
#' - `y_mis`: Missing values
#'
#' See `list_parameters(model = "MixedAR1")`.
#'
#' @details
#' - The default prior for `sigma` translates to a width of the predictive distribution to be at most `max_score`.
#' - The default priors for `mu_logit_alpha` and `sigma_logit_alpha` have "reasonable" ranges and
#' translate to a prior on `alpha` that is approximately uniform.
#' - The default prior for `mu_inf` spans the entire range of the score.
#' - The default prior for `sigma_inf` translates to a range in the distribution of `y_inf` to be at most `max_score`.
#' - The model is naive as it is trained with a non-truncated.
#' - For more details see the [vignette]().
#'
#' @name MixedAR1
NULL

# Prior -------------------------------------------------------------------

#' Stop if the prior input is not valid for the mixed effect AR1 model
#'
#' @param prior A named list of corresponding to parameters' prior. See [default_prior_MixedAR1()].
#'
#' @return NULL if all statements are TRUE, otherwise an error message
#'
#' @seealso [base::stopifnot()]
#'
#' @noRd
stopifnot_prior_MixedAR1 <- function(prior) {
  stopifnot(
    is.list(prior),
    length(prior) == 5,
    all(c("sigma", "mu_logit_alpha", "sigma_logit_alpha",
          "mu_inf", "sigma_inf") %in% names(prior)),
    all(sapply(prior, is.numeric)),
    all(sapply(prior, function(x) {length(x) == 2})),
    all(sapply(prior, function(x) {x[2] > 0})))
}

#' @rdname MixedAR1
#' @export
default_prior_MixedAR1 <- function() {
  list(
    sigma = c(0, 0.1),
    mu_logit_alpha = c(0, 1),
    sigma_logit_alpha = c(0, 1.5),
    mu_inf = c(0.5, 0.25),
    sigma_inf = c(0, 0.125)
  )
}

# Fit ---------------------------------------------------------------------

#' @rdname MixedAR1
#' @export
fit_MixedAR1 <- function(train, test = NULL, max_score, prior = default_prior_MixedAR1(), ...) {
  fit_continuous(train = train, test = test, max_score = max_score, model = "MixedAR1", prior = prior, ...)
}

#' @rdname MixedAR1
#' @export
sample_prior_MixedAR1 <- function(N_patient = 1, t_max = c(2), max_score, prior = default_prior_MixedAR1(), ...) {
  sample_prior_continuous(N_patient = N_patient, t_max = t_max, max_score = max_score, model = "MixedAR1", prior = prior, ...)
}
