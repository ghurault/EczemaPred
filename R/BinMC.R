# Documentation -----------------------------------------------------------

#' Binomial Markov Chain model
#'
#' This is a state-space model defined by a Binomial measurement error and a latent Markov Chain.
#'
#' - `default_prior_BinMC` returns the default prior
#' - `fit_BinMC` fits the Binomial Markov chain model
#' - `sample_prior_BinMC` samples the prior predictive distribution
#'
#' @param train Training dataframe (details of the format in [prepare_data_lgtd()])
#' @param test Testing dataframe (details of the format in [prepare_data_lgtd()])
#' @param max_score Maximum value that the score can take
#' @param prior A named list with elements `sigma`, `mu_logit_p10` and `sigma_logit_p10`
#' specifying priors for the corresponding parameters.
#' Each element of the list should be a vector of length 2, containing values for x1 and x2, x2 > 0, such as:
#'
#' - `sigma ~ normal(x1, x2)`
#' - `mu_logit_p10 ~ normal(x1, x2)`
#' - `sigma_logit_p10 ~ normal(x1, x2)`
#' - `logit_tss1_0 ~ normal(x1, x2)`
#'
#' NB: For `sigma` and `sigma_logit_p10`, usually x1=0 to define a half-normal distribution
#' since the parameter is constrained to be positive.
#'
#' @param ... Arguments to be pass to [rstan::sampling()]
#' @param N_patient Number of patients
#' @param t_max Vector of size N_patient indicating the time-series length
#'
#' @return
#' - `default_prior_BinMC`: named list defining the default priors (see details)
#' - `fit_BinMC`: Stanfit object
#' - `sample_prior_BinMC`: Stanfit object
#'
#' @section Model's parameters:
#'
#' Population parameters:
#'
#' - `sigma`: Standard deviation of the evolution of `ss1`
#' - `mu_logit_p10`: Population logit mean of `p10`
#' - `sigma_logit_p10`: Population logit standard deviation of `p10`
#'
#' Patient-dependent parameters:
#'
#' - `p10`: Probability of transitioning from state 1 to state 0
#' - `logit_p10`: logit of `p10`
#' - `logit_tss1_0`: Initial condition of the `logit(ss1 * (1 + p10))`
#'
#' Observation-dependent (patient- and time-dependent) parameters:
#'
#' - `p01`: Probability of transitioning from state 0 to state 1
#' - `lambda`: Mobility of the Markov Chain (eigen value of the transition matrix)
#' - `ss1`: Steady state probability of state 1
#' - `y_lat`: Latent score (probability)
#'
#' See `list_parameters(model = "BinMC")`.
#'
#' @details
#' - The default prior for `sigma` translates to an odd ratio increment of at most 5 (~ 2 * upper bound of prior).
#' - The default priors for `mu_logit_p10` and `sigma_logit_p10` translate to an approximately uniform prior on `p10`.
#' - The prior for the initial condition of `ss1` is hard coded and a function of `p10`.
#' - For more details see the [vignette]().
#'
#' @name BinMC
NULL

# Prior -------------------------------------------------------------------

#' Stop if the prior input is not valid for the Binomial Markov chain model
#'
#' @param prior A named list of corresponding to parameters' prior. See [default_prior_BinMC()].
#'
#' @return NULL if all statements are TRUE, otherwise an error message
#'
#' @seealso [base::stopifnot()]
#'
#' @noRd
stopifnot_prior_BinMC <- function(prior) {
  stopifnot(
    is.list(prior),
    length(prior) == 4,
    all(c("sigma", "mu_logit_p10", "sigma_logit_p10", "logit_tss1_0") %in% names(prior)),
    all(sapply(prior, is.numeric)),
    all(sapply(prior, function(x) {length(x) == 2})),
    all(sapply(prior, function(x) {x[2] > 0}))
  )
}

#' @rdname BinMC
#' @export
default_prior_BinMC <- function() {
  list(
    sigma = c(0, 0.25 * log(5)),
    mu_logit_p10 = c(0, 1),
    sigma_logit_p10 = c(0, 1.5),
    logit_tss1_0 = c(-1, 1)
  )
}

# Fit ---------------------------------------------------------------------

#' @rdname BinMC
#' @export
fit_BinMC <- function(train, test = NULL, max_score, prior = default_prior_BinMC(), ...) {
  fit_discrete(train = train, test = test, max_score = max_score, model = "BinMC", prior = prior, ...)
}

#' @rdname BinMC
#' @export
sample_prior_BinMC <- function(N_patient = 1, t_max = c(2), max_score, prior = default_prior_BinMC(), ...) {
  sample_prior_discrete(N_patient = N_patient, t_max = t_max, max_score = max_score, model = "BinMC", prior = prior, ...)
}
