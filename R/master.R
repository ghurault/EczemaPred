# Master functions to call different models (except MC for fitting or sampling prior)

# Helpers -----------------------------------------------------------------

#' Call default_prior_* for different models
#'
#' @param model Model name
#' @param max_score Maximum value that the score can take (only required for model = "OrderedRW")
#'
#' @return List (see default_prior_*)
#' @export
#'
#' @examples
#' default_prior(model = "BinRW")
default_prior <- function(model = c("BinRW", "BinMC", "OrderedRW", "RW", "Smoothing", "AR1", "MixedAR1"),
                          max_score = NULL) {

  model <- match.arg(model)

  if (model == "BinRW") {
    out <- default_prior_BinRW()
  } else if (model == "BinMC") {
    out <- default_prior_BinMC()
  } else if (model == "OrderedRW") {
    stopifnot(is_scalar_wholenumber(max_score))
    out <- default_prior_OrderedRW(max_score)
  } else if (model == "RW") {
    out <- default_prior_RW()
  } else if (model == "Smoothing") {
    out <- default_prior_Smoothing()
  } else if (model == "AR1") {
    out <- default_prior_AR1()
  } else if (model == "MixedAR1") {
    out <- default_prior_MixedAR1()
  }

  return(out)

}

#' Call stopifnot_prior_* for different models
#'
#' @param prior Named list (see stopifnot_prior_*)
#' @param model Model name
#' @param max_score Maximum value that the score can take
#'
#' @return NULL if all statements are TRUE, otherwise an error message
#' @noRd
stopifnot_prior <- function(prior, model = c("BinRW", "BinMC", "OrderedRW", "RW", "Smoothing", "AR1", "MixedAR1"), max_score) {
  model <- match.arg(model)

  if (model == "BinRW") {
    stopifnot_prior_BinRW(prior)
  } else if (model == "BinMC") {
    stopifnot_prior_BinMC(prior)
  } else if (model == "OrderedRW") {
    stopifnot_prior_OrderedRW(prior, max_score)
  } else if (model == "RW") {
    stopifnot_prior_RW(prior)
  } else if (model == "Smoothing") {
    stopifnot_prior_Smoothing(prior)
  } else if (model == "AR1") {
    stopifnot_prior_AR1(prior)
  } else if (model == "MixedAR1") {
    stopifnot_prior_MixedAR1(prior)
  }

}

# Fit models --------------------------------------------------------------

#' Fit different models
#'
#' @param train Training dataframe (details of the format in [prepare_data_lgtd()])
#' @param test Testing dataframe (details of the format in [prepare_data_lgtd()])
#' @param max_score Maximum value that the score can take
#' @param discrete Whether the model is discrete
#' @param model Model name
#' @param prior A named list of corresponding to parameters' prior.
#' @param ... Arguments to be pass to [rstan::sampling()]
#'
#' @return Stanfit object
#' @noRd
fit_all <- function(train,
                    test = NULL,
                    max_score,
                    discrete = FALSE,
                    model = c("BinRW", "BinMC", "OrderedRW", "RW", "Smoothing", "AR1", "MixedAR1"),
                    prior = default_prior(model, max_score),
                    ...) {

  data_stan <- prepare_data_lgtd(train = train, test = test, max_score = max_score, discrete = discrete)

  stopifnot_prior(prior, model = model, max_score = max_score)
  data_stan <- add_prior(data_stan, prior)

  data_stan$discrete <- as.numeric(discrete) # only useful for RW
  data_stan$run <- 1 # only useful for state-space models

  fit <- rstan::sampling(stanmodels[[model]], data = data_stan, ...)

  return(fit)

}

#' Fit discrete models
#'
#' Except Markov Chain model
#'
#' @param train Training dataframe (details of the format in [prepare_data_lgtd()])
#' @param test Testing dataframe (details of the format in [prepare_data_lgtd()])
#' @param max_score Maximum value that the score can take
#' @param model Model name
#' @param prior A named list of corresponding to parameters' prior.
#' @param ... Arguments to be pass to [rstan::sampling()]
#'
#' @return Stanfit object
#'
#' @seealso [fit_BinRW()], [fit_BinMC()], [fit_OrderedRW()], [fit_RW()]
#'
#' @export
fit_discrete <- function(train,
                         test = NULL,
                         max_score,
                         model = c("BinRW", "BinMC", "OrderedRW", "RW"),
                         prior = default_prior(model, max_score),
                         ...) {

  fit_all(train = train, test = test, max_score = max_score, discrete = TRUE, model = model, prior = prior, ...)

}

#' Fit continuous models
#'
#' @param train Training dataframe (details of the format in [prepare_data_lgtd()])
#' @param test Testing dataframe (details of the format in [prepare_data_lgtd()])
#' @param max_score Maximum value that the score can take
#' @param model Model name
#' @param prior A named list of corresponding to parameters' prior.
#' @param ... Arguments to be pass to [rstan::sampling()]
#'
#' @return Stanfit object
#'
#' @seealso [fit_RW()], [fit_Smoothing()], [fit_AR1()], [fit_MixedAR1()]
#'
#' @export
fit_continuous <- function(train,
                           test = NULL,
                           max_score,
                           model = c("RW", "Smoothing", "AR1", "MixedAR1"),
                           prior = default_prior(model, max_score),
                           ...) {

  fit_all(train = train, test = test, max_score = max_score, discrete = FALSE, model = model, prior = prior, ...)

}

# Sample prior ------------------------------------------------------------

#' Sample prior for different models
#'
#' @param N_patient Number of patients
#' @param t_max Vector of size N_patient indicating the time-series length
#' @param max_score Maximum value that the score can take
#' @param discrete Whether the model is discrete
#' @param model Model name
#' @param prior A named list of corresponding to parameters' prior.
#' @param ... Arguments to be pass to [rstan::sampling()]
#'
#' @return Stanfit object
#' @noRd
sample_prior_all <- function(N_patient = 1,
                             t_max = c(2),
                             max_score,
                             discrete = FALSE,
                             model = c("BinRW", "BinMC", "OrderedRW", "RW", "Smoothing", "AR1", "MixedAR1"),
                             prior = default_prior(model, max_score),
                             ...) {

  data_stan <- prepare_priorpred_lgtd(N_patient = N_patient, t_max = t_max, max_score = max_score, discrete = discrete)

  stopifnot_prior(prior, model = model, max_score = max_score)
  data_stan <- add_prior(data_stan, prior)

  data_stan$discrete <- as.numeric(discrete) # only useful for RW
  data_stan$run <- 0 # only useful for state-space models

  fit <- rstan::sampling(stanmodels[[model]], data = data_stan, ...)

  return(fit)

}

#' Sample prior for discrete models
#'
#' Except Markov Chain model
#'
#' @param N_patient Number of patients
#' @param t_max Vector of size N_patient indicating the time-series length
#' @param max_score Maximum value that the score can take
#' @param model Model name
#' @param prior A named list of corresponding to parameters' prior.
#' @param ... Arguments to be pass to [rstan::sampling()]
#'
#' @return Stanfit object
#'
#' @seealso [sample_prior_BinRW()], [sample_prior_BinMC()], [sample_prior_OrderedRW()], [sample_prior_RW()]
#'
#' @export
sample_prior_discrete <- function(N_patient = 1,
                                  t_max = c(2),
                                  max_score,
                                  model = c("BinRW", "BinMC", "OrderedRW", "RW"),
                                  prior = default_prior(model, max_score),
                                  ...) {

  sample_prior_all(N_patient = N_patient, t_max = t_max, max_score = max_score, discrete = TRUE, model = model, prior = prior, ...)

}

#' Sample prior for continuous models
#'
#' @param N_patient Number of patients
#' @param t_max Vector of size N_patient indicating the time-series length
#' @param max_score Maximum value that the score can take
#' @param model Model name
#' @param prior A named list of corresponding to parameters' prior.
#' @param ... Arguments to be pass to [rstan::sampling()]
#'
#' @return Stanfit object
#'
#' @seealso [sample_prior_RW()], [sample_prior_Smoothing()], [sample_prior_AR1()], [sample_prior_MixedAR1()]
#'
#' @export
sample_prior_continuous <- function(N_patient = 1,
                                    t_max = c(2),
                                    max_score,
                                    model = c("RW", "Smoothing", "AR1", "MixedAR1"),
                                    prior = default_prior(model, max_score),
                                    ...) {

  sample_prior_all(N_patient = N_patient, t_max = t_max, max_score = max_score, discrete = FALSE, model = model, prior = prior, ...)

}
