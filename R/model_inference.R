# EczemaFit ---------------------------------------------------------------

#' @param ... Arguments to pass to [rstan::sampling()]
#'
#' @export
#'
#' @rdname EczemaFit
#'
#' @examples
#' \dontrun{
#' model <- EczemaModel("BinRW", max_score = 100)
#' train <- data.frame(Patient = 1, Time = 1:10, Score = rbinom(10, 100, .5))
#' EczemaFit(model, train)
#' }
EczemaFit.EczemaModel <- function(model, train, test = NULL, ...) {

  data_stan <- prepare_data_lgtd(train = train,
                                 test = test,
                                 max_score = model$max_score,
                                 discrete = model$discrete) %>%
    add_prior(model$prior) %>%
    c(list(
      discrete = as.numeric(model$discrete), # only useful for RW
      run = 1 # only useful for state-space models
    ))

  fit <- rstan::sampling(stanmodels[[model$stanmodel]], data = data_stan, ...)

  return(fit)

}

#' @param ... Arguments to pass to [rstan::sampling()]
#'
#' @export
#'
#' @rdname EczemaFit
#'
#' @examples
#' \dontrun{
#' model <- EczemaModel("MC", K = 5)
#' N <- 1e2
#' train <- data.frame(y0 = rbinom(N, 5, .5), y1 = rbinom(N, 5, .5), dt = rpois(N, 1) + 1)
#' EczemaFit(model, train)
#' }
EczemaFit.MC <- function(model, train, test = NULL, ...) {

  data_stan <- prepare_data_MC(train = train, test = test, K = model$K) %>%
    add_prior(model$prior)

  fit <- rstan::sampling(stanmodels$MC, data = data_stan, ...)

  return(fit)

}

# sample_prior ------------------------------------------------------------

#' @param N_patient Number of patients to simulate
#' @param t_max Vector of size `N_patient` indicating the time-series length of each patient
#' @param ... Arguments to pass to [rstan::sampling()]
#'
#' @export
#'
#' @rdname sample_prior
#'
#' @examples
#' \dontrun{
#' model <- EczemaModel("BinRW", max_score = 100)
#' sample_prior(model)
#' }
sample_prior.EczemaModel <- function(model, N_patient = 1, t_max = c(2), ...) {

  data_stan <- prepare_priorpred_lgtd(N_patient = N_patient,
                                      t_max = t_max,
                                      max_score = model$max_score,
                                      discrete = model$discrete) %>%
    add_prior(model$prior) %>%
    c(list(
      discrete = as.numeric(model$discrete), # only useful for RW
      run = 0 # only useful for state-space models
    ))

  fit <- rstan::sampling(stanmodels[[model$stanmodel]], data = data_stan, ...)

  return(fit)

}

#' @param data Dataframe (details of the format in ...)
#' Only the columns `y0` and `dt` are relevant to simulate data from.
#' @param ... Arguments to pass to [rstan::sampling()]
#'
#' @export
#'
#' @rdname sample_prior
#'
#' @examples
#' \dontrun{
#' model <- EczemaModel("MC", K = 5)
#' sample_prior(model)
#' }
sample_prior.MC <- function(model,
                            data = data.frame(y0 = integer(), y1 = integer(), dt = integer()),
                            ...) {

  data_stan <- prepare_data_MC(train = data, test = NULL, K = model$K) %>%
    add_prior(model$prior)
  data_stan$run <- 0

  fit <- rstan::sampling(stanmodels$MC, data = data_stan, ...)

  return(fit)

}
