# EczemaFit ---------------------------------------------------------------

#' @param ... Arguments to pass to [rstan::sampling()]
#'
#' @export
#'
#' @describeIn EczemaFit Method for EczemaModel object
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

# sample_prior ------------------------------------------------------------

#' @param N_patient Number of patients to simulate
#' @param t_max Vector of size `N_patient` indicating the time-series length of each patient
#' @param ... Arguments to pass to [rstan::sampling()]
#'
#' @export
#'
#' @describeIn sample_prior Method for EczemaModel object
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
