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

  data_stan <- prepare_standata(model, train = train, test = test) %>%
    c(list(run = 1)) # only useful for main models and MC

  fit <- rstan::sampling(stanmodels[[model$stanmodel]], data = data_stan, ...)

  return(fit)

}

# sample_prior ------------------------------------------------------------

#' Create "empty" train and test set to pass to `prepare_standata` for `sample_prior`
#'
#' Used internally.
#'
#' @param N_patient Number of patients in the datasets
#' @param t_max Vector of size `N_patient` indicating the time-series length of each patient
#' @param max_score Maximum value that the score can take
#' @param discrete Whether to use a discrete normal distribution (only relevant for testing)
#'
#' @return List containing "Training" and "Testing" dataframes
#'
#' @export
#'
#' @examples
#' make_empty_data(max_score = 10, discrete = TRUE)
make_empty_data <- function(N_patient = 1, t_max = c(2), max_score, discrete) {

  train <- data.frame(Patient = 1:N_patient,
                      Time = 1,
                      Score = stats::runif(N_patient, 0, max_score))
  if (discrete) {
    train[["Score"]] <- round(train[["Score"]])
  }

  test <- data.frame(Patient = 1:N_patient,
                     Time = t_max,
                     Score = 0)

  return(list(Training = train, Testing = test))

}

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

  tmp <- make_empty_data(N_patient = N_patient,
                         t_max = t_max,
                         max_score = model$max_score,
                         discrete = model$discrete)

  data_stan <- prepare_standata(model, train = tmp[["Training"]], test = tmp[["Testing"]]) %>%
    c(list(run = 0)) # only useful for state-space models

  fit <- rstan::sampling(stanmodels[[model$stanmodel]], data = data_stan, ...)

  return(fit)

}

#' @param data Dataframe (see details below).
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

  data_stan <- prepare_standata(model, train = data, test = NULL)
  data_stan$run <- 0

  fit <- rstan::sampling(stanmodels$MC, data = data_stan, ...)

  return(fit)

}
