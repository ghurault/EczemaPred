# Extract fake data -------------------------------------------------------

#' Extract fake data
#'
#' Not exported
#'
#' @param fit_prior Stanfit object corresponding to prior predictive distribution
#' @param draw Draw ID
#' @param pars Vector of parameter names to extract
#' @param N_patient Number of patients generated in fit_prior
#' @param t_max Vector of size N_patient indicating the time-series length generated in fit_prior
#' @param horizon Prediction horizon (for test set)
#'
#' @import dplyr
#' @noRd
#'
#' @return List containing three dataframes:
#' - TrueParameters (parameters used to generate the data)
#' - Train (training set)
#' - Test (testing set)
extract_fakedata <- function(fit_prior, draw, pars, N_patient, t_max, horizon) {

  true_param <- rstan::extract(fit_prior, pars = pars) %>%
    HuraultMisc::extract_draws(draw)

  yrep <- rstan::extract(fit_prior, pars = "y_rep")[[1]]

  fd <- get_index2(N_patient, t_max) %>%
    mutate(Score = yrep[draw, ])

  train <- fd %>%
    group_by(.data$Patient) %>%
    filter(.data$Time <= max(.data$Time) - horizon) %>%
    ungroup() %>%
    drop_na()

  test <- fd %>%
    group_by(.data$Patient) %>%
    filter(.data$Time > max(.data$Time) - horizon) %>%
    mutate(Horizon = .data$Time - min(.data$Time) + 1) %>%
    ungroup() %>%
    drop_na()

  return(list(TrueParameters = true_param,
              Train = train,
              Test = test))
}
