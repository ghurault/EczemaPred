# Extract fake data -------------------------------------------------------

#' Extract fake data
#'
#' Similar to `extract_simulations`, except that the data is split into training and testing set (leave last observations)
#' and that the output is different.
#' The function is not exported and is a helper for tests.
#'
#' NB: Apart from testing `plot_ppc` in BinRW, we may not need this function.
#'
#' @param fit Stanfit object
#' @param id Dataframe linking index in obj to (Patient, Time) pairs, cf. output from [get_index()]
#' @param draw Draw ID
#' @param pars Vector of parameters to extract. Default to all parameters except `y_rep`.
#' @param horizon Prediction horizon for test set
#'
#' @return List containing three dataframes:
#' - TrueParameters (parameters used to generate the data)
#' - Train (training set)
#' - Test (testing set)
#'
#' @noRd
extract_fakedata <- function(fit, id, draw, pars = NULL, horizon = 0) {

  tmp <- extract_simulations(fit = fit, id = id, draw = draw, pars = pars)

  train <- tmp$Data %>%
    group_by(.data$Patient) %>%
    filter(.data$Time <= max(.data$Time) - horizon) %>%
    ungroup() %>%
    drop_na()

  if (horizon > 0) {
    test <- tmp$Data %>%
      group_by(.data$Patient) %>%
      filter(.data$Time > max(.data$Time) - horizon) %>%
      mutate(Horizon = .data$Time - min(.data$Time) + 1) %>%
      ungroup() %>%
      drop_na()
  } else {
    test <- NULL
  }

  return(list(TrueParameters = tmp$Parameters,
              Train = train,
              Test = test))
}
