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

# Generate fake data ------------------------------------------------------

#' Generate fake data from an smoothing mixed effect autoregressive model
#'
#' @param N_pt Number of patients to generate
#' @param t_max Length of time-series of each patient
#' @param max_score Maximum value that the score can take
#' @param params List of parameters.
#' Default to parameters of a random walk
#'
#' @return Dataframe of fake time-series
#' @noRd
generate_fakedata <- function(
  N_pt = 10,
  t_max = rpois(N_pt, 20),
  max_score = 100,
  params = list(alpha = 1,
                intercept = rep(0, N_pt),
                slope = rep(1, N_pt),
                y0 = round(rbeta(N_pt, 5, 5) * max_score),
                sigma = .01 * max_score)
) {

  lapply(1:N_pt,
         function(k) {

           err <- rnorm(t_max[k] - 1, 0, params$sigma)
           y <- rep(NA, t_max[k])
           y[1] <- params$y0[k]
           l <- y
           for (t in 2:t_max[k]) {
             y[t] = params$slope[k] * l[t - 1] + params$intercept[k] + err[t - 1]
             l[t] = params$alpha * y[t] + (1 - params$alpha) * l[t - 1]
           }

           out <- tibble(Patient = k,
                         Time = 1:t_max[k],
                         Score = y) %>%
             mutate(OutsideRange = (Score < 0 | Score > max_score),
                    OutsideRange = cumsum(OutsideRange)) %>%
             filter(OutsideRange == 0)

           return(out)

         }) %>%
    bind_rows()

}
