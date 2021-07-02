# Two states Markov Chain sequence ---------------------------------------------------------------------

#' Generate two states Markov Chain sequence
#'
#' @param N Length of the sequence
#' @param p01 Probability of transitioning from state 0 to state 1
#' @param p10 Probability of transitioning from state 1 to state 0
#' @param t0 Initial state
#'
#' @return Numeric vector of length N taking 0 or 1 values
#'
#' @noRd
#'
#' @importClassesFrom markovchain markovchain
#' @examples
#' generate_MC2_sequence(10)
generate_MC2_sequence <- function(N, p01 = .5, p10 = .5, t0 = NULL) {

  stopifnot(is.numeric(c(N, p01, p10)),
            all(vapply(list(N, p01, p10), function(x) {length(x) == 1}, logical(1))),
            N == round(N),
            p01 >= 0 & p01 <= 1,
            p10 >= 0 & p10 <= 1)

  tm <- matrix(c(1 - p01, p01,
                 p10, 1 - p10),
               nrow = 2, ncol = 2, byrow = TRUE)
  tm <- new("markovchain",
            transitionMatrix = tm,
            states = c("0", "1"))
  if (is.null(t0)) {
    t0 <- sample(tm@states, 1)
  } else {
    stopifnot(t0 %in% list(0, 1, "0", "1"))
  }
  out <- markovchain::markovchainSequence(N - 1, tm, t0 = as.character(t0), include.t0 = TRUE)
  out <- as.numeric(out)

  return(out)
}

# Generate missing --------------------------------------------------------

#' Generate missing values in a times-series
#'
#' First and last values are nor missing.
#' Missing indices can be generated at random (Binomial distribution) or using a Markov Chain (if consecutives missing values are deemed more likely).
#' The markov chain is parametrised in terms of the steady state probability of a value being missing and the probability that the next value is observed when the current value is also observed.
#'
#' @param N Length of the time-series
#' @param type Method to generate the missing values. One of "random" or "markovchain"
#' @param p_mis Probability of a given value to be missing (steady state probability for type == "markovchain")
#' @param p_obs_obs Probability of the next value being observed when the current is observed (for type == "markovchain")
#'
#' @return Logical vector of length N
#' @export
#'
#' @examples
#' generate_missing(10)
#' generate_missing(10, type = "markovchain")
generate_missing <- function(N, type = c("random", "markovchain"), p_mis = .25, p_obs_obs = .75) {

  stopifnot(is_scalar_wholenumber(N),
            is_scalar(p_mis),
            is.numeric(p_mis),
            dplyr::between(p_mis, 0, 1))
  type <- match.arg(type)

  if (type == "random") {
    id_mis <- as.logical(stats::rbinom(N - 2, 1, p_mis))
  } else if (type == "markovchain") {
    stopifnot(is_scalar(p_obs_obs),
              is.numeric(p_obs_obs),
              dplyr::between(p_obs_obs, 1 - p_mis / (1 - p_mis), 1))

    beta <- 1 - p_obs_obs # transition probability from observed to missing
    alpha <-  beta * (1 - p_mis) / p_mis # transition probability from missing to observed
    id_mis <- generate_MC2_sequence(N - 2, p01 = alpha, p10 = beta, t0 = 1)
    id_mis <- !as.logical(id_mis)
  }

  id_mis <- c(FALSE, id_mis, FALSE)
  return(id_mis)
}
