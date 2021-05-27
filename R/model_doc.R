# BinRW -------------------------------------------------------------------

#' Binomial random walk model
#'
#' This is a state-space model defined by a Binomial measurement error and a latent random walk.
#' For more details see the [vignette](https://ghurault.github.io/EczemaPred/articles/BinRW.html).
#'
#' @param max_score Maximum value that the score can take
#' @param prior Named list of the model's priors. If `NULL`, uses the default prior for the model (see [default_prior()]).
#'
#' @details Details of the model are available in the [paper](#).
#'
#' @section Parameters:
#'
#' Population parameters:
#'
#' - `sigma`: Standard deviation of the random walk (in logit scale)
#' - `mu_logit_y0`: Population mean of the initial condition (in logit scale)
#' - `sigma_logit_y0`: Population standard deviation of the initial condition (logit scale)
#'
#' Patient-dependent parameters:
#'
#' - `logit_y0`: Logit of the initial condition
#'
#' Observation-dependent (patient- and time-dependent) parameters:
#'
#' - `y_lat`: Latent score (probability)
#' - `logit_lat`: logit of `y_lat`
#'
#' See `list_parameters(model = "BinRW")`.
#'
#' @section Priors:
#' The priors are passed as a named list with elements `sigma`, `mu_logit_y0` and `sigma_logit_y0`
#' specifying priors for the corresponding parameters.
#' Each element of the list should be a vector of length 2, containing values for x1 and x2, x2 > 0, such as:
#'
#' - `sigma ~ normal+(x1, x2)`.
#' - `mu_logit_y0 ~ normal(x1, x2)`
#' - `sigma_logit_y0 ~ normal+(x1, x2)`
#'
#' NB: For `sigma` and `sigma_logit_y0`, usually x1=0 to define a half-normal distribution
#' since the parameters are constrained to be positive.
#'
#' @section Default priors:
#' - The default priors do not depend on `max_score`, and are printed in the examples section.
#' - The default prior for `sigma` translates to an odd ratio increment of at most 5 (~ 2 * upper bound of prior).
#' - The default priors for `mu_logit_y0` and `sigma_logit_y0` translates to an approximately uniform prior on `y0`.
#'
#' @name BinRW
#'
#' @examples
#' EczemaModel("BinRW", max_score = 100)
NULL

# OrderedRW ---------------------------------------------------------------

#' Ordered Logistic random walk model
#'
#' This is a state-space model defined by a Ordered logistic measurement error distribution and a latent random walk.
#' For more details see the BinRW [vignette](https://ghurault.github.io/EczemaPred/articles/BinRW.html).
#'
#' @param max_score Maximum value that the score can take
#' @param prior Named list of the model's priors. If `NULL`, uses the default prior for the model (see [default_prior()]).
#'
#' @details Details of the model are available in the [paper](#).
#'
#' @section Parameters:
#'
#' Population parameters:
#'
#' - `sigma`: Standard deviation of the random walk
#' - `mu_y0`: Population mean of `y0` (initial condition).
#' - `sigma_y0`: Population standard deviation of `y0` (initial condition).
#' - `delta`: Difference between cutpoints (vector of length `max_score - 1`)
#' - `ct`: Cutpoints (vector of length `max_score`)
#' - `p0`: Probability distribution of the average patient at t0 (vector of length `max_score`)
#'
#' Patient-dependent parameters:
#'
#' - `y0`: `y_lat` at t0.
#'
#' Observation-dependent (patient- and time-dependent) parameters:
#'
#' - `y_lat`: Latent score
#'
#' See `list_parameters(model = "OrderedRW")`.
#'
#' @section Priors:
#' The priors are passed as a named list with elements `delta`, `sigma`, `mu_y0` and `sigma_y0`
#' specifying priors for the corresponding parameters.
#'
#' The element `delta` should be a matrix with 2 rows and `max_score - 1` columns,
#' such as the i-th column is a vector with values x1 and x2, where x2 > 0 and
#' `delta[i] ~ normal+(x1, x2)`.
#' The other parameters are normalised by the difference between the highest and lowest cutpoints (approx. the range of the score),
#' and their priors are defined by a vector of length 2, containing values for x1 and x2, x2 > 0, such as:
#'
#' - `sigma ~ normal+(x1, x2)`
#' - `mu_y0 ~ normal(x1, x2)`
#' - `sigma_y0 ~ normal+(x1, x2)`
#'
#' NB: `delta`, `sigma` and `sigma_y0` are constrained to be positive so x1 are usually set to 0 to define a half-normal distribution.
#'
#' @section Default priors:
#' - The default prior for `delta` is set so that `delta` is less than the width of the logistic distribution.
#' - The default prior for `sigma` assumes it would be to go to a state where `y = 0` is the most likely outcome to
#' a state where `y = M` in two transitions.
#' - The default priors for `mu_y0` and `sigma_y0` have reasonable ranges and translate to an approximately uniform prior
#' over the range of the score for `y0`.
#'
#' @name OrderedRW
#'
#' @examples
#' EczemaModel("OrderedRW", max_score = 10)
NULL

# BinMC -------------------------------------------------------------------

#' Binomial Markov Chain model
#'
#' This is a state-space model defined by a Binomial measurement error and a latent Markov Chain.
#' For more details see the BinRW [vignette](https://ghurault.github.io/EczemaPred/articles/BinRW.html).
#'
#' @param max_score Maximum value that the score can take
#' @param prior Named list of the model's priors. If `NULL`, uses the default prior for the model (see [default_prior()]).
#'
#' @details Details of the model are available in the [paper](#).
#'
#' @section Parameters:
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
#' @section Priors:
#' The priors are passed as a named list with elements `sigma`, `mu_logit_p10` and `sigma_logit_p10`
#' specifying priors for the corresponding parameters.
#' Each element of the list should be a vector of length 2, containing values for x1 and x2, x2 > 0, such as:
#'
#' - `sigma ~ normal+(x1, x2)`
#' - `mu_logit_p10 ~ normal(x1, x2)`
#' - `sigma_logit_p10 ~ normal+(x1, x2)`
#' - `logit_tss1_0 ~ normal(x1, x2)`
#'
#' NB: For `sigma` and `sigma_logit_p10`, usually x1=0 to define a half-normal distribution
#' since the parameter is constrained to be positive.
#'
#' @section Default priors:
#' - The default prior for `sigma` translates to an odd ratio increment of at most 5 (~ 2 * upper bound of prior).
#' - The default priors for `mu_logit_p10` and `sigma_logit_p10` translate to an approximately uniform prior on `p10`.
#' - The prior for the initial condition of `ss1` is hard coded and a function of `p10`.
#'
#' @name BinMC
#'
#' @examples
#' EczemaModel("BinMC", max_score = 100)
NULL
