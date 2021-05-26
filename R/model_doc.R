# BinRW -------------------------------------------------------------------

#' Binomial random walk model
#'
#' This is a state-space model defined by a Binomial measurement error and a latent random walk.
#' For more details see the [vignette](https://ghurault.github.io/EczemaPred/articles/BinRW.html).
#'
#' @section Priors:
#' The priors are passed as a named list with elements `sigma`, `mu_logit_y0` and `sigma_logit_y0`
#' specifying priors for the corresponding parameters.
#' Each element of the list should be a vector of length 2, containing values for x1 and x2, x2 > 0, such as:
#'
#' - `sigma ~ normal(x1, x2)`.
#' - `mu_logit_y0 ~ normal(x1, x2)`
#' - `sigma_logit_y0 ~ normal(x1, x2)`
#'
#' NB: For `sigma` and `sigma_logit_y0`, usually x1=0 to define a half-normal distribution
#' since the parameters are constrained to be positive.
#'
#' @section Default priors:
#' - The default priors do not depend on `max_score`, and are printed in the examples section.
#' - The default prior for `sigma` translates to an odd ratio increment of at most 5 (~ 2 * upper bound of prior).
#' - The default priors for `mu_logit_y0` and `sigma_logit_y0` translates to an approximately uniform prior on `y0`.
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
#' @name BinRW
#'
#' @examples
#' EczemaModel("BinRW", max_score = 100)
NULL
