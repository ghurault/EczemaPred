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
#' ## Population parameters:
#'
#' - `sigma`: Standard deviation of the random walk (in logit scale)
#' - `mu_logit_y0`: Population mean of the initial condition (in logit scale)
#' - `sigma_logit_y0`: Population standard deviation of the initial condition (logit scale)
#'
#' ## Patient-dependent parameters:
#'
#' - `logit_y0`: Logit of the initial condition
#'
#' ## Observation-dependent (patient- and time-dependent) parameters:
#'
#' - `y_lat`: Latent score (probability)
#' - `logit_lat`: logit of `y_lat`
#'
#' See `list_parameters(model = "BinRW")` for more details.
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
#' @param prior Named list of the model's priors.
#' If `NULL`, uses the default prior for the model (see [default_prior()]).
#'
#' @details Details of the model are available in the [paper](#).
#'
#' @section Parameters:
#'
#' ## Population parameters:
#'
#' - `sigma_lat`: Standard deviation of the random walk
#' - `sigma_meas`: Standard deviation (not scale) of the logistic distribution (in `[0, max_score]` space)
#' - `sigma_tot`: Total standard deviation for prediction one step ahead
#' - `rho2`: Proportion of measurement variance to the total variance.
#' It can be interpreted similarly to an R-squared, the proportion of the explained variance
#' (the variance of the measurements) in the total variance.
#' - `mu_y0`: Population mean of `y0` (initial condition).
#' - `sigma_y0`: Population standard deviation of `y0` (initial condition).
#' - `delta`: Relative difference between cutpoints (simplex of length `max_score - 1`)
#' - `ct`: Cutpoints (vector of length `max_score`, in `[0, max_score]` space)
#'
#' ## Patient-dependent parameters:
#'
#' - `y0`: initial latent score (`y_lat` at t0).
#'
#' ## Observation-dependent (patient- and time-dependent) parameters:
#'
#' - `y_lat`: Latent score (in `[0, max_score]` space)
#'
#' See `list_parameters(model = "OrderedRW")` for more details.
#'
#' @section Priors:
#' The priors are passed as a named list with elements `delta`, `sigma_lat`, `sigma_meas`, `mu_y0` and `sigma_y0`
#' specifying priors for the corresponding parameters.
#'
#' The element `delta` should be a vector X1 of length `max_score - 1`,
#' such as all all elements of X1 are positive and
#' `delta ~ dirichlet(X1)`.
#'
#' The latent score can be interpreted in the original `[0, max_score]` space,
#' the priors for the other parameters are specified normalised `max_score`.
#' Their priors are defined by a vector of length 2, containing values for x1 and x2, x2 > 0, such as:
#'
#' - `sigma_meas / max_score ~ lognormal(x1, x2)`
#' - `sigma_lat / max_score ~ lognormal(x1, x2)`
#' - `mu_y0 ~ normal(x1, x2)`
#' - `sigma_y0 ~ normal+(x1, x2)`
#'
#' NB: for the lognormal distribution, x1 corresponds to the mean of the log and x2 to the sd of the log.
#' NB: `sigma_y0` is constrained to be positive so x1 are usually set to 0 to define a half-normal distribution.
#'
#' @section Default priors:
#' - The default prior for `delta` is a uniform symmetric Dirichlet distribution with concentration 2.
#' - The default priors for `sigma_meas` and `sigma_lat` are lognormal distribution which translate to
#' a 95% CI that is approximately `[.02, 0.40] * M`.
#' The prior for `sigma_lat` thus allows fast or slow transitions from a state where `y = 0`
#' is the most likely outcome to a state where `y = M` is the most likely outcome.
#' The prior for `sigma_meas` allows very precise or imprecise measurements.
#' - The default priors for `mu_y0` and `sigma_y0` have reasonable ranges and translate to
#' an approximately uniform prior over the range of the score for `y0`.
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
#' ## Population parameters:
#'
#' - `sigma`: Standard deviation of the evolution of `ss1`
#' - `mu_logit_p10`: Population logit mean of `p10`
#' - `sigma_logit_p10`: Population logit standard deviation of `p10`
#'
#' ## Patient-dependent parameters:
#'
#' - `p10`: Probability of transitioning from state 1 to state 0
#' - `logit_p10`: logit of `p10`
#' - `logit_tss1_0`: Initial condition of the `logit(ss1 * (1 + p10))`
#'
#' ## Observation-dependent (patient- and time-dependent) parameters:
#'
#' - `p01`: Probability of transitioning from state 0 to state 1
#' - `lambda`: Mobility of the Markov Chain (eigenvalue of the transition matrix)
#' - `ss1`: Steady state probability of state 1
#' - `y_lat`: Latent score (probability)
#'
#' See `list_parameters(model = "BinMC")` for more details.
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

# RW ----------------------------------------------------------------------

#' Random walk model
#'
#' @param max_score Maximum value that the score can take
#' @param discrete Whether to use a discrete normal distribution.
#' This will be used to check whether the data is discrete or not, and for rounding predictions (cf. testing).
#' @param prior Named list of the model's priors. If `NULL`, uses the default prior for the model (see [default_prior()]).
#'
#' @details
#' - Details of the model are available in the [paper](#).
#' - The model takes as input a continuous score defined between 0 and `max_score`.
#' - The model is naive as the likelihood is non-truncated and not discretised (when `discrete = TRUE`).
#' As a result, sampling from the prior predictive distribution can be challenging if the score is near the bounds
#' and the variance is sufficiently large.
#' - For more details see the [vignette](https://ghurault.github.io/EczemaPred/articles/ContinuousModels.html).
#'
#' @section Parameters:
#'
#' - `sigma`: Standard deviation of the random walk
#' - `y_mis`: Missing values
#'
#' See `list_parameters(model = "RW")` for more details.
#'
#' @section Priors:
#' The priors are passed as a named list with element `sigma`
#' specifying priors for the corresponding parameter, where
#' `sigma / max_score ~ normal+(x1, x2)`
#' and the element `sigma` of the list is a vector of length two containing x1 and x2.
#' NB: usually x1=0 to define a half-normal distribution (sigma is constraint to be positive) and
#' x2 should be positive.
#'
#' @section Default priors:
#' The default prior for `sigma` translates to a width of the predictive distribution to be at most `max_score`.
#'
#' @name RW
#'
#' @examples
#' EczemaModel("RW", max_score = 100, discrete = FALSE)
NULL

# Smoothing ---------------------------------------------------------------

#' Exponential smoothing model
#'
#' @param max_score Maximum value that the score can take
#' @param discrete Whether to use a discrete normal distribution.
#' This will be used to check whether the data is discrete or not, and for rounding predictions (cf. testing).
#' @param prior Named list of the model's priors. If `NULL`, uses the default prior for the model (see [default_prior()]).
#'
#' @details
#' - Details of the model are available in the [paper](#).
#' - The model takes as input a continuous score defined between 0 and `max_score`.
#' - The model is naive as the likelihood is non-truncated and not discretised (when `discrete = TRUE`).
#' As a result, sampling from the prior predictive distribution can be challenging if the score is near the bounds
#' and the variance is sufficiently large.
#' - For more details see the [vignette](https://ghurault.github.io/EczemaPred/articles/ContinuousModels.html).
#'
#' @section Parameters:
#'
#' - `sigma`: Standard deviation of the random walk
#' - `alpha`: Smoothing factor
#' - `tau`: Time constant associated with the smoothing factor
#' - `y_mis`: Missing values
#'
#' See `list_parameters(model = "Smoothing")` for more details.
#'
#' @section Priors:
#' The priors are passed as a named list with elements `sigma` and `tau`
#' specifying priors for the corresponding parameters.
#' Each element of the list should be a vector of length 2, containing values for x1 and x2, x2 > 0, such as:
#'
#' - `sigma / max_score ~ normal+(x1, x2)`.
#' - `tau ~ lognormal(x1, x2)`.
#'
#' NB: For `sigma`, usually x1=0 to define a half-normal distribution
#' since the parameter is constrained to be positive.
#'
#' @section Default priors:
#' - The default prior for `sigma` translates to a width of the predictive distribution to be at most `max_score`.
#' - The default prior for `tau` assumes it could range from less a 1 to 100 (time units).
#'
#' @name Smoothing
#'
#' @examples
#' EczemaModel("Smoothing", max_score = 100)
NULL

# AR1 ---------------------------------------------------------------------

#' Autoregressive model (order 1)
#'
#' @param max_score Maximum value that the score can take
#' @param discrete Whether to use a discrete normal distribution.
#' This will be used to check whether the data is discrete or not, and for rounding predictions (cf. testing).
#' @param prior Named list of the model's priors. If `NULL`, uses the default prior for the model (see [default_prior()]).
#'
#' @details
#' - Details of the model are available in the [paper](#).
#' - The model takes as input a continuous score defined between 0 and `max_score`.
#' - The model is naive as the likelihood is non-truncated and not discretised (when `discrete = TRUE`).
#' As a result, sampling from the prior predictive distribution can be challenging if the score is near the bounds
#' and the variance is sufficiently large.
#' - For more details see the [vignette](https://ghurault.github.io/EczemaPred/articles/ContinuousModels.html).
#'
#' @section Parameters:
#'
#' - `sigma`: Standard deviation of the autoregression
#' - `slope`: Autocorrelation parameter
#' - `intercept`: Intercept
#' - `y_inf`: Autoregression mean
#' - `y_mis`: Missing values
#'
#' See `list_parameters(model = "AR1")` for more details.
#'
#' @section Priors:
#' The priors are passed as a named list with elements `sigma`, `y_inf` and `slope`
#' specifying priors for the corresponding parameters.
#' Each element of the list should be a vector of length 2, containing values for x1 and x2, x2 > 0, such as:
#'
#' - `sigma / max_score ~ normal+(x1, x2)`.
#' - `y_inf / max_score ~ normal(x1, x2)`.
#' - `slope ~ beta(x1, x2)`.
#'
#' NB: For `sigma`, usually x1=0 to define a half-normal distribution
#' since the parameter is constrained to be positive.
#' NB: For `slope`, both `x1` and `x2` must be positive.
#'
#' @section Default priors:
#' - The default prior for `sigma` translates to a width of the predictive distribution to be at most `max_score`.
#' - The default prior for `y_inf` covers the full range of the score.
#' - The default prior for `slope` is uniform in 0-1.
#'
#' @details
#' - The model is naive as it is trained with a non-truncated distribution
#' - For more details see the [vignette](https://ghurault.github.io/EczemaPred/articles/ContinuousModels.html).
#'
#' @name AR1
#'
#' @examples
#' EczemaModel("AR1", max_score = 100)
NULL

# MixedAR1 ----------------------------------------------------------------

#' Mixed effect autoregressive model (order 1)
#'
#' @param max_score Maximum value that the score can take.
#' Note that even if `discrete=FALSE`, `max_score` must be an integer.
#' @param discrete Whether to use a discrete normal distribution.
#' This will be used to check whether the data is discrete or not, and for rounding predictions (cf. testing).
#' @param prior Named list of the model's priors. If `NULL`, uses the default prior for the model (see [default_prior()]).
#'
#' @details
#' - Details of the model are available in the [paper](#).
#' The model takes as input a continuous score defined between 0 and `max_score`.
#' - The model is naive as the likelihood is non-truncated and not discretised (when `discrete = TRUE`).
#' - Unlike the `AR1` model, the discretisation of predictions is not implemented
#' - For more details see the [vignette](https://ghurault.github.io/EczemaPred/articles/ContinuousModels.html).
#'
#' @section Parameters:
#'
#' ## Population parameters:
#'
#' - `sigma`: Standard deviation of the autoregression
#' - `mu_logit_slope`: Population mean of the logit of `slope`
#' - `sigma_logit_slope`: Population standard deviation of the logit of `slope`
#' - `mu_inf`: Population mean of `y_inf`
#' - `sigma_inf`: Population standard deviation of `y_inf`
#'
#' ## Patient-dependent parameters:
#'
#' - `slope`: Autocorrelation parameter
#' - `y_inf`: Autoregression mean
#' - `intercept`: Intercept
#'
#' ## Other parameters:
#'
#' - `y_mis`: Missing values
#'
#' See `list_parameters(model = "MixedAR1")` for more details.
#'
#' @section Priors:
#' The priors are passed as a named list with elements `sigma`, `mu_logit_slope`, `sigma_logit_slope`, `mu_inf`, `sigma_inf`
#' specifying priors for the corresponding parameters.
#' Each element of the list should be a vector of length 2, containing values for x1 and x2, x2 > 0, such as:
#'
#' - `sigma / max_score ~ normal+(x1, x2)`.
#' - `mu_logit_slope ~ normal(x1, x2)`.
#' - `sigma_logit_slope ~ normal+(x1, x2)`.
#' - `mu_inf / max_score ~ normal(x1, x2)`.
#' - `sigma_inf / max_score ~ normal+(x1, x2)`.
#'
#' NB: For `sigma`, `sigma_logit_slope` and `sigma_inf`, usually x1=0 to define a half-normal distribution
#' since the parameter is constrained to be positive.
#'
#' @section Default priors:
#' - The default prior for `sigma` translates to a width of the predictive distribution to be at most `max_score`.
#' - The default priors for `mu_logit_slope` and `sigma_logit_slope` have "reasonable" ranges and
#' translate to a prior on `slope` that is approximately uniform.
#' - The default prior for `mu_inf` spans the entire range of the score.
#' - The default prior for `sigma_inf` translates to a range in the distribution of `y_inf` to be at most `max_score`.
#'
#' @name MixedAR1
#'
#' @examples
#' EczemaModel("MixedAR1", max_score = 100)
NULL

# MC ----------------------------------------------------------------------

#' Markov Chain model
#'
#' For more details see the [Markov Chain vignette](https://ghurault.github.io/EczemaPred/articles/MC.html).
#'
#' @param K Number of states of the Markov Chain
#' @param prior Named list of the model's priors. If `NULL`, uses the default prior for the model (see [default_prior()]).
#'
#' @details Details of the model are available in the [paper](#).
#'
#' @section Parameters:
#'
#' - `p`: matrix of size K * K where `p[i, j]` represents the transition probabilities from state i to state j.
#'
#' See `list_parameters(model = "MC")` for more details.
#'
#' @section Priors:
#' The priors are passed as a named list with element `p`.
#' The transition probabilities from state i `p[i, ]` are assumed to follow a Dirichlet distribution.
#' The prior should be a matrix where each line correspond to the parameters of the Dirichlet distribution for `p[i, ]`.
#'
#' @section Default priors:
#' The default prior for all `p[i, ]` is a symmetric uniform Dirichlet distribution (all concentration parameters are equal to 1).
#'
#' @name MC
#'
#' @examples
#' EczemaModel("MC", K = 5)
NULL
