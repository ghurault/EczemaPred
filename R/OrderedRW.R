# Documentation -----------------------------------------------------------

#' Ordered Logistic random walk model
#'
#' This is a state-space model defined by a Ordered logistic measurement error distribution and a latent random walk.
#'
#' - `default_prior_OrderedRW` returns the default prior
#' - `fit_OrderedRW` fits the Ordered logistic random walk model
#' - `sample_prior_OrderedRW` samples the prior predictive distribution
#'
#' @param train Training dataframe (details of the format in [prepare_data_lgtd()])
#' @param test Testing dataframe (details of the format in [prepare_data_lgtd()])
#' @param max_score Maximum value that the score can take
#' @param prior A named list with elements `delta`, `sigma`, `mu_y0` and `sigma_y0`
#' specifying priors for the corresponding parameters.
#'
#' The element `delta` should be a matrix with 2 rows and `max_score - 1` columns,
#' such as the i-th column is a vector with values x1 and x2, where x2 > 0 and
#' `delta[i] ~ normal(x1, x2)`.
#' The other parameters are normalised by the difference between the highest and lowest cutpoints (approx. the range of the score),
#' and their priors are defined by a vector of length 2, containing values for x1 and x2, x2 > 0, such as:
#'
#' - `sigma ~ normal(x1, x2)`
#' - `mu_y0 ~ normal(x1, x2)`
#' - `sigma_y0 ~ normal(x1, x2)`
#'
#' NB: `delta`, `sigma` and `sigma_y0` are constrained to be positive so x1 are usually set to 0 to define a half-normal distribution.
#'
#' @param ... Arguments to be pass to [rstan::sampling()]
#' @param N_patient Number of patients
#' @param t_max Vector of size N_patient indicating the time-series length
#'
#' @return
#' - `default_prior_OrderedRW`: named list containing the default prior for `delta`
#' - `fit_OrderedRW`: Stanfit object
#' - `sample_prior_OrderedRW`: Stanfit object
#'
#' @section Model's parameters:
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
#' @details
#' - The default prior for `delta` is set so that `delta` is less than the width of the logistic distribution.
#' - The default prior for `sigma` assumes it would be to go to a state where `y = 0` is the most likely outcome to
#' a state where `y = M` in two transitions.
#' - The default priors for `mu_y0` and `sigma_y0` have reasonable ranges and translate to an approximately uniform prior
#' over the range of the score for `y0`.
#' - For more details see the [vignette]().
#'
#' @name OrderedRW
NULL

# Prior ---------------------------------------------------------------

#' Stop if the prior input is not valid for the Ordered random Walk model
#'
#' @param prior A named list of corresponding to parameters' prior. See [default_prior_OrderedRW()].
#' @param max_score Maximum value that the score can take
#'
#' @return NULL if all statements are TRUE, otherwise an error message
#'
#' @seealso [base::stopifnot()]
#'
#' @noRd
stopifnot_prior_OrderedRW <- function(prior, max_score) {
  stopifnot(is.list(prior),
            length(prior) == 4,
            all(c("delta", "sigma", "mu_y0", "sigma_y0") %in% names(prior)),
            all(sapply(prior, is.numeric)),
            dim(prior$delta_sd) == c(2, max_score - 1),
            all(prior$delta[2, ] > 0),
            all(sapply(prior[c("sigma", "mu_y0", "sigma_y0")], function(x) {length(x) == 2})),
            all(sapply(prior[c("sigma", "mu_y0", "sigma_y0")], function(x) {x[2] > 0})))
}

#' @rdname OrderedRW
#' @export
default_prior_OrderedRW <- function(max_score) {
  list(
    delta = matrix(rep(c(0, pi / sqrt(3) * 2), max_score - 1),
                   nrow = 2, byrow = FALSE),
    sigma = c(0, 0.1),
    mu_y0 = c(0.5, 0.25),
    sigma_y0 = c(0, 0.125)
  )
}

# Fit ---------------------------------------------------------------------

#' @rdname OrderedRW
#' @export
fit_OrderedRW <- function(train, test = NULL, max_score, prior = default_prior_OrderedRW(max_score), ...) {
  fit_discrete(train = train, test = test, max_score = max_score, model = "OrderedRW", prior = prior, ...)
}

#' @rdname OrderedRW
#' @export
sample_prior_OrderedRW <- function(N_patient = 1, t_max = c(2), max_score, prior = default_prior_OrderedRW(max_score), ...) {
  sample_prior_discrete(N_patient = N_patient, t_max = t_max, max_score = max_score, model = "OrderedRW", prior = prior, ...)
}

# Plot latent score -------------------------------------------------------

#' Plot the evolution of the expected latent score of the OrderedRW model
#'
#' @param fit Stanfit object
#' @param id Dataframe linking index in fit to (Patient, Time) pairs, cf. output from [get_index()]
#' @param patient_id Patient ID
#'
#' @return Ggplot
#' - Horizontal lines correspond to the expected cut-offs
#' - Ribbons correspond to the CI of a logistic distribution
#'
#' @import ggplot2 dplyr
#'
#' @export
plot_latent_OrderedRW <- function(fit, id, patient_id) {

  stopifnot(is_stanfit(fit),
            all(c("y_lat", "ct") %in% fit@model_pars),
            is.data.frame(id),
            nrow(id) == fit@par_dims[["y_lat"]],
            all(c("Patient", "Time", "Index") %in% colnames(id)),
            patient_id %in% unique(id[["Patient"]]))

  # Extract mean latent score and cutpoints
  id1 <- filter(id, .data$Patient == patient_id)
  df <- mutate(id1, Mean = rstan::extract(fit, pars = paste0("y_lat[", id1[["Index"]], "]")) %>%
                 sapply(mean))
  ct <- rstan::extract(fit, pars = "ct")[[1]] %>%
    apply(2, mean)

  max_score <- length(ct)
  lvl <- seq(0.1, 0.9, 0.1)

  # Label location
  midpoint <- (ct - lag(ct))[-1] / 2
  midpoint <- ct[1:length(midpoint)] + midpoint
  midpoint <- c(ct[1] - 3.5, midpoint, ct[length(ct)] + 3.5)

  # Dataset containing CI of different levels
  ssi <- lapply(lvl,
                function(CI) {
                  z <- stats::qlogis(0.5 + CI / 2)
                  out <- mutate(df, Lower = .data$Mean - z, Upper = .data$Mean + z, Level = CI)
                  return(out)
                }) %>%
    bind_rows()

  p <- plot_fanchart(ssi) +
    geom_hline(yintercept = ct) +
    geom_label(data = data.frame(Label = paste0("y = ", 0:max_score), x = 1, y = midpoint),
               aes_string(x = "x", y = "y", label = "Label"), hjust = 0) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(y = "Expected latent score")

  return(p)
}
