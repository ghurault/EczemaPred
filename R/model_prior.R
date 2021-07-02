# Utilities ---------------------------------------------------------------

#' @rdname print_prior
#'
#' @param parameter_name Name of the parameter
#' @param distribution_name Name of the distribution
#' @param arguments Arguments of the distribution (numeric vector)
#' @param digits Number of significant digits to print (cf. [base::signif()])
#'
#' @export
#'
#' @examples
#' print_distribution("x", "normal", c(0, 1))
print_distribution <- function(parameter_name, distribution_name, arguments, digits = 2) {

  stopifnot(is_scalar(parameter_name),
            is.character(parameter_name),
            is_scalar(distribution_name),
            is.character(distribution_name),
            is.vector(arguments, mode = "numeric"))

  tmp <- arguments %>%
    signif(digits = digits) %>%
    paste0(collapse = ",")

  cat("- ", parameter_name, " ~ ", distribution_name, "(", tmp,  ")\n", sep = "")

}

# Methods for class character ---------------------------------------------

#' @param max_score Maximum value that the score can take
#' @param K Number of categories
#'
#' @export
#'
#' @examples
#' default_prior("BinRW")
#'
#' @describeIn default_prior The function creates an EczemaModel object and call the corresponding method.
default_prior.character <- function(model, max_score = 1, K = 2, ...) {
  message("default prior for max_score=", max_score, " (or, when applicable K=", K, ")")
  EczemaModel(model, max_score = max_score, K = K) %>%
    default_prior(...)
}

# BinRW -------------------------------------------------------------------

#' @export
validate_prior.BinRW <- function(model, ...) {
  prior <- model$prior
  stopifnot(
    is.list(prior),
    all(c("sigma", "mu_logit_y0", "sigma_logit_y0") %in% names(prior)),
    all(vapply(prior, is.numeric, logical(1))),
    all(vapply(prior, function(x) {length(x) == 2}, logical(1))),
    all(vapply(prior, function(x) {x[2] > 0}, logical(1)))
  )
}

#' @export
default_prior.BinRW <- function(model, ...) {
  list(
    sigma = c(0, 0.25 * log(5)),
    mu_logit_y0 = c(0, 1),
    sigma_logit_y0 = c(0, 1.5)
  )
}

#' @export
print_prior.BinRW <- function(model, digits = 2, ...) {
  print_distribution("sigma", "normal+", model$prior$sigma, digits = digits)
  print_distribution("mu_logit_y0", "normal", model$prior$mu_logit_y0, digits = digits)
  print_distribution("sigma_logit_y0", "normal", model$prior$sigma_logit_y0, digits = digits)
}

# OrderedRW ---------------------------------------------------------------

#' @export
validate_prior.OrderedRW <- function(model, ...) {
  prior <- model$prior
  stopifnot(
    is.list(prior),
    all(c("delta", "sigma_meas", "sigma_lat", "mu_y0", "sigma_y0") %in% names(prior)),
    all(vapply(prior, is.numeric, logical(1))),
    length(prior$delta) == model$max_score - 1,
    all(prior$delta > 0),
    all(vapply(prior[c("sigma_meas", "sigma_lat", "mu_y0", "sigma_y0")], function(x) {length(x) == 2}, logical(1))),
    all(vapply(prior[c("sigma_meas", "sigma_lat", "mu_y0", "sigma_y0")], function(x) {x[2] > 0}, logical(1)))
  )
}

#' @export
default_prior.OrderedRW <- function(model, ...) {
  list(
    delta = rep(2, model$max_score - 1),
    sigma_meas = c(-log(10), 0.5 * log(4)),
    sigma_lat = c(-log(10), 0.5 * log(4)),
    mu_y0 = c(0.5, 0.25),
    sigma_y0 = c(0, 0.125)
  )
}

#' @export
print_prior.OrderedRW <- function(model, digits = 2, ...) {
  print_distribution("delta", "dirichlet", model$prior$delta, digits = digits)
  print_distribution("sigma_meas / max_score", "lognormal", model$prior$sigma_meas, digits = digits)
  print_distribution("sigma_lat / max_score", "lognormal", model$prior$sigma_lat, digits = digits)
  print_distribution("mu_y0 / max_score", "normal", model$prior$mu_y0, digits = digits)
  print_distribution("sigma_y0 / max_score", "normal+", model$prior$sigma_y0, digits = digits)
}

# BinMC -------------------------------------------------------------------

#' @export
validate_prior.BinMC <- function(model, ...) {
  prior <- model$prior
  stopifnot(
    is.list(prior),
    all(c("sigma", "mu_logit_p10", "sigma_logit_p10", "logit_tss1_0") %in% names(prior)),
    all(vapply(prior, is.numeric, logical(1))),
    all(vapply(prior, function(x) {length(x) == 2}, logical(1))),
    all(vapply(prior, function(x) {x[2] > 0}, logical(1)))
  )
}

#' @export
default_prior.BinMC <- function(model, ...) {
  list(
    sigma = c(0, 0.25 * log(5)),
    mu_logit_p10 = c(0, 1),
    sigma_logit_p10 = c(0, 1.5),
    logit_tss1_0 = c(-1, 1)
  )
}

#' @export
print_prior.BinMC <- function(model, digits = 2, ...) {
  print_distribution("sigma", "normal+", model$prior$sigma, digits = digits)
  print_distribution("mu_logit_p10", "normal", model$prior$mu_logit_p10, digits = digits)
  print_distribution("sigma_logit_p10", "normal+", model$prior$sigma_logit_p10, digits = digits)
  print_distribution("logit_tss1_0", "normal", model$prior$logit_tss1_0, digits = digits)
}

# RW ----------------------------------------------------------------------

#' @export
validate_prior.RW <- function(model, ...) {
  prior <- model$prior
  stopifnot(
    is.list(prior),
    "sigma" %in% names(prior),
    all(is.numeric(prior$sigma)),
    length(prior$sigma) == 2,
    prior$sigma[2] > 0
  )
}

#' @export
default_prior.RW <- function(model, ...) {
  list(sigma = c(0, 0.1))
}

#' @export
print_prior.RW <- function(model, digits = 2, ...) {
  print_distribution("sigma / max_score", "normal+", model$prior$sigma, digits = digits)
}

# Smoothing ---------------------------------------------------------------

#' @export
validate_prior.Smoothing <- function(model, ...) {
  prior <- model$prior
  stopifnot(
    is.list(prior),
    all(c("sigma", "tau") %in% names(prior)),
    all(vapply(prior, is.numeric, logical(1))),
    all(vapply(prior, function(x) {length(x) == 2}, logical(1))),
    prior$sigma[2] > 0,
    prior$tau[2] > 0
  )
}

#' @export
default_prior.Smoothing <- function(model, ...) {
  list(
    sigma = c(0, 0.1),
    tau = c(0.5 * log(10), 0.75 * log(10))
  )
}

#' @export
print_prior.Smoothing <- function(model, digits = 2, ...) {
  print_distribution("sigma / max_score", "normal+", model$prior$sigma, digits = digits)
  print_distribution("tau", "lognormal", model$prior$tau, digits = digits)
}

# AR1 ---------------------------------------------------------------------

#' @export
validate_prior.AR1 <- function(model, ...) {
  prior <- model$prior
  stopifnot(
    is.list(prior),
    all(c("sigma", "slope", "y_inf") %in% names(prior)),
    all(vapply(prior, is.numeric, logical(1))),
    all(vapply(prior, function(x) {length(x) == 2}, logical(1))),
    prior$sigma[2] > 0,
    prior$y_inf[2] > 0,
    all(prior$slope > 0)
  )
}

#' @export
default_prior.AR1 <- function(model, ...) {
  list(
    sigma = c(0, 0.1),
    slope = c(1, 1),
    y_inf = c(0.5, 0.25)
  )
}

#' @export
print_prior.AR1 <- function(model, digits = 2, ...) {
  print_distribution("sigma / max_score", "normal+", model$prior$sigma, digits = digits)
  print_distribution("slope", "beta", model$prior$slope, digits = digits)
  print_distribution("y_inf / max_score", "normal", model$prior$y_inf, digits = digits)
}

# MixedAR1 ----------------------------------------------------------------

#' @export
validate_prior.MixedAR1 <- function(model, ...) {
  prior <- model$prior
  stopifnot(
    is.list(prior),
    all(c("sigma", "mu_logit_slope", "sigma_logit_slope",
          "mu_inf", "sigma_inf") %in% names(prior)),
    all(vapply(prior, is.numeric, logical(1))),
    all(vapply(prior, function(x) {length(x) == 2}, logical(1))),
    all(vapply(prior, function(x) {x[2] > 0}, logical(1)))
  )
}

#' @export
default_prior.MixedAR1 <- function(model, ...) {
  list(
    sigma = c(0, 0.1),
    mu_logit_slope = c(0, 1),
    sigma_logit_slope = c(0, 1.5),
    mu_inf = c(0.5, 0.25),
    sigma_inf = c(0, 0.125)
  )
}

#' @export
print_prior.MixedAR1 <- function(model, digits = 2, ...) {
  print_distribution("sigma / max_score", "normal+", model$prior$sigma, digits = digits)
  print_distribution("mu_logit_slope", "normal", model$prior$mu_logit_slope, digits = digits)
  print_distribution("sigma_logit_slope", "normal+", model$prior$sigma_logit_slope, digits = digits)
  print_distribution("mu_inf / max_score", "normal", model$prior$mu_inf, digits = digits)
  print_distribution("sigma_inf / max_score", "normal+", model$prior$mu_inf, digits = digits)
}

# MC ----------------------------------------------------------------------

#' @export
validate_prior.MC <- function(model, ...) {
  prior <- model$prior
  stopifnot(
    is.list(prior),
    "p" %in% names(prior),
    is.matrix(prior$p),
    dim(prior$p) == c(model$K, model$K),
    all(prior$p > 0)
  )
}

#' @export
default_prior.MC <- function(model, ...) {
  list(p = matrix(1, nrow = model$K, ncol = model$K))
}

#' @export
print_prior.MC <- function(model, digits = 2, ...) {
  for (i in 1:model$K) {
    print_distribution(paste0("p[", i, ", ]"), "dirichlet", model$prior$p[i, ])
  }
}
