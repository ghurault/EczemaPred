# Utilities ---------------------------------------------------------------

#' Print distribution
#'
#' Used internally in [print_prior()].
#'
#' @param parameter_name Name of the parameter
#' @param distribution_name Name of the distribution
#' @param arguments Arguments of the distribution
#' @param digits Number of significant digits to print
#'
#' @return None
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
    signif(2) %>%
    paste0(collapse = ",")

  cat("- ", parameter_name, " ~ ", distribution_name, "(", tmp,  ")\n", sep = "")

}

# Methods for class character ---------------------------------------------

#' @rdname default_prior
#'
#' @param max_score Maximum value that the score can take
#' @param K Number of categories
#' @param ... Arguments to pass to [default_prior()] as an EczemaPred object
#'
#' @export
#' @examples
#' default_prior("BinRW")
default_prior.character <- function(model, max_score = 1, K = 2, ...) {
  message("default prior for max_score=", max_score, " (or, when applicable K=", K, ")")
  EczemaModel(model, max_score = max_score, K = K) %>%
    default_prior(...)
}

# BinRW -------------------------------------------------------------------

#' @rdname validate_prior
#' @export
validate_prior.BinRW <- function(model, ...) {
  prior <- model$prior
  stopifnot(
    is.list(prior),
    length(prior) == 3,
    all(c("sigma", "mu_logit_y0", "sigma_logit_y0") %in% names(prior)),
    all(sapply(prior, is.numeric)),
    all(sapply(prior, function(x) {length(x) == 2})),
    all(sapply(prior, function(x) {x[2] > 0}))
  )
}

#' @rdname default_prior
#' @export
#' @examples
#' default_prior(EczemaModel("BinRW", max_score = 10))
default_prior.BinRW <- function(model, ...) {
  list(
    sigma = c(0, 0.25 * log(5)),
    mu_logit_y0 = c(0, 1),
    sigma_logit_y0 = c(0, 1.5)
  )
}

#' @rdname print_prior
#' @export
print_prior.BinRW <- function(model, digits = 2, ...) {
  print_distribution("sigma", "normal+", model$prior$sigma, digits = digits)
  print_distribution("mu_logit_y0", "normal", model$prior$mu_logit_y0, digits = digits)
  print_distribution("sigma_logit_y0", "normal", model$prior$sigma_logit_y0, digits = digits)
}

# OrderedRW ---------------------------------------------------------------

#' @rdname validate_prior
#' @export
validate_prior.OrderedRW <- function(model, ...) {
  prior <- model$prior
  stopifnot(
    is.list(prior),
    length(prior) == 4,
    all(c("delta", "sigma", "mu_y0", "sigma_y0") %in% names(prior)),
    all(vapply(prior, is.numeric, logical(1))),
    dim(prior$delta_sd) == c(2, model$max_score - 1),
    all(prior$delta[2, ] > 0),
    all(vapply(prior[c("sigma", "mu_y0", "sigma_y0")], function(x) {length(x) == 2}, logical(1))),
    all(vapply(prior[c("sigma", "mu_y0", "sigma_y0")], function(x) {x[2] > 0}, logical(1)))
  )
}

#' @rdname default_prior
#' @export
#' @examples
#' default_prior(EczemaModel("OrderedRW", max_score = 10))
default_prior.OrderedRW <- function(model, ...) {
  list(
    delta = matrix(rep(c(0, pi / sqrt(3) * 2), model$max_score - 1),
                   nrow = 2, byrow = FALSE),
    sigma = c(0, 0.1),
    mu_y0 = c(0.5, 0.25),
    sigma_y0 = c(0, 0.125)
  )
}

#' @rdname print_prior
#' @export
print_prior.OrderedRW <- function(model, digits = 2, ...) {
  for (i in 1:(model$max_score - 1)) {
    print_distribution(paste0("delta[", i, "]"), "normal+", model$prior$delta[, i])
  }
  print_distribution("sigma", "normal+", model$prior$sigma, digits = digits)
  print_distribution("mu_y0", "normal", model$prior$mu_y0, digits = digits)
  print_distribution("sigma_y0", "normal+", model$prior$sigma_y0, digits = digits)
}

# BinMC -------------------------------------------------------------------

#' @rdname validate_prior
#' @export
validate_prior.BinMC <- function(model, ...) {
  prior <- model$prior
  stopifnot(
    is.list(prior),
    length(prior) == 4,
    all(c("sigma", "mu_logit_p10", "sigma_logit_p10", "logit_tss1_0") %in% names(prior)),
    all(sapply(prior, is.numeric)),
    all(sapply(prior, function(x) {length(x) == 2})),
    all(sapply(prior, function(x) {x[2] > 0}))
  )
}

#' @rdname default_prior
#' @export
#' @examples
#' default_prior(EczemaModel("BinMC", max_score = 100))
default_prior.BinMC <- function(model, ...) {
  list(
    sigma = c(0, 0.25 * log(5)),
    mu_logit_p10 = c(0, 1),
    sigma_logit_p10 = c(0, 1.5),
    logit_tss1_0 = c(-1, 1)
  )
}

#' @rdname print_prior
#' @export
print_prior.BinMC <- function(model, digits = 2, ...) {
  print_distribution("sigma", "normal+", model$prior$sigma, digits = digits)
  print_distribution("mu_logit_p10", "normal", model$prior$mu_logit_p10, digits = digits)
  print_distribution("sigma_logit_p10", "normal+", model$prior$sigma_logit_p10, digits = digits)
  print_distribution("logit_tss1_0", "normal", model$prior$logit_tss1_0, digits = digits)
}

# RW ----------------------------------------------------------------------

#' @rdname validate_prior
#' @export
validate_prior.RW <- function(model, ...) {
  prior <- model$prior
  stopifnot(
    is.list(prior),
    length(prior) == 1,
    "sigma" %in% names(prior),
    all(is.numeric(prior$sigma)),
    length(prior$sigma) == 2,
    prior$sigma[2] > 0
  )
}

#' @rdname default_prior
#' @export
#' @examples
#' default_prior(EczemaModel("RW", max_score = 100))
default_prior.RW <- function(model, ...) {
  list(sigma = c(0, 0.1))
}

#' @rdname print_prior
#' @export
print_prior.RW <- function(model, digits = 2, ...) {
  print_distribution("sigma / max_score", "normal+", model$prior$sigma, digits = digits)
}

# Smoothing ---------------------------------------------------------------

#' @rdname validate_prior
#' @export
validate_prior.Smoothing <- function(model, ...) {
  prior <- model$prior
  stopifnot(
    is.list(prior),
    length(prior) == 2,
    all(c("sigma", "tau") %in% names(prior)),
    all(sapply(prior, is.numeric)),
    all(sapply(prior, function(x) {length(x) == 2})),
    prior$sigma[2] > 0,
    prior$tau[2] > 0
  )
}

#' @rdname default_prior
#' @export
#' @examples
#' default_prior(EczemaModel("Smoothing", max_score = 100))
default_prior.Smoothing <- function(model, ...) {
  list(
    sigma = c(0, 0.1),
    tau = c(0.5 * log(10), 0.75 * log(10))
  )
}

#' @rdname print_prior
#' @export
print_prior.Smoothing <- function(model, digits = 2, ...) {
  print_distribution("sigma / max_score", "normal+", model$prior$sigma, digits = digits)
  print_distribution("tau", "lognormal", model$prior$tau, digits = digits)
}

# AR1 ---------------------------------------------------------------------

#' @rdname validate_prior
#' @export
validate_prior.AR1 <- function(model, ...) {
  prior <- model$prior
  stopifnot(
    is.list(prior),
    length(prior) == 3,
    all(c("sigma", "alpha", "y_inf") %in% names(prior)),
    all(sapply(prior, is.numeric)),
    all(sapply(prior, function(x) {length(x) == 2})),
    prior$sigma[2] > 0,
    prior$y_inf[2] > 0,
    all(prior$alpha > 0)
  )
}

#' @rdname default_prior
#' @export
#' @examples
#' default_prior(EczemaModel("AR1", max_score = 100))
default_prior.AR1 <- function(model, ...) {
  list(
    sigma = c(0, 0.1),
    alpha = c(1, 1),
    y_inf = c(0.5, 0.25)
  )
}

#' @rdname print_prior
#' @export
print_prior.AR1 <- function(model, digits = 2, ...) {
  print_distribution("sigma / max_score", "normal+", model$prior$sigma, digits = digits)
  print_distribution("alpha", "beta", model$prior$alpha, digits = digits)
  print_distribution("y_inf / max_score", "normal", model$prior$y_inf, digits = digits)
}

# MixedAR1 ----------------------------------------------------------------

#' @rdname validate_prior
#' @export
validate_prior.MixedAR1 <- function(model, ...) {
  prior <- model$prior
  stopifnot(
    is.list(prior),
    length(prior) == 5,
    all(c("sigma", "mu_logit_alpha", "sigma_logit_alpha",
          "mu_inf", "sigma_inf") %in% names(prior)),
    all(sapply(prior, is.numeric)),
    all(sapply(prior, function(x) {length(x) == 2})),
    all(sapply(prior, function(x) {x[2] > 0}))
  )
}

#' @rdname default_prior
#' @export
#' @examples
#' default_prior(EczemaModel("MixedAR1", max_score = 100))
default_prior.MixedAR1 <- function(model, ...) {
  list(
    sigma = c(0, 0.1),
    mu_logit_alpha = c(0, 1),
    sigma_logit_alpha = c(0, 1.5),
    mu_inf = c(0.5, 0.25),
    sigma_inf = c(0, 0.125)
  )
}

#' @rdname print_prior
#' @export
print_prior.MixedAR1 <- function(model, digits = 2, ...) {
  print_distribution("sigma / max_score", "normal+", model$prior$sigma, digits = digits)
  print_distribution("mu_logit_alpha", "normal", model$prior$mu_logit_alpha, digits = digits)
  print_distribution("sigma_logit_alpha", "normal+", model$prior$sigma_logit_alpha, digits = digits)
  print_distribution("mu_inf / max_score", "normal", model$prior$mu_inf, digits = digits)
  print_distribution("sigma_inf / max_score", "normal+", model$prior$mu_inf, digits = digits)
}

# MC ----------------------------------------------------------------------

#' @rdname validate_prior
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

#' @rdname default_prior
#' @export
#' @examples
#' default_prior(EczemaModel("MC", K = 10))
default_prior.MC <- function(model, ...) {
  list(p = matrix(1, nrow = model$K, ncol = model$K))
}

#' @rdname print_prior
#' @export
print_prior.MC <- function(model, digits = 2, ...) {
  for (i in 1:model$K) {
    print_distribution(paste0("p[", i, ", ]"), "dirichlet", model$prior$p[i, ])
  }
}
