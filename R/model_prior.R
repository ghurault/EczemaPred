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
#' @export
#' @examples
#' default_prior("BinRW")
default_prior.character <- function(model, max_score = 1, ...) {
  EczemaModel(model, max_score = max_score) %>%
    default_prior()
}

# BinRW -------------------------------------------------------------------

#' @rdname validate_prior
#' @export
validate_prior.BinRW <- function(model) {
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
default_prior.BinRW <- function(model) {
  list(
    sigma = c(0, 0.25 * log(5)),
    mu_logit_y0 = c(0, 1),
    sigma_logit_y0 = c(0, 1.5)
  )
}

#' @rdname print_prior
#' @export
print_prior.BinRW <- function(model, digits = 2) {
  print_distribution("sigma", "normal+", model$prior$sigma, digits = digits)
  print_distribution("mu_logit_y0", "normal", model$prior$mu_logit_y0, digits = digits)
  print_distribution("sigma_logit_y0", "normal", model$prior$sigma_logit_y0, digits = digits)
}

# OrderedRW ---------------------------------------------------------------

#' @rdname validate_prior
#' @export
validate_prior.OrderedRW <- function(model) {
  prior <- model$prior
  stopifnot(is.list(prior),
            length(prior) == 4,
            all(c("delta", "sigma", "mu_y0", "sigma_y0") %in% names(prior)),
            all(sapply(prior, is.numeric)),
            dim(prior$delta_sd) == c(2, model$max_score - 1),
            all(prior$delta[2, ] > 0),
            all(sapply(prior[c("sigma", "mu_y0", "sigma_y0")], function(x) {length(x) == 2})),
            all(sapply(prior[c("sigma", "mu_y0", "sigma_y0")], function(x) {x[2] > 0})))
}

#' @rdname validate_prior
#' @export
#' @examples
#' default_prior(EczemaModel("OrderedRW", max_score = 10))
default_prior.OrderedRW <- function(model) {
  list(
    delta = matrix(rep(c(0, pi / sqrt(3) * 2), model$max_score - 1),
                   nrow = 2, byrow = FALSE),
    sigma = c(0, 0.1),
    mu_y0 = c(0.5, 0.25),
    sigma_y0 = c(0, 0.125)
  )
}

#' @rdname validate_prior
#' @export
print_prior.OrderedRW <- function(model, digits = 2) {
  for (i in 1:(model$max_score - 1)) {
    print_distribution(paste0("delta[", i, "]"), "normal+", model$prior$delta[, i])
  }
  print_distribution("sigma", "normal+", model$prior$sigma, digits = digits)
  print_distribution("mu_y0", "normal", model$prior$mu_y0, digits = digits)
  print_distribution("sigma_y0", "normal", model$prior$sigma_y0, digits = digits)
}
