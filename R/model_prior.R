# Utilities ---------------------------------------------------------------

#' Print distribution
#'
#' @param distribution_name Name of the distribution
#' @param parameters Parameter values
#' @param digits Number of significant digits to print
#'
#' @return None
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
  print_distribution("sigma", "normal+", model$prior$sigma)
  print_distribution("mu_logit_y0", "normal", model$prior$mu_logit_y0)
  print_distribution("sigma_logit_y0", "normal", model$prior$sigma_logit_y0)
}
