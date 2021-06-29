# Pipe --------------------------------------------------------------------

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

# Add prior to stan list ---------------------------------------------------------------

#' Add prior to the list serving as input to the Stan sampler
#'
#' Used internally
#'
#' @param data_stan List
#' @param prior A named list of corresponding to parameters' prior
#' @param prefix Prefix to add to the names of prior
#'
#' @return data_stan with additional items corresponding to the prior.
#'
#' @export
add_prior <- function(data_stan, prior, prefix = "prior_") {

  stopifnot(is.list(data_stan),
            is.list(prior),
            is_scalar(prefix),
            is.character(prefix))

  if (length(prior) > 0) {
    stopifnot(!is.null(names(prior)))
    names(prior) <- paste0(prefix, names(prior))
  }

  return(c(data_stan, prior))
}

# Compiled model ----------------------------------------------------------

#' Get compiled model
#'
#' @param stanmodel Stan model name.
#' NB: this may differ from the name of the model
#'
#' @return Compiled model (object to pass to `rstan::sampling`)
#' @export
get_compiled_model <- function(stanmodel) {
  stopifnot(is_scalar(stanmodel),
            is.character(stanmodel))
  if (!(stanmodel %in% names(stanmodels))) {
    stop("stanmodel should be one of ", paste(stanmodels, collapse = ", "))
  }
  return(stanmodels[[stanmodel]])
}

# Samples to list ---------------------------------------------------------

#' Process samples to a list that can be included to a dataframe
#'
#' @param object Stanfit object containing variable `par_name` or
#' matrix with rows representing samples, columns representing variables.
#' @param n_samples How many samples to return.
#' Default (=NULL) to all samples.
#' @param par_name Name of variable to extract when `object` is a stanfit object.
#'
#' @return List of vector of samples
#'
#' @export
#'
#' @examples
#' samples_to_list(matrix(rnorm(1e3), nrow = 1e2))
samples_to_list <- function(object, par_name = "", n_samples = NULL) {

  if (is_stanfit(object)) {
    stopifnot("y_pred" %in% object@model_pars)
    pred <- rstan::extract(object, pars = "y_pred")[[1]]
  } else {
    pred <- object
  }
  stopifnot(is.matrix(pred))

  if (!is.null(n_samples)) {
    stopifnot(is_scalar_wholenumber(n_samples),
              n_samples > 0)
    pred <- pred[sample(1:nrow(pred), size = n_samples, replace = TRUE), ]
  }
  out <- lapply(1:ncol(pred), function(i) {pred[, i]})

  return(out)

}
