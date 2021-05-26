# Note: every S3 methods must be exported, even if the generic is not

# Constructor ------------------------------------------------------------

#' EczemaModel constructor
#'
#' @param model_name Name of the model to create
#' @param max_score Maximum value that the score can take
#' @param discrete Whether the model is discrete or not
#' @param K Number of categories (only used when `model_name == "MC"`)
#' @param prior Prior for the model. If `NULL`, uses the default prior for the model (see [default_prior()])
#'
#' @return An object (list) of class `model_name` and EczemaModel
#' @export
#'
#' @examples
#' mdl <- EczemaModel("BinRW", max_score = 10)
EczemaModel <- function(model_name = c("BinRW"),
                        max_score,
                        discrete = (model_name %in% c("BinRW")),
                        K = ifelse(discrete, max_score + 1, NA),
                        prior = NULL) {

  model_name <- match.arg(model_name)

  if (!is.null(max_score)) {
    stopifnot((is_scalar_wholenumber(max_score) & max_score > 0) || (is_scalar(max_score) & is.na(max_score)))
  }
  stopifnot(is_scalar(discrete),
            is.logical(discrete))

  x <- structure(
    list(name = model_name,
         stanmodel = model_name,
         max_score = max_score,
         discrete = discrete),
    class = c(model_name, "EczemaModel")
  )
  if (discrete && !is.null(K)) {
    stopifnot((is_scalar_wholenumber(K) & K > 0) || (is_scalar(K) & is.na(K)))
    x$K <- K
  }

  if (is.null(prior)) {
    prior <- default_prior(x)
  }
  x$prior <- prior
  validate_prior(x)

  return(x)

}

# New generics ----------------------------------------------------------------

#' Default prior
#'
#' @param model Object of class EczemaModel
#'
#' @return Named list of parameters' priors.
#' For more details, see the generic of the model class.
#'
#' @export
default_prior <- function(model) {
  UseMethod("default_prior")
}

#' Check the prior of an EczemaModel is correct
#'
#' @param model Object of class EczemaModel
#'
#' @return NULL if all statements are TRUE, otherwise an error message
validate_prior <- function(model) {
  UseMethod("validate_prior")
}

#' Print prior distribution
#'
#' @param model Object of class EczemaModel
#' @param ... Arguments to pass to other methods
#'
#' @return Invisible
print_prior <- function(model, ...) {
  UseMethod("print_prior")
}

#' Fit an EczemaModel
#'
#' @param model Object of class EczemaModel
#' @param train Training dataframe
#' @param test Testing dataframe
#' @param ... Arguments to pass to other methods
#'
#' @return Object of class stanfit
#' @export
EczemaFit <- function(model, train, test, ...) {
  UseMethod("EczemaFit")
}

#' Prior predictive distribution
#'
#' @param model Object of class EczemaModel
#' @param ... Arguments to pass to other methods
#'
#' @return Object of class stanfit
#' @export
sample_prior <- function(model, ...) {
  UseMethod("sample_prior")
}

# make_standata?

# Base methods for EczemaModel object ------------------------------------------

#' Print model
#'
#' @param model Object of class EczemaModel
#' @param digits Number of significant digits to print
#'
#' @return Invisible
#' @export
print.EczemaModel <- function(model, digits = 2) {

  cat(model$name, " model (", ifelse(model$discrete, "discrete", "continuous"), ") \n", sep = "")
  cat("- max_score =", model$max_score, "\n")
  if (model$discrete) {
    cat("-", model$K, "categories \n")
  }

  cat("Prior: \n")
  print_prior(model, digits = digits)

}
