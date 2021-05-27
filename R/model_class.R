# Note: every S3 methods must be exported, even if the generic is not

# Constructor ------------------------------------------------------------

#' EczemaModel constructor
#'
#' @param model_name Name of the model to create
#' @param max_score Maximum value that the score can take. Required for all models except "MC".
#' @param K Number of categories. Only required for "MC" model.
#' @param discrete Whether the model is discrete or not. Only required for "RW".
#' @param prior Named list of the model's priors. If `NULL`, uses the default prior for the model (see [default_prior()]).
#'
#' @return An object (list) of class `model_name` and EczemaModel
#' @export
#'
#' @examples
#' EczemaModel("BinRW", max_score = 10)
EczemaModel <- function(model_name = c("BinRW", "OrderedRW", "BinMC", "RW", "Smoothing", "AR1", "MixedAR1", "MC"),
                        max_score = NULL,
                        K = NULL,
                        discrete = TRUE,
                        prior = NULL) {

  model_name <- match.arg(model_name)

  model_spec <- list(name = model_name,
                     stanmodel = model_name)

  if (model_name %in% c("BinRW", "OrderedRW", "BinMC", "MC")) {
    discrete <- TRUE
  } else if (model_name %in% c("Smoothing", "AR1", "MixedAR1")) {
    discrete <- FALSE
  } else if (model_name %in% c("RW")) {
    stopifnot(is_scalar(discrete),
              is.logical(discrete))
  }
  model_spec$discrete <- discrete

  if (model_name %in% c("BinRW", "OrderedRW", "BinMC", "RW", "Smoothing", "AR1", "MixedAR1")) {
    if (is.null(max_score)) {
      stop("max_score must be supplied for ", model_name)
    } else {
      # NB: max_score must be a wholenumber even if discrete=FALSE
      stopifnot(is_scalar_wholenumber(max_score),
                max_score > 0)
      model_spec$max_score <- max_score
    }
  }

  if (model_name %in% c("MC")) {
    if (is.null(K)) {
      stop("K must be supplied for ", model_name)
    } else {
      stopifnot(is_scalar_wholenumber(K),
                K > 1)
      model_spec$K <- K
    }
  }

  x <- structure(model_spec,
                 class = c(model_name, "EczemaModel"))

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
#' @param model Object
#' @param ... Arguments to pass to other methods
#'
#' @return Named list of parameters' priors.
#' For more details, see the generic of the model class.
#'
#' @export
default_prior <- function(model, ...) {
  UseMethod("default_prior")
}

#' Check the prior of an EczemaModel is correct
#'
#' Used internally when constructing an EczemaModel.
#'
#' @param model Object
#' @param ... Arguments to pass to other methods
#'
#' @return NULL if all statements are TRUE, otherwise an error message
#'
#' @seealso [base::stopifnot()]
#'
#' @export
validate_prior <- function(model, ...) {
  UseMethod("validate_prior")
}

#' Print prior distribution
#'
#' Used internally in the `print.EczemaModel` method.
#'
#' @param model Object
#' @param digits Number of significant digits to print
#' @param ... Arguments to pass to other methods
#'
#' @return None
#'
#' @export
print_prior <- function(model, ...) {
  UseMethod("print_prior")
}

#' List available parameters
#'
#' @param model Object
#' @param main Whether to output the main parameters only.
#' @param ... Arguments to pass to other methods
#'
#' @return Named list of parameters names, grouped into broad categories:
#' - Population: population parameters (i.e. patient- and time-independent)
#' - Patient: patient-dependent parameters
#' - PatientTime: patient- and time-dependent parameters (e.g. latent scores)
#' - Test: parameters related to the test set
#' - Misc: other parameters
#'
#' @details
#' See [MC], [BinRW], [BinMC], [OrderedRW], [RW], [Smoothing], [AR1] and [MixedAR1] for details about the model-specific parameters.
#' Other parameters are available across models:
#'
#' - `y_rep` correspond to posterior replications. To get the corresponding index, use [get_index()].
#' - `y_pred` is a subset of y_rep corresponding to test samples (size `N_test` equal to the number of observations in the test set).
#' - `lpd` is the log predictive density of test samples (of size `N_test`).
#' - `cum_err` is the cumulative error distribution, only available for discrete outcomes
#' (matrix with dimensions `N_test * (max_score  + 1)`).
#'
#' @export
list_parameters <- function(model, ...) {
  UseMethod("list_parameters")
}

# See when we would include MC
if (FALSE) {
  prepare_standata <- function(model, train, test, ...) {
    UseMethod("prepare_standata")
  }
}

#' Fit an EczemaModel
#'
#' @param model Object
#' @param train Training dataframe (details of the format in [prepare_data_lgtd()])
#' @param test Testing dataframe (details of the format in [prepare_data_lgtd()])
#' @param ... Arguments to pass to other methods
#'
#' @return Stanfit object
#' @export
EczemaFit <- function(model, train, test, ...) {
  UseMethod("EczemaFit")
}

#' Prior predictive distribution
#'
#' @param model Object
#' @param ... Arguments to pass to other methods
#'
#' @return Object of class stanfit
#' @export
sample_prior <- function(model, ...) {
  UseMethod("sample_prior")
}

# Base methods for EczemaModel object ------------------------------------------

#' Print model
#'
#' @param model Object of class EczemaModel
#' @param digits Number of significant digits to print
#'
#' @return None
#' @export
print.EczemaModel <- function(model, digits = 2, ...) {

  cat(model$name, " model", sep = "")
  if ("discrete" %in% names(model)) {
    cat(" (", ifelse(model$discrete, "discrete", "continuous"), ")", sep = "")
  }
  cat("\n", sep = "")

  if ("max_score" %in% names(model)) {
    cat("- max_score =", model$max_score, "\n")
  }
  if ("K" %in% names(model)) {
    cat("-", model$K, "categories \n")
  }

  cat("Prior: \n")
  print_prior(model, digits = digits)

}
