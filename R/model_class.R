# Note: every S3 methods must be exported, even if the generic is not

# Constructor ------------------------------------------------------------

#' EczemaModel constructor
#'
#' @param model_name Name of the model to create
#' @param max_score Maximum value that the score can take. Required for all models except "MC".
#' @param K Number of categories. Only required for "MC" model.
#' @param discrete Whether the model is discrete or not. Only required for "RW".
#' @param prior Named list of the model's priors.
#' It uses the default priors (see [default_prior()]) if `NULL` and for the parameters that are not provided.
#'
#' @return An object (list) of class `model_name` and EczemaModel, with elements:
#' - `model_name`: Name of the model
#' - `stanmodel`: Name of the Stan model.
#' Used internally to locate the compiled code.
#' It can also be used to store the filepath of the Stan code.
#' - `discrete`: Whether the model is discrete or not.
#' - `max_score`: Maximum value that the score can take (when applicable)
#' - `K`: Number of categories (when applicable)
#' - `prior`: List of parameters' priors
#'
#' @export
#'
#' @examples
#' EczemaModel("BinRW", max_score = 10)
EczemaModel <- function(model_name = c("BinRW", "OrderedRW", "BinMC", "RW", "Smoothing", "AR1", "MixedAR1", "MC"),
                        max_score = NULL,
                        K = NULL,
                        discrete = FALSE,
                        prior = NULL) {

  model_name <- match.arg(model_name)

  model_spec <- list(name = model_name,
                     stanmodel = model_name)
  if (model_name %in% c("RW", "Smoothing")) {
    model_spec$stanmodel <- "Smoothing"
  }

  if (model_name %in% c("BinRW", "OrderedRW", "BinMC", "MC")) {
    discrete <- TRUE
  } else if (model_name %in% c("AR1", "MixedAR1")) {
    discrete <- FALSE
  } else {
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
                max_score > 0,
                max_score > 1 || model_name != "OrderedRW")
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

  x$prior <- default_prior(x)
  x <- replace_prior(x, prior = prior)
  validate_prior(x)

  return(x)

}

#' Replace prior
#'
#' Used internally to overwrite default prior in the constructor.
#' Beware that the validity of the new prior is not tested in this function,
#' you may want to call [validate_prior()] after using this function.
#'
#' @param x EczemaModel object
#' @param prior Named list of the model's prior to replace. If NULL, the prior stays the same
#'
#' @return Object of the same class as `x`
#' @export
#'
#' @examples
#' model <- EczemaModel("OrderedRW", max_score = 5)
#' print(model)
#' replace_prior(model, prior = list(sigma = c(0, 1)))
replace_prior <- function(x, prior = NULL) {

  if (!is.null(prior)) {
    stopifnot(is.list(prior),
              length(names(prior)) == length(unique(names(prior))))

    old_prior <- x$prior
    x$prior <- NULL

    replaced_pars <- intersect(names(prior), names(old_prior))
    unused_pars <- setdiff(names(prior), replaced_pars)
    if (length(unused_pars) > 0) {
      warning("The following parameters do not exist or their priors do not need to be specified: ",
              paste(unused_pars, collapse = ","))
    }

    old_prior[replaced_pars] <- NULL
    new_prior <- c(old_prior, prior[replaced_pars])

    x$prior <- new_prior
  }

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
#'
#' @examples
#' default_prior(EczemaModel("BinRW", max_score = 10))
#' default_prior(EczemaModel("MC", K = 10))
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
#'
#' @examples
#' model <- EczemaModel("BinRW", max_score = 10)
#' print_prior(model)
#' print_prior(model, digits = 5)
print_prior <- function(model, ...) {
  UseMethod("print_prior")
}

#' List available parameters
#'
#' @param model Object
#' @param main Whether to output the main parameters only (when applicable).
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
#'
#' @examples
#' list_parameters("RW")
#' list_parameters(EczemaModel("RW", max_score = 100))
list_parameters <- function(model, ...) {
  UseMethod("list_parameters")
}

#' Prepare the data list to pass to the Stan sampler
#'
#' Used internally.
#'
#' @param model Object
#' @param train Training dataframe (details of the format in [EczemaFit()])
#' @param test Testing dataframe (details of the format in [EczemaFit()])
#' @param ... Arguments to pass to other methods
#'
#' @return List to serve as input to the Stan sampler.
#' The list is usually incomplete needs to be optional parameters, such as:
#' - `run` (binary, for main and MC models, indicating whether to evaluate the likelihood)
#'
#' @details
#' - `prepare_data_lgtd` is helps build `prepare_standata.EczemaModel` and is kept for compatibility reasons.
#' The list that it outputs does not include the priors.
#'
#' @export
prepare_standata <- function(model, train, test, ...) {
  UseMethod("prepare_standata")
}

#' Fit an EczemaModel
#'
#' @param model Object
#' @param train Training dataframe (see details below)
#' @param test Testing dataframe (see details below)
#' @param ... Arguments to pass to other methods
#'
#' @return Stanfit object
#'
#' @section Data format:
#'
#' ## All models except "MC"
#'
#' - `train` and `test` should have the columns `Patient` (patient ID), `Time` (timepoint) and `Score` (score to model).
#' - `Patient` should take integer values between 1 and the number of patients in the training set.
#' - `Time` should take integer (so discrete) values and starts with one for every patient.
#' - `Score` should take values between 0 and max_score.
#' - Missing values are not allowed (but Time values are not necessarily consecutive,
#' for example if Score at t=5 is missing, but not at t=4 and t=6, just remove t=5).
#'
#' ## "MC" model
#'
#' - `train` and `test` should have columns `y0` (for the current state), `y1` (for the next state) and
#' `dt` (for the time delay between states).
#' - `y0` and `y1` should take integer values between 1 and K.
#' - `dt` should take integer values greater than or equal to 1.
#' - Missing values are not allowed.
#'
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
#' @param x Object of class EczemaModel
#' @param digits Number of significant digits to print
#' @param ... Arguments to pass to [print_prior()]
#'
#' @return None
#' @export
print.EczemaModel <- function(x, digits = 2, ...) {

  cat(x$name, " model", sep = "")
  if ("discrete" %in% names(x)) {
    cat(" (", ifelse(x$discrete, "discrete", "continuous"), ")", sep = "")
  }
  cat("\n", sep = "")

  if ("max_score" %in% names(x)) {
    cat("max_score =", x$max_score, "\n")
  }
  if ("K" %in% names(x)) {
    cat(x$K, "categories \n")
  }

  cat("Prior: \n")
  print_prior(x, digits = digits, ...)

}

#' @export
print_prior.EczemaModel <- function(model, ...) {
  print(model$prior)
}

#' @export
validate_prior.EczemaModel <- function(model, ...) {
  message("Using the validate_prior method for EczemaModel object")
  stopifnot(
    is.list(model$prior)
  )
}
