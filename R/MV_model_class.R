# need to write:
# - sample_prior
# - EczemaFit
#
# And need to mention that indexing in `extract_parameters does not work

# Constructor -------------------------------------------------------------

#' EczemaMVModel constructor
#'
#' @param model_name Name of the model to create
#' @param max_score Maximum value that the scores can take
#' @param D Number of components
#' @param independent_components Whether to have diagonal correlations matrices or not.
#' In other words, whether changes in latent scores are correlated,
#' or whether the components are indepedent.
#' @param prior Named list of the model's priors.
#' It uses the default priors (see [default_prior()]) if `NULL` and for the parameters that are not provided.
#'
#' @return An object of class `model_name` and EczemaMVModel
#'
#' @export
#'
#' @examples
#' EczemaMVModel("BinMRW", max_score = 10, D = 5)
EczemaMVModel <- function(model_name = c("BinMRW", "OrderedMRW"), max_score, D, independent_components = FALSE, prior = NULL) {

  model_name <- match.arg(model_name)

  stopifnot(HuraultMisc::is_scalar_wholenumber(max_score),
            max_score > 1,
            HuraultMisc::is_scalar_wholenumber(D),
            D > 0,
            HuraultMisc::is_scalar(independent_components),
            is.logical(independent_components))

  x <- structure(
    list(
      name = model_name,
      stanmodel = paste0(model_name, ".stan"),
      max_score = max_score,
      D = D,
      discrete = TRUE,
      independent_components = independent_components
    ),
    class = c(model_name, "EczemaMVModel")
  )

  x$prior <- default_prior(x)
  x <- replace_prior(x, prior = prior)
  validate_prior(x)

  return(x)

}

# Methods -----------------------------------------------------------------

# validate_prior.EczemaMVModel <- validate_prior.EczemaModel

# print_prior.EczemaMVModel <- validate_prior.EczemaModel

#' Print model
#'
#' @param x Object
#' @param digits Number of significants digits to print
#' @param ... Arguments to pass to other methods
#'
#' @return None
#'
#' @export
print.EczemaMVModel <- function(x, digits = 2, ...) {

  cat(x$name, " model", sep = "")
  if ("discrete" %in% names(x)) {
    cat(" (", ifelse(x$discrete, "discrete", "continuous"), ")", sep = "")
  }
  cat("\n", sep = "")

  if ("D" %in% names(x)) {
    cat(x$D, "components \n")
  }

  if ("max_score" %in% names(x)) {
    cat("max_score =", x$max_score, "\n")
  }

  cat("Prior: \n")
  print_prior(x, digits = digits, ...)

}

#' Prepare data list to pass to the Stan sampler
#'
#' @param model Object
#' @param train Training dataframe
#' @param test Testing dataframe
#' @param ... Arguments to pass to other methods
#'
#' @return Named list
#'
#' @export
prepare_standata.EczemaMVModel <- function(model, train, test = NULL, ...) {

  stopifnot(all(train[["Component"]] %in% 1:model$D))

  out <- prepare_data_lgtd(train = train, test = test, max_score = model$max_score, discrete = TRUE)

  out$D <- model$D
  out$d_obs <- train[["Component"]]
  out$d_test <- vector()

  if (!is.null(test)) {
    stopifnot(all(test[["Component"]] %in% 1:model$D))
    out$d_test <- array(test[["Component"]])
  }

  out <- out %>%
    add_prior(model$prior) %>%
    c(list(independent_components = as.numeric(model$independent_components)))

  return(out)

}
