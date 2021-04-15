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
#' @param model Stan model name
#'
#' @return Compiled model (object to pass to `rstan::sampling`)
#' @export
get_compiled_model <- function(model) {
  stopifnot(model %in% names(stanmodels))
  return(stanmodels[[model]])
}

# Extract simulations -----------------------------------------------------

#' Extract simulations
#'
#' @param fit Stanfit object
#' @param id Dataframe linking index in obj to (Patient, Time) pairs, cf. output from [get_index()]
#' @param draw Draw ID
#' @param pars Vector of parameters to extract. Default to all parameters except `y_rep`.
#'
#' @return Named list:
#' - Data: dataframe with columns Patient, Time, Index, and Score corresponding to simulations
#' - Parameters: dataframe containing parameters used to generate the data (cf. [HuraultMisc::extract_draws()])
#'
#' @import dplyr
#' @export
extract_simulations <- function(fit, id, draw, pars = NULL) {

  stopifnot(is_stanfit(fit),
            is.data.frame(id),
            all(c("Patient", "Time", "Index") %in% colnames(id)),
            is_scalar_wholenumber(draw),
            draw > 0)

  if (is.null(pars)) {
    pars <- setdiff(fit@model_pars, "y_rep")
  } else {
    stopifnot(is.vector(pars, mode = "character"))
  }
  param <- rstan::extract(fit, pars = pars) %>%
    HuraultMisc::extract_draws(draws = draw)

  yrep <- rstan::extract(fit, pars = "y_rep")[[1]]
  df <- mutate(id, Score = yrep[draw, ])

  return(list(Data = df,
              Parameters = param))
}
