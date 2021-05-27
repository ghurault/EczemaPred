# List parameters ---------------------------------------------------------

if (FALSE) {

  #' List available parameters
  #'
  #' @param model Model name
  #' @param main Whether to output the main parameters only.
  #' Parameters that are simple transformation of another parameter are dropped.
  #' For instance, in BinRW, `logit_lat` would be dropped but `y_lat` would remain.
  #'
  #' @return Named list of parameters names, grouped into broad categories:
  #' - Population: population parameters (i.e. patient- and time-independent)
  #' - Patient: patient-dependent parameters
  #' - PatientTime: patient- and time-dependent parameters (e.g. latent scores)
  #' - Test: parameters related to the test set
  #' - Misc: other parameters
  #'
  #' @export
  #' @importFrom HuraultMisc is_scalar
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
  #' @examples
  #' list_parameters("BinRW")
  list_parameters <- function(model = c("MC", "BinRW", "BinMC", "OrderedRW", "RW", "Smoothing", "AR1", "MixedAR1"),
                              main = TRUE) {

    model <- match.arg(model)
    stopifnot(is_scalar(main),
              is.logical(main))

    if (model == "MC") {
      out <- list(Population = "p",
                  Observation  = "y_rep",
                  Test = c("y_pred", "lpd", "cum_err"))
    }
    if (model == "BinRW") {
      out <- list(Population = c("sigma", "mu_logit_y0", "sigma_logit_y0"),
                  Patient = "logit_y0",
                  PatientTime =  c("y_lat", "logit_lat", "y_rep"),
                  Test = c("y_pred", "lpd", "cum_err"))
      if (main) {
        out$PatientTime <- setdiff(out$PatientTime, "logit_lat")
      }
    }
    if (model == "BinMC") {
      out <- list(Population = c("mu_logit_p10", "sigma_logit_p10", "sigma"),
                  Patient = c("p10", "logit_p10", "logit_tss1_0"),
                  PatientTime = c("p01", "lambda", "ss1", "y_lat", "y_rep"),
                  Test = c("y_pred", "lpd", "cum_err"))
      if (main) {
        out$Patient <- setdiff(out$Patient, c("logit_p10", "logit_tss1_0"))
      }
    }
    if (model == "OrderedRW") {
      out <- list(Population = c("sigma", "mu_y0", "sigma_y0", "p0", "ct", "delta"),
                  Patient = "y0",
                  PatientTime = c("y_lat", "y_rep"),
                  Test = c("y_pred", "lpd", "cum_err"))
      if (main) {
        out$Population <- setdiff(out$Population, "p0")
      }
    }
    if (model == "RW") {
      out <- list(Population = "sigma",
                  PatientTime = "y_rep",
                  Test = c("y_pred", "lpd", "cum_err"),
                  Misc = "y_mis")
    }
    if (model == "Smoothing") {
      out <- list(Population = c("sigma", "tau", "alpha"),
                  PatientTime = c("L", "y_rep"),
                  Test = c("y_pred", "lpd"),
                  Misc = "y_mis")
    }
    if (model == "AR1") {
      out <- list(Population = c("sigma", "alpha", "b", "y_inf"),
                  PatientTime = "y_rep",
                  Test = c("y_pred", "lpd"),
                  Misc = "y_mis")
    }
    if (model == "MixedAR1") {
      out <- list(Population = c("sigma", "mu_logit_alpha", "sigma_logit_alpha", "mu_inf", "sigma_inf"),
                  Patient = c("alpha", "y_inf", "b"),
                  PatientTime = c("y_rep"),
                  Test = c("y_pred", "lpd"),
                  Misc = "y_mis")
    }

    return(out)

  }

}

#' @rdname list_parameters
#' @export
#' @examples
#' list_parameters("BinRW")
list_parameters.character <- function(model, ...) {
  structure(list(name = model),
            class = c(model)) %>%
    list_parameters(...)
}

#' @rdname list_parameters
#' @importFrom HuraultMisc is_scalar
#' @export
#' @examples
#' list_parameters(EczemaModel("BinRW", max_score = 100))
list_parameters.BinRW <- function(model, main = TRUE) {

  stopifnot(is_scalar(main),
            is.logical(main))

  out <- list(Population = c("sigma", "mu_logit_y0", "sigma_logit_y0"),
              Patient = "logit_y0",
              PatientTime =  c("y_lat", "logit_lat", "y_rep"),
              Test = c("y_pred", "lpd", "cum_err"))
  if (main) {
    out$PatientTime <- setdiff(out$PatientTime, "logit_lat")
  }

  return(out)

}

#' @rdname list_parameters
#' @importFrom HuraultMisc is_scalar
#' @export
#' @examples
#' list_parameters(EczemaModel("OrderedRW", max_score = 100))
list_parameters.OrderedRW <- function(model, main = TRUE) {

  stopifnot(is_scalar(main),
            is.logical(main))

  out <- list(Population = c("sigma", "mu_y0", "sigma_y0", "p0", "ct", "delta"),
              Patient = "y0",
              PatientTime = c("y_lat", "y_rep"),
              Test = c("y_pred", "lpd", "cum_err"))
  if (main) {
    out$Population <- setdiff(out$Population, "p0")
  }

  return(out)

}

#' @rdname list_parameters
#' @importFrom HuraultMisc is_scalar
#' @export
#' @examples
#' list_parameters(EczemaModel("BinMC", max_score = 100))
list_parameters.BinMC <- function(model, main = TRUE) {

  stopifnot(is_scalar(main),
            is.logical(main))

  out <- list(Population = c("mu_logit_p10", "sigma_logit_p10", "sigma"),
              Patient = c("p10", "logit_p10", "logit_tss1_0"),
              PatientTime = c("p01", "lambda", "ss1", "y_lat", "y_rep"),
              Test = c("y_pred", "lpd", "cum_err"))
  if (main) {
    out$Patient <- setdiff(out$Patient, c("logit_p10", "logit_tss1_0"))
  }

  return(out)

}

#' @rdname list_parameters
#' @export
#' @examples
#' list_parameters(EczemaModel("RW", max_score = 100))
list_parameters.RW <- function(model) {
  list(Population = "sigma",
       PatientTime = "y_rep",
       Test = c("y_pred", "lpd", "cum_err"),
       Misc = "y_mis")
}

#' @rdname list_parameters
#' @export
#' @examples
#' list_parameters(EczemaModel("Smoothing", max_score = 100))
list_parameters.Smoothing <- function(model) {
  list(Population = c("sigma", "tau", "alpha"),
       PatientTime = c("L", "y_rep"),
       Test = c("y_pred", "lpd"),
       Misc = "y_mis")
}

#' @rdname list_parameters
#' @export
#' @examples
#' list_parameters(EczemaModel("AR1", max_score = 100))
list_parameters.AR1 <- function(model) {
  list(Population = c("sigma", "alpha", "b", "y_inf"),
       PatientTime = "y_rep",
       Test = c("y_pred", "lpd"),
       Misc = "y_mis")
}

#' @rdname list_parameters
#' @export
#' @examples
#' list_parameters(EczemaModel("MixedAR1", max_score = 100))
list_parameters.MixedAR1 <- function(model) {
  list(Population = c("sigma", "mu_logit_alpha", "sigma_logit_alpha", "mu_inf", "sigma_inf"),
       Patient = c("alpha", "y_inf", "b"),
       PatientTime = c("y_rep"),
       Test = c("y_pred", "lpd"),
       Misc = "y_mis")
}

# Extract parameters ------------------------------------------------------

#' Extract parameters posterior summary statistics
#'
#' @param fit Stanfit object
#' @param pars Named list of parameters to extract. See [list_parameters()].
#' @param id Dataframe associating Index to (Patient, Time) pairs. See [get_index()].
#' @param ... Arguments to pass to [HuraultMisc::summary_statistics()].
#'
#' @return Tibble dataframe containing posterior summary statistics.
#' See details in [HuraultMisc::summary_statistics()]. Additional columns Patient and Time if `id` is not NULL.
#'
#' @export
#' @import dplyr
#' @importFrom HuraultMisc is_stanfit
extract_parameters <- function(fit, pars = NULL, id = NULL, ...) {

  stopifnot(is_stanfit(fit),
            is.list(pars),
            all(is.character(unlist(pars))))

  par <- HuraultMisc::summary_statistics(fit, pars = unlist(pars), ...)

  if ("Patient" %in% names(pars)) {
    par <- bind_rows(
      filter(par, !(.data$Variable %in% pars$Patient)),
      par %>%
        filter(.data$Variable %in% pars$Patient) %>%
        mutate(Patient = .data$Index)
    )
  }

  if ("PatientTime" %in% names(pars) && !is.null(id)) {
    stopifnot(is.data.frame(id),
              all(c("Index", "Patient", "Time") %in% colnames(id)))

    par <- bind_rows(
      filter(par, !(.data$Variable %in% pars$PatientTime)),
      par %>%
        filter(.data$Variable %in% pars$PatientTime) %>%
        mutate(Patient = NULL) %>%
        left_join(id, by = "Index")
    )
  }

  par <- par %>%
    # mutate(Variable = factor(Variable, levels = unlist(pars))) %>%
    arrange(.data$Variable, .data$Index)

  return(par)
}
