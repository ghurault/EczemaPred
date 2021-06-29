# List parameters ---------------------------------------------------------

#' @export
list_parameters.character <- function(model, ...) {
  structure(list(name = model),
            class = c(model)) %>%
    list_parameters(...)
}

#' @rdname list_parameters
#'
#' @importFrom HuraultMisc is_scalar
#' @export
#' @examples
#' list_parameters(EczemaModel("BinRW", max_score = 100))
list_parameters.BinRW <- function(model, main = TRUE, ...) {

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
list_parameters.OrderedRW <- function(model, main = TRUE, ...) {

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
list_parameters.BinMC <- function(model, main = TRUE, ...) {

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

#' @export
list_parameters.RW <- function(model, ...) {
  list(Population = "sigma",
       PatientTime = "y_rep",
       Test = c("y_pred", "lpd", "cum_err"),
       Misc = "y_mis")
}

#' @export
list_parameters.Smoothing <- function(model, ...) {
  list(Population = c("sigma", "tau", "alpha"),
       PatientTime = c("L", "y_rep"),
       Test = c("y_pred", "lpd"),
       Misc = "y_mis")
}

#' @export
list_parameters.AR1 <- function(model, ...) {
  list(Population = c("sigma", "slope", "intercept", "y_inf"),
       PatientTime = "y_rep",
       Test = c("y_pred", "lpd"),
       Misc = "y_mis")
}

#' @export
list_parameters.MixedAR1 <- function(model, ...) {
  list(Population = c("sigma", "mu_logit_slope", "sigma_logit_slope", "mu_inf", "sigma_inf"),
       Patient = c("slope", "y_inf", "intercept"),
       PatientTime = c("y_rep"),
       Test = c("y_pred", "lpd"),
       Misc = "y_mis")
}

#' @export
list_parameters.MC <- function(model, ...) {
  list(Population = "p",
       Observation  = "y_rep",
       Test = c("y_pred", "lpd", "cum_err"))
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
