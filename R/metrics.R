# Extract metric ---------------------------------------------------------------------

#' Extract lpd (predictive log-likelihood) and RPS from stanfit object
#'
#' The metrics are computed for the expected forecast distribution.
#' The lpd is defined for continuous and discrete outcomes.
#' The RPS is defined for discrete outcomes only and is computed by
#' extracting the cumulative error distribution (`cum_err`: cumulative forecast - cumulative distribution),
#' taking its expected value (cf. expected forecast), squaring it and apply,
#' summing over possible outcomes and normalising by the number of outcomes `- 1`.
#'
#' @param fit Stanfit object
#' @param par_name Name of the parameter to parameter in the Stan model.
#' Usually `lpd`, `log_lik` (for the log likelihood of the data) or `cum_err`.
#'
#' @return Vector of lpd/RPS for each prediction
#'
#' @name extract_metric
NULL

#' @export
#' @rdname extract_metric
extract_loglikelihood <- function(fit, par_name = "log_lik") {
  stopifnot(is_stanfit(fit)) # presence of `par_name` checked in rstan::extract
  lpd_mat <- rstan::extract(fit, pars = par_name)[[1]]
  stopifnot(length(dim(lpd_mat)) == 2)
  # Taking expectation of probability (mean(exp)) and use log scoring rule
  apply(lpd_mat, 2, function(x) {log(mean(exp(x)))})
}

#' @export
#' @rdname extract_metric
extract_lpd <- function(fit) {
  extract_loglikelihood(fit, par_name = "lpd")
}

#' @export
#' @rdname extract_metric
extract_RPS <- function(fit, par_name = "cum_err") {
  stopifnot(is_stanfit(fit)) # presence of `par_name` checked in rstan::extract
  cum_err_mat <- rstan::extract(fit, pars = par_name)[[1]]
  stopifnot(length(dim(cum_err_mat)) == 3)
  cum_err <- apply(cum_err_mat, c(2, 3), mean) # Taking expectation
  apply(cum_err, 1, function(x) {sum(x^2) / (length(x) - 1)})
}

# Add metrics -------------------------------------------------------------

#' Append lpd and (C)RPS to (test) dataframe
#'
#' - [add_metrics1_d()] and [add_metrics1_c()] extracts the lpd and RPS from the Stanfit object
#' - [add_metrics2_d()] and [add_metrics2_c()] calculates the lpd and (C)RPS from the empirical pmf
#' - The metrics in [add_metrics2_c()] and the CRPS of [add_metrics1_c()] are
#' calculated using the `scoringRules` package.
#'
#' @param df Dataframe to add the metrics to
#' - For [add_metrics1_c()], it must contain a column "Score".
#' - For [add_metrics2_c()] and [add_metrics2_d()], it must contain the columns "Samples" and "Score".
#' @param fit Stanfit object with parameters "lpd", and for [add_metrics1_d()] "cum_err".
#' @param support Support of the distribution
#' @param add_samples Numeric vector used to initialise the distribution when computing the lpd and (C)RPS.
#' For example, this can be used to add a uniform distribution to the vector of samples,
#' to avoid problems at the tail of the distribution.
#' If `NULL`, the empirical pmf is not changed.
#' Default to the uniform distribution (i.e. `support`) for [add_metrics2_d()] and `NULL` for [add_metrics2_c()].
#' The column "Samples" is not modified when `add_samples` is not NULL.
#' @param bw Bandwidth, for calculating lpd, see [scoringRules::logs_sample()].
#' Useful to set the "resolution" of the distribution.
#'
#' @return Dataframe `df` appended by the columns "lpd", "RPS"
#' (or "CRPS" for [add_metrics1_c()] and [add_metrics2_d()]).
#'
#' @seealso [extract_lpd()], [extract_RPS()], [HuraultMisc::compute_RPS()],
#' [scoringRules::logs_sample()], [scoringRules::crps_sample()].
#'
#' @import dplyr
#'
#' @name add_metrics
NULL

#' @export
#' @rdname add_metrics
add_metrics1_d <- function(df, fit) {

  stopifnot(is.data.frame(df),
            is_stanfit(fit))

  out <- df %>%
    mutate(lpd = extract_lpd(fit),
           RPS = extract_RPS(fit))

  return(out)

}

#' @export
#' @rdname add_metrics
add_metrics1_c <- function(df, fit) {

  stopifnot(is.data.frame(df),
            is_stanfit(fit),
            "Score" %in% colnames(df),
            is.vector(df[["Score"]], mode = "numeric"),
            "y_pred" %in% fit@model_pars)

  pred <- rstan::extract(fit, pars = "y_pred")[[1]]
  out <- df %>%
    mutate(lpd = extract_lpd(fit),
           CRPS = scoringRules::crps_sample(df[["Score"]], t(pred)))

  return(out)

}

#' @export
#' @rdname add_metrics
add_metrics2_d <- function(df, support, add_samples = support) {

  stopifnot(is.data.frame(df),
            all(c("Samples", "Score") %in% colnames(df)),
            is.numeric(support),
            length(support) > 1)

  if (!is.null(add_samples)) {
    stopifnot(is.numeric(add_samples),
              all(add_samples %in% support))
  }

  prob <- do.call(rbind,
                  lapply(1:nrow(df),
                         function(i) {
                           HuraultMisc::extract_distribution(c(add_samples, df$Samples[[i]]),
                                                             type = "discrete",
                                                             support = support)$Probability
                         }))
  tmp <- vapply(1:nrow(prob),
                function(i) {
                  score_id <- which(abs(support - df$Score[i]) < sqrt(.Machine$double.eps)) # HuraultMisc::approx_equal
                  c(log(prob[i, score_id]),
                    HuraultMisc::compute_RPS(prob[i, ], score_id))
                },
                numeric(2))

  df <- df %>%
    mutate(lpd = tmp[1, ],
           RPS = tmp[2, ])

  return(df)

}

#' @export
#' @rdname add_metrics
add_metrics2_c <- function(df, add_samples = NULL, bw = NULL) {

  stopifnot(is.data.frame(df),
            all(c("Samples", "Score") %in% colnames(df)),
            length(unique(vapply(df[["Samples"]], length, numeric(1)))) == 1)

  pred <- do.call(rbind, df[["Samples"]])

  if (!is.null(add_samples)) {
    stopifnot(length(add_samples) > 0,
              is.vector(add_samples, mode = "numeric"))
    pred0 <- matrix(rep(add_samples, nrow(df)), nrow = nrow(df), byrow = TRUE)
    pred <- cbind(pred0, pred)
  }

  if (!is.null(bw)) {
    stopifnot(is_scalar(bw),
              is.numeric(bw))
    bw <- rep(bw, nrow(df))
    stop("Specifying bw is disabled because of a bug in the scoringRules package.")
  }

  df <- df %>%
    mutate(lpd = -scoringRules::logs_sample(df[["Score"]], pred, bw = bw),
           CRPS = scoringRules::crps_sample(df[["Score"]], pred))

  return(df)

}
