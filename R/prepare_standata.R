# Helpers ------------------------------------------------------------------

#' Stop if the dataframe is not a correct input for a longitudinal model
#'
#' - `stopifnot_lgtd_id`: check ID (Patient, Time columns)
#' - `stopifnot_lgtd_dataframe`: valid for both training and testing set
#' - `stopifnot_lgtd_train`: `stopifnot_lgtd_dataframe` + additional check for training set
#' - `stopifnot_lgtd_test`: `stopifnot_lgtd_dataframe` + additional check for testing set
#'
#' NB: the function assumes that:
#' - `df` is a dataframe with no missing values
#' - `max_score` and `discrete` are valid.
#' - in `stopifnot_lgtd_test`, train is supposed to be valid.
#'
#' @param df Dataframe to test
#' @param train Training dataframe
#' @param test Testing dataframe
#' @param max_score Maximum value that the score can take
#' @param discrete Whether the score should take discrete values
#'
#' @return NULL if all statements are TRUE, otherwise an error message
#'
#' @seealso [base::stopifnot()]
#'
#' @name stopifnot_lgtd
#' @noRd
NULL

#' @rdname stopifnot_lgtd
#' @importFrom HuraultMisc is_wholenumber
#' @noRd
stopifnot_lgtd_id <- function(df) {
  stopifnot(is.data.frame(df),
            nrow(df) > 0,
            all(c("Patient", "Time") %in% colnames(df)),
            all(is_wholenumber(df[["Patient"]])),
            all(is_wholenumber(df[["Time"]])),
            all(df[["Time"]] > 0))
}

#' Stop if x is not a valid score input
#'
#' @param x Object to test
#' @param max_score Maximum value that the score can take.
#' If NA, the support of the score is not checked.
#' @param discrete Whether the score should take discrete values
#'
#' @return NULL if all statements are TRUE, otherwise an error message
#'
#' @seealso [base::stopifnot()]
#' @noRd
stopifnot_valid_score <- function(x, max_score = NA, discrete) {

  stopifnot(is.vector(x, mode = "numeric"),
            all(!is.na(x)))

  if (!is.na(max_score)) {
    if (discrete) {
      stopifnot(all(x %in% c(0:max_score)))
    } else {
      stopifnot(all(sapply(x, function(x) {dplyr::between(x, 0, max_score)})))
    }
  }

}

#' @rdname stopifnot_lgtd
#' @noRd
stopifnot_lgtd_dataframe <- function(df, max_score = NA, discrete) {
  stopifnot_lgtd_id(df)
  stopifnot(all("Score" %in% colnames(df)))
  stopifnot_valid_score(df[["Score"]], max_score = max_score, discrete = discrete)
}

#' @rdname stopifnot_lgtd
#' @noRd
#' @import dplyr
stopifnot_lgtd_train <- function(train, max_score, discrete) {
  stopifnot_lgtd_dataframe(train, max_score, discrete)

  pt <- sort(unique(train[["Patient"]]))
  stopifnot(all(pt %in% 1:max(pt)))
  if (max(pt) != length(pt)) {
    warning("Some patients have no observations in the data, you can reconsider reindexing patients.")
  }
}

#' @rdname stopifnot_lgtd
#' @noRd
stopifnot_lgtd_test <- function(test, train, max_score, discrete) {

  stopifnot_lgtd_dataframe(test, max_score, discrete)

  intersection_train_test <- dplyr::intersect(train[, c("Patient", "Time")],
                                              test[, c("Patient", "Time")])
  stopifnot(nrow(intersection_train_test) == 0)

  if (!all(unique(test[["Patient"]]) %in% unique(train[["Patient"]]))) {
    warning("Some patients in test are not in train.")
  }

}

# Prepare data for longitudinal models ------------------------------------

#' @rdname prepare_standata
#'
#' @param max_score Maximum value that the score can take
#' @param discrete Whether to use a discrete normal distribution (only relevant for testing)
#'
#' @export
#' @importFrom HuraultMisc is_scalar_wholenumber
prepare_data_lgtd <- function(train, test = NULL, max_score, discrete) {

  stopifnot(is_scalar_wholenumber(max_score),
            max_score > 0,
            is_scalar(discrete),
            is.logical(discrete))
  stopifnot_lgtd_train(train, max_score, discrete)

  data_stan <- list(
    N_obs = nrow(train),
    N_pt = max(train[["Patient"]]),
    M = max_score,

    k_obs = array(train[["Patient"]]),
    t_obs = array(train[["Time"]]),
    y_obs = array(train[["Score"]]),

    N_test = 0,
    k_test = vector(),
    t_test = vector(),
    y_test = vector()

  )

  if (!is.null(test)) {

    stopifnot_lgtd_test(test, train, max_score, discrete)

    data_stan$N_pt <- max(data_stan$N_pt, max(test[["Patient"]]))

    data_stan$N_test <- nrow(test)
    data_stan$k_test <- array(test[["Patient"]])
    data_stan$t_test <- array(test[["Time"]])
    data_stan$y_test <- array(test[["Score"]])
  }

  return(data_stan)

}

#' @rdname prepare_standata
#' @export
prepare_standata.EczemaModel <- function(model, train, test = NULL, ...) {
  prepare_data_lgtd(train = train,
                    test = test,
                    max_score = model$max_score,
                    discrete = model$discrete) %>%
    add_prior(model$prior)
}

# Prepare data for Markov Chain model -------------------------------------

#' Stop if the dataframe is not a correct input for the Markov Chain model.
#'
#' NB: the function assumes that K is an integer greater than 2.
#'
#' @param df Dataframe to test
#' @param K Number of states of the Markov Chain
#'
#' @return NULL if all statements are TRUE, otherwise an error message
#' @noRd
#'
#' @seealso [base::stopifnot()]
stopifnot_MC_dataframe <- function(df, K) {

  stopifnot(is.data.frame(df),
            all(c("y0", "y1", "dt") %in% colnames(df)),
            all(df[["y0"]] %in% 1:K),
            all(df[["y1"]] %in% 1:K),
            all(is_wholenumber(df[["dt"]])),
            all(df[["dt"]] > 0))

}

#' @rdname prepare_standata
#' @export
prepare_standata.MC <- function(model, train, test = NULL, ...) {

  # NB: here we include run and the prior

  K <- model$K
  stopifnot(is_scalar_wholenumber(K),
            K > 1)
  stopifnot_MC_dataframe(train, K)

  data_stan <- list(
    K = K,

    N = nrow(train),
    y0 = array(train[["y0"]]),
    y1 = array(train[["y1"]]),
    dt =  array(train[["dt"]]),

    # run = 1,

    N_test = 0,
    y0_test = vector(),
    y1_test = vector(),
    dt_test = vector()
  )

  if (!is.null(test)) {
    stopifnot_MC_dataframe(test, K)

    data_stan$N_test <- nrow(test)
    data_stan$y0_test <- array(test[["y0"]])
    data_stan$y1_test <- array(test[["y1"]])
    data_stan$dt_test <- array(test[["dt"]])
  }

  data_stan <- add_prior(data_stan, model$prior)

  return(data_stan)

}
