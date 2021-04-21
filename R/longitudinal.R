# Generic functions designed for all longitudinal models

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

# Prepare data for Stan ---------------------------------------------------

#' Prepares the data list to pass to the Stan sampler for longitudinal models
#'
#' Used internally.
#'
#' @param train Training dataframe
#' @param test Testing dataframe
#' @param max_score Maximum value that the score can take
#' @param discrete Whether to use a discrete normal distribution (only relevant for testing)
#'
#' @return List to serve as input to the Stan sampler.
#' The list later needs to be appended by the item "discrete" for the RW model, "run" for the other models and
#' priors (see `add_prior()`).
#'
#' @section Dataframe format:
#' `train` and `test` should have the columns `Patient` (patient ID), `Time` (timepoint) and `Score` (score to model).
#' `Patient` should take integer values between 1 and the number of patients in the training set.
#' `Time` should take integer (so discrete) values and starts with one for every patient.
#' `Score` should take values between 0 and max_score.
#' Missing values are not allowed (but Time values are not necessarily consecutive,
#' for example if Score at t=5 is missing, but not at t=4 and t=6, just remove t=5).
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

    k_obs = train[["Patient"]],
    t_obs = train[["Time"]],
    y_obs = train[["Score"]],

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

#' Prepare data list to pass to the Stan sampler for prior predictive checks
#'
#' Not exported
#'
#' @param N_patient Number of patients
#' @param t_max Vector of size N_patient indicating the time-series length
#' @param max_score Maximum value that the score can take
#' @param discrete Whether to use a discrete normal distribution (only relevant for testing)
#'
#' @return List to serve as input to the Stan sampler.
#'
#' @noRd
prepare_priorpred_lgtd <- function(N_patient, t_max, max_score, discrete) {

  stopifnot(is_scalar_wholenumber(N_patient),
            N_patient > 0,
            N_patient == length(t_max),
            all(is_wholenumber(t_max)),
            t_max > 0,
            is_scalar_wholenumber(max_score),
            max_score > 0,
            is_scalar(discrete),
            is.logical(discrete))

  data_stan <- list(
    N_obs = N_patient,
    N_pt = N_patient,
    M = max_score,
    discrete = is.numeric(discrete),

    k_obs = array(1:N_patient),
    t_obs = array(rep(1, N_patient)),
    y_obs = array(stats::runif(N_patient, 0, max_score)),

    # Artificial predictions to define the length of the time-series
    N_test = N_patient,
    k_test = array(1:N_patient),
    t_test = array(t_max),
    y_test = array(rep(0, N_patient)) # does not matter
  )

  if (discrete) {
    data_stan$y_obs <- round(data_stan$y_obs)
  }

  return(data_stan)
}

# get_index ---------------------------------------------------------------

#' Associate (Patient, Time) pairs to corresponding index in the model
#'
#' @param train Training dataframe
#' @param test Testing dataframe
#' @param t_max Vector indicating the length of each patient time-series
#'
#' @return Dataframe with columns Patient, Time, Index
#' @export
#' @import dplyr
#'
#' @details For more details, see the [vignette]()
#'
#' @name get_index
#'
#' @examples
#' library(dplyr)
#' id <- get_index2(t_max = rpois(10, 20))
#' df <- id %>% select(-Index) %>% slice_sample(prop = 0.9) %>% arrange(Patient, Time)
#' get_index(train = df)
NULL

#' @rdname get_index
#' @import dplyr
#' @export
get_index <- function(train, test = NULL) {

  stopifnot_lgtd_id(train)

  if (!is.null(test)) {
    stopifnot_lgtd_id(test)
    train <- bind_rows(train, test)
  }

  full_df <- train %>%
    select(.data$Patient, .data$Time)

  stopifnot(all(!is.na(full_df)))

  tmp <- full_df %>%
    group_by(.data$Patient) %>%
    summarise(t_max = max(.data$Time))

  out <- lapply(1:nrow(tmp),
                function(i) {
                  data.frame(Patient = tmp[i, "Patient"],
                             Time = 1:tmp[i, "t_max"][[1]])
                }) %>%
    bind_rows() %>%
    mutate(Index = 1:n())

  return(out)
}

#' @rdname get_index
#' @export
get_index2 <- function(t_max) {
  get_index(train = data.frame(Patient = 1:length(t_max), Time = t_max), test = NULL)
}

# standata_to_df ----------------------------

#' Convert Stan data list to original dataframe
#'
#' Not exported. Useful to use get_index when we only have data_stan.
#'
#' @param data_stan List (output from [prepare_data_lgtd()])
#'
#' @return Dataframe with columns Patient, Time, Score, Label
#'
#' @noRd
standata_to_df <- function(data_stan) {

  stopifnot(is.list(data_stan),
            all(c("k_obs", "t_obs", "y_obs", "k_test", "t_test", "y_test") %in% names(data_stan)),
            length(data_stan$k_obs) == length(data_stan$t_obs),
            length(data_stan$k_obs) == length(data_stan$y_obs),
            length(data_stan$k_test) == length(data_stan$t_test),
            length(data_stan$k_test) == length(data_stan$y_test))

  out <- with(data_stan,
              data.frame(Patient = c(k_obs, k_test),
                         Time = c(t_obs, t_test),
                         Score = c(y_obs, y_test),
                         Label = c(rep("Training", length(y_obs)),
                                   rep("Testing", length(y_test)))))

  return(out)

}
