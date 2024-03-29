# Forward chaining --------------------------------------------------------

#' Forward chaining
#'
#' In a forward chaining setting, we retrain/update the model every `horizon` day/week/months/etc.
#' and test the model on the next `horizon` day/week/months/etc., i.e. the data that is included at the next training step.
#' We call this training/testing steps iterations and start at iteration 0 where only the first timepoint is only included.
#' At iteration 1, timepoints 1 to `1 + horizon` are included.
#'
#' - `get_fc_iteration` associate a vector of timepoints `t` to the corresponding iteration.
#' - `split_fc_dataset` split dataset `df` into a training and testing set,
#' and computes prediction horizon and last available score in the test set.
#' - `get_fc_training_iteration` identify training iterations such as test set is not empty
#' - `detail_fc_training` derive training data characteristics for each iteration in `df`,
#' including the number of training observations, the proportion of training observations on the total number of observations,
#' and the maximum timepoint in the training sets.
#'
#' @param t Vector of timepoints
#' @param horizon Updating horizon
#' @param df Dataframe with columns `Patient`, `Time`, `Horizon`, `Iteration.`
#' Only the columns Time is required for `detail_fc_training`.
#' @param it Iteration number
#' @param it_test Vector of testing iteration numbers
#'
#' @return
#' - `get_fc_iteration` returns a vector corresponding to iteration numbers
#' - `split_fc_dataset` returns a named list of dataframes (`Training` and `Testing`)
#' - `get_fc_training_iteration` returns a vector of unique training iteration number
#' - `detail_fc_training` returns a dataframe with columns: `Iteration`, `N`, `Proportion`, `LastTime`
#'
#' @details
#' Time=t is in Iteration=i means that:
#' - Time=t is in the new training data of iteration i
#' - Time=t is not in the training data of iterations < i
#' - Time=t is in the testing data of iteration i-1
#' - Time=t is in the training data of iterations >= i
#'
#' @name forward_chaining
#'
#' @examples
#' h <- 2
#' df <- get_index2(t_max = rpois(10, 10))
#' df$Score <- rnorm(nrow(df))
#' df$Iteration <- get_fc_iteration(df$Time, h)
#' sp <- split_fc_dataset(df, 1)
#' train_it <- get_fc_training_iteration(df$Iteration)
#' fc_char <- detail_fc_training(df, h)
NULL

#' @rdname forward_chaining
#' @export
get_fc_iteration <- function(t, horizon) {
  stopifnot(is_wholenumber(t),
            all(t > 0),
            is_scalar_wholenumber(horizon),
            horizon >= 1)
  return((t - 2) %/% horizon + 1)
}

#' @rdname forward_chaining
#' @export
#' @import dplyr tidyr
split_fc_dataset <- function(df, it) {

  stopifnot(is.data.frame(df),
            all(c("Patient", "Time", "Score", "Iteration") %in% colnames(df)),
            is_scalar_wholenumber(it),
            it >= 0)

  train <- filter(df, .data$Iteration <= it)
  test <- filter(df, .data$Iteration == it + 1)

  # Compute prediction horizon
  last_obs <- train %>%
    group_by(.data$Patient) %>%
    filter(.data$Time == max(.data$Time)) %>%
    ungroup() %>%
    rename(LastTime = .data$Time, LastScore = .data$Score)
  test <- full_join(last_obs,
                    test %>% select(-.data$Iteration),
                    by = "Patient") %>%
    drop_na() %>%
    mutate(Horizon = .data$Time - .data$LastTime)

  return(list(Training = train, Testing = test))
}

#' @rdname forward_chaining
#' @export
#' @import dplyr
detail_fc_training <- function(df, horizon) {

  stopifnot(is.data.frame(df),
            all(c("Time") %in% colnames(df)),
            is_scalar_wholenumber(horizon),
            horizon >= 1)

  out <- df %>%
    mutate(Iteration = get_fc_iteration(.data$Time, horizon)) %>%
    group_by(.data$Iteration) %>%
    summarise(N = n()) %>%
    arrange(.data$Iteration) %>%
    mutate(N = cumsum(.data$N),
           Proportion = .data$N / max(.data$N)) %>%
    filter(.data$Iteration < max(.data$Iteration))

  last_time <- tibble(Time = 1:max(df[["Time"]]),
                      Iteration = get_fc_iteration(.data$Time, horizon)) %>%
    group_by(.data$Iteration) %>%
    summarise(LastTime = max(.data$Time))

  out <- left_join(out, last_time, by = "Iteration")

  return(out)
}

#' @rdname forward_chaining
#' @export
get_fc_training_iteration <- function(it_test) {
  stopifnot(all(is_wholenumber(it_test)))
  x <- NULL
  tibble(x = it_test) %>%
    distinct() %>%
    filter(x > 0) %>%
    mutate(x = x - 1) %>%
    pull() %>%
    sort()
}

# Uniform forecast --------------------------------------------------------

#' @rdname add_uniform_pred
#' @noRd
#' @import dplyr
add_discrete_uniform_pred <- function(test, max_score, include_samples, n_samples) {

  p0 <- rep(1 / (max_score + 1), max_score + 1)
  RPS_dict <- vapply(0:max_score,
                     function(x) {HuraultMisc::compute_RPS(p0, x + 1)},
                     numeric(1))

  out <- test %>%
    mutate(lpd = -log(max_score + 1),
           RPS = RPS_dict[.data$Score + 1])

  if (include_samples) {
    if (is.null(n_samples)) {
      smp <- lapply(1:nrow(test), function(i) {0:max_score})
    } else {
      smp <- lapply(1:nrow(test), function(i) {sample(0:max_score, size = n_samples, replace = TRUE)})
    }
    out <- mutate(out, Samples = smp)
  }

  return(out)
}

#' @rdname add_uniform_pred
#' @noRd
#' @import dplyr
add_continuous_uniform_pred <- function(test, max_score, include_samples, n_samples) {

  out <- test %>%
    mutate(lpd = -log(max_score),
           CRPS = scoringRules::crps_unif(test[["Score"]], min = 0, max = max_score))

  if (include_samples) {
    if (is.null(n_samples)) {
      smp <- lapply(1:nrow(test), function(i) {0:max_score})
    } else {
      smp <- lapply(1:nrow(test), function(i) {stats::runif(n_samples, 0, max_score)})
    }
    out <- mutate(out, Samples = smp)
  }

  return(out)
}

#' Performance of a uniform forecast
#'
#' @param test Testing dataframe. The only requirements is that it contains a column "Score".
#' @param max_score Maximum value that the score can take
#' @param discrete Whether to estimate a discrete or continuous historical forecast
#' @param include_samples Whether to return samples from the historical forecast in the output
#' @param n_samples If include_samples=TRUE, how many samples to return.
#' When NULL, the samples `0:max_score` are returned.
#'
#' @return Dataframe `test` appended by the columns "lpd", "RPS" (or CRPS if discrete=FALSE) and optionally "Samples"
#' @export
#'
#' @examples
#' max_score <- 100
#' test <- data.frame(Score = rbinom(1e2, max_score, 0.5))
#' add_uniform_pred(test, max_score)
add_uniform_pred <- function(test, max_score, discrete = TRUE, include_samples = FALSE, n_samples = NULL) {

  stopifnot(is.data.frame(test),
            all(c("Score") %in% colnames(test)),
            is_scalar_wholenumber(max_score),
            max_score > 0,
            is_scalar(discrete),
            is.logical(discrete),
            is_scalar(include_samples),
            is.logical(include_samples))
  stopifnot_valid_score(test[["Score"]], max_score = max_score, discrete = discrete)
  if (include_samples && !is.null(n_samples)) {
    stopifnot(is_scalar_wholenumber(n_samples),
              n_samples > 0)
  }

  if (discrete) {
    out <- add_discrete_uniform_pred(test = test,
                                     max_score = max_score,
                                     include_samples = include_samples,
                                     n_samples = n_samples)
  } else {
    out <- add_continuous_uniform_pred(test = test,
                                       max_score = max_score,
                                       include_samples = include_samples,
                                       n_samples = n_samples)
  }

  return(out)
}

# Historical performance --------------------------------------------------

#' Performance of (population) historical forecast
#'
#' @param test Testing dataframe. The only requirements is that it contains a column "Score".
#' @param train Training dataframe. The only requirements is that it contains a column "Score".
#' @param max_score Maximum value that the score can take
#' @param discrete Whether to estimate a discrete or continuous historical forecast
#' @param add_uniform Whether to include samples from uniform distribution when computing a discrete historical forecast.
#' This ensures that all states are visited.
#' @param include_samples Whether to return samples from the historical forecast in the output
#' @param n_samples If `include_samples=TRUE`, how many samples to return. When NULL, the function return the training set.
#'
#' @details
#' The continuous historical forecast is calculated by considering the training set as samples from the predictive distribution.
#'
#' @return Dataframe `test` appended by the columns "lpd", "RPS" (or CRPS if discrete=FALSE) and optionally "Samples"
#' @export
#'
#' @examples
#' max_score <- 100
#' train <- data.frame(Score = rbinom(1e2, max_score, 0.2))
#' test <- data.frame(Score = rbinom(1e2, max_score, 0.5))
#' add_historical_pred(test, train, max_score)
add_historical_pred <- function(test,
                                train,
                                max_score,
                                discrete = TRUE,
                                add_uniform = TRUE,
                                include_samples = FALSE,
                                n_samples = NULL) {

  for (df in list(train, test)) {
    stopifnot(is.data.frame(df),
              "Score" %in% colnames(df))
    stopifnot_valid_score(df[["Score"]], max_score = max_score, discrete = discrete)
  }
  stopifnot(is_scalar_wholenumber(max_score),
            max_score > 0,
            is_scalar(discrete),
            is.logical(discrete),
            is_scalar(include_samples),
            is.logical(include_samples),
            is_scalar(add_uniform),
            is.logical(add_uniform))

  if (discrete) {
    p0 <- train[["Score"]]
    if (add_uniform) {
      p0 <- c(0:max_score, p0)
    }
    p0 <- factor(p0, levels = 0:max_score)
    p0 <- as.numeric(table(p0) / length(p0))
    RPS_dict <- vapply(0:max_score,
                       function(x) {HuraultMisc::compute_RPS(p0, x + 1)},
                       numeric(1))

    out <- test %>%
      mutate(lpd = log(p0[.data$Score + 1]),
             RPS = RPS_dict[.data$Score + 1])
  } else {
    p0 <- train[["Score"]]
    pred <- matrix(p0, nrow = nrow(test), ncol = nrow(train), byrow = TRUE)
    out <- test %>%
      mutate(lpd = -scoringRules::logs_sample(test[["Score"]], pred),
             CRPS = scoringRules::crps_sample(test[["Score"]], pred))
  }

  if (include_samples) {
    if (is.null(n_samples)) {
      smp <- lapply(1:nrow(test), function(i) {train[["Score"]]})
    } else {
      stopifnot(is_scalar_wholenumber(n_samples),
                n_samples > 0)
      smp <- lapply(1:nrow(test), function(i) {sample(p0, size = n_samples, replace = TRUE)})
    }
    out <- mutate(out, Samples = smp)
  }

  return(out)

}

# Compute performance -----------------------------------------------------

#' Append lpd, (C)RPS and predictive samples to (test) dataframe
#'
#' @param df Dataframe.
#' When `discrete = FALSE`, it must contain a column "Score".
#' @param fit Stanfit object
#' @param discrete Whether to estimate a discrete or continuous forecast.
#' For a discrete forecast, the RPS will be computed and the CRPS for a continuous forecast.
#' @param include_samples Whether to return samples from the historical forecast in the output
#' @param n_samples If `include_samples=TRUE`, how many samples to return. Default (=NULL) to all samples.
#'
#' @return Dataframe `df` appended by the columns "lpd", "RPS" (or CRPS if `discrete=FALSE`) and optionally "Samples"
#'
#' @seealso [add_metrics1_d()] and [add_metrics1_c()]
#'
#' @export
add_predictions <- function(df, fit, discrete = TRUE, include_samples = FALSE, n_samples = NULL) {

  stopifnot(is_scalar(include_samples),
            is.logical(include_samples))

  if (discrete) {
    df <- add_metrics1_d(df = df, fit = fit)
  } else {
    df <- add_metrics1_c(df = df, fit = fit)
  }

  if (include_samples) {
    df <- mutate(df, Samples = samples_to_list(fit, par_name = "y_pred", n_samples = n_samples))
  }

  return(df)

}
