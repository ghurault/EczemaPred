# Helpers -----------------------------------------------------------------

is_vector_or_array <- function(x) {is.vector(x) | is.array(x)}

check_format <- function(ds) {
  # Args: ds: List to be passed to the stan sampler
  input_names <- c("N_obs", "N_pt", "M", "k_obs", "t_obs", "y_obs", "N_test", "k_test", "t_test", "y_test")

  expect_true(is.list(ds))
  expect_true(all(input_names %in% names(ds)))
  expect_true(is_vector_or_array(ds$k_obs))
  expect_true(is_vector_or_array(ds$t_obs))
  expect_true(is_vector_or_array(ds$y_obs))
  expect_true(is_vector_or_array(ds$k_test))
  expect_true(is_vector_or_array(ds$t_test))
  expect_true(is_vector_or_array(ds$y_test))
}

make_wrong_df <- function(df, max_score) {
  # Assuming df is a correct train/test input, makes a list of inputs derived from df that are incorrect

  list(
    as.list(df),
    filter(df, Score > max_score),
    rename(df, k = Patient),
    rename(df, t = Time),
    rename(df, y = Score),
    mutate(df, Score = replace(Score, Score == min(Score), NA)),
    mutate(df, Patient = Patient + rbeta(n(), 2, 2)),
    mutate(df, Time = Time + rbeta(n(), 2, 2)),
    mutate(df, Time = Time - 10),
    mutate(df, Score = Score - min(Score) - 0.5 * max_score),
    mutate(df, Score = Score - max(Score) + max_score + 1)
  )

}

# Test prepare_data_lgtd and prepare_stan.EczemaModel --------------------------------------------------

max_score <- 100
discrete <- TRUE
df <- expand_grid(Patient = 1:10, Time = 1:10) %>%
  mutate(Score = rbinom(n(), max_score, 0.5))
train <- df %>% filter(Time <= 8)
test <- df %>% filter(Time > 8)

test_that("prepare_data_lgtd returns the correct output", {

  ds1 <- prepare_data_lgtd(train, test = NULL, max_score, discrete)
  check_format(ds1)
  expect_equal(ds1$N_obs, nrow(train))
  expect_equal(ds1$N_pt, length(unique(train[["Patient"]])))
  expect_equal(as.numeric(ds1$k_obs), train[["Patient"]])
  expect_equal(as.numeric(ds1$t_obs), train[["Time"]])
  expect_equal(as.numeric(ds1$y_obs), train[["Score"]])
  expect_equal(ds1$N_test, 0)

  ds2 <- prepare_data_lgtd(train, test = test, max_score, discrete)
  check_format(ds2)
  expect_equal(ds2$N_test, nrow(test))
  expect_equal(as.numeric(ds2$k_test), test[["Patient"]])
  expect_equal(as.numeric(ds2$t_test), test[["Time"]])
  expect_equal(as.numeric(ds2$y_test), test[["Score"]])

})

test_that("prepare_stan.EczemaModel returns the correct output", {
  model <- EczemaModel("BinRW", max_score = max_score)
  ds <- prepare_standata(model, train = train, test = test)
  check_format(ds)
  expect_equal(sum(grepl("prior_", names(ds))), length(model$prior))
})

test_that("prepare_data_lgtd train and test inputs are correct", {
  expect_null(stopifnot_lgtd_train(train, max_score, discrete))
  expect_null(stopifnot_lgtd_test(test, train, max_score, discrete))
})

test_that("prepare_data_lgtd catches errors with train", {
  # cf. stopifnot_lgtd_dataframe
  l <- make_wrong_df(train, max_score)
  for (i in 1:length(l)) {
    expect_error(prepare_data_lgtd(l[[i]], test = NULL, max_score, discrete))
  }
  # cf. stopifnot_lgtd_train
  expect_warning(prepare_data_lgtd(filter(df, Patient != unique(df[["Patient"]])[1]), test = NULL, max_score, discrete))
})

test_that("prepare_data_lgtd catches errors with test", {
  # cf. stopifnot_lgtd_dataframe
  l <- make_wrong_df(test, max_score)
  for (i in 1:length(l)) {
    expect_error(prepare_data_lgtd(train, test = l[[i]], max_score, discrete))
  }
  # cf. stopifnot_lgtd_test
  expect_error(prepare_data_lgtd(train, test = filter(df, Time >= 8), max_score, discrete))
  expect_warning(prepare_data_lgtd(train, test = mutate(test, Patient = Patient + 1), max_score, discrete))
})

test_that("prepare_data_lgtd catches errors with discrete", {
  expect_error(prepare_data_lgtd(train, test = NULL, c(0, max_score), c(FALSE, FALSE)))
  expect_error(prepare_data_lgtd(train, test = NULL, c(0, max_score), 0))
})

test_that("prepare_data_lgtd catches errors with max_score", {
  expect_error(prepare_data_lgtd(train, test = NULL, max_score + 0.1, discrete))
  expect_error(prepare_data_lgtd(train, test = NULL, c(0, max_score), discrete))
})

# Test prepare_standata.MC ------------------------------------------------

# In test-model_MC.R
