set.seed(2021)
options(warn = -1)

# Helpers -----------------------------------------------------------------

is_vector_or_array <- function(x) {is.vector(x) | is.array(x)}

check_format <- function(ds) {
  input_names <- c("K", "N", "y0", "y1", "dt", "N_test", "y0_test", "y1_test", "dt_test", "prior_p")

  expect_true(is.list(ds))
  expect_true(all(input_names %in% names(ds)))
  expect_true(is_vector_or_array(ds$y0))
  expect_true(is_vector_or_array(ds$y1))
  expect_true(is_vector_or_array(ds$dt))
  expect_true(is_vector_or_array(ds$y0_test))
  expect_true(is_vector_or_array(ds$y1_test))
  expect_true(is_vector_or_array(ds$dt_test))
}

make_wrong_df <- function(df, K) {
  # Assuming df is a correct train/test input, makes a list of inputs derived from df that are incorrect

  list(
    as.list(df),
    rename(df, Current = y0),
    rename(df, Next = y1),
    rename(df, Delay = dt),
    mutate(df, y0 = y0 - max(y0) + K + 1),
    mutate(df, y0 = y0 - min(y0)),
    mutate(df, y1 = y1 - max(y1) + K + 1),
    mutate(df, y1 = y1 - min(y1)),
    mutate(df, dt = dt + 0.5),
    mutate(df, dt = -dt),
    mutate(df, y0 = replace(y0, y0 == min(y0), NA))
  )

}

make_wrong_prior <- function(K) {
  # Make a list of incorrect prior inputs

  list(
    matrix(1, nrow = K, ncol = K),
    list(matrix(1, nrow = K, ncol = K)),
    list(p = as.data.frame(matrix(1, nrow = K, ncol = K))),
    list(p = 1),
    list(p = matrix(-1, nrow = K, ncol = K))
  )

}

# Test priors ---------------------------------------------------

test_that("MC constructor catches errors in prior", {
  for (K in 2:5) {
    wrong_priors <- make_wrong_prior(K)
    for (i in 1:length(wrong_priors)) {
      expect_error(EczemaModel("BinRW", K = K, prior = wrong_priors[[i]]))
    }
  }
})

test_that("MC constructor accepts other prior", {
  K <- 5
  prior <- list(p = matrix(2, nrow = K, ncol = K))
  model <- EczemaModel("MC", K = K, prior = prior)
  expect_equal(model$prior, prior)
})

# Test prepare_standata ---------------------------------------------------------------------

K <- 5
model <- EczemaModel("MC", K = K)
train <- data.frame(y0 = 1:K, y1 = 1:K, dt = 1)
test <- data.frame(y0 = K, y1 = 1, dt = 2)

test_that("prepare_standata returns the correct output", {

  expect_null(stopifnot_MC_dataframe(train, K))

  ds1 <- prepare_standata(model, train, test = NULL)
  check_format(ds1)
  expect_equal(ds1$K, K)
  expect_equal(ds1$N, nrow(train))
  expect_equal(as.numeric(ds1$y0), train[["y0"]])
  expect_equal(as.numeric(ds1$y1), train[["y1"]])
  expect_equal(as.numeric(ds1$dt), train[["dt"]])
  expect_equal(ds1$N_test, 0)
  expect_equal(ds1$prior_p, model$prior$p)

  ds2 <- prepare_standata(model, train, test)
  check_format(ds2)
  expect_equal(ds2$N_test, nrow(test))
  expect_equal(as.numeric(ds2$y0_test), test[["y0"]])
  expect_equal(as.numeric(ds2$y1_test), test[["y1"]])
  expect_equal(as.numeric(ds2$dt_test), test[["dt"]])

})

test_that("prepare_standata catches errors in train", {
  # cf. stopifnot_MC_dataframe
  l <- make_wrong_df(train, K)
  for (i in 1:length(l)) {
    expect_error(prepare_standata(modell[[i]], test = NULL))
  }
})

test_that("prepare_standata catches errors in test", {
  # cf. stopifnot_MC_dataframe
  l <- make_wrong_df(test, K)
  for (i in 1:length(l)) {
    expect_error(prepare_standata(model, train, test = l[[i]]))
  }
})

# Test sample_prior ----------------------------------------------------

K1 <- 3
model <- EczemaModel("MC", K = K1)
fit_prior <- sample_prior(model, chains = 1, refresh = 0)

test_that("sample_prior_MC returns a stanfit object", {
  expect_true(is_stanfit(fit_prior))
})

test_that("estimates from sample_prior_MC are accurate", {
  p_mean <- apply(rstan::extract(fit_prior, pars = "p")[[1]], c(2, 3), mean)
  p_se <- apply(rstan::extract(fit_prior, pars = "p")[[1]], c(2, 3), function(x) {sd(x) / sqrt(length(x))})
  expect_true(all(abs(c(p_mean) - 1 / K1) < c(p_se) * 2.5)) # cf. default prior is symmetric
})

# Test fitting -------------------------------------------------------------

test_that("EczemaFit returns a stanfit object", {
  expect_true(is_stanfit(MC_fit))
})

test_that("estimates from EczemaFit are accurate", {
  p_smp <- rstan::extract(MC_fit, pars = c("p[1,2]", "p[2,1]"))
  p_mean <- vapply(p_smp, mean, numeric(1))
  p_sd <- vapply(p_smp, sd, numeric(1))

  expect_true(all(abs(p_mean - unlist(MC_params[c("p01", "p10")])) < 2 * p_sd))
})

# Test plot_transition_MC -------------------------------------------------

test_that("plot_transition_MC returns a ggplot object", {
  expect_s3_class(plot_transition_MC(fit_prior), "ggplot")
  expect_s3_class(plot_transition_MC(MC_fit), "ggplot")
})
