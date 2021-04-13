set.seed(2021)
options(warn = -1)

# Helpers -----------------------------------------------------------------

is_vector_or_array <- function(x) {is.vector(x) | is.array(x)}

check_format <- function(ds) {
  input_names <- c("K", "N", "y0", "y1", "dt", "run", "prior_p", "N_test", "y0_test", "y1_test", "dt_test")

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

# Test default prior --------------------------------------------------------------

test_that("default_prior_MC works", {
  expect_error(default_prior_MC(-1))
  expect_error(default_prior_MC(1:5))
  for (K in 2:5) {
    expect_null(stopifnot_prior_MC(default_prior_MC(K), K))
  }
})

# Test prepare_data_MC ---------------------------------------------------------------------

K <- 5
train <- data.frame(y0 = 1:K, y1 = 1:K, dt = 1)
test <- data.frame(y0 = K, y1 = 1, dt = 2)
prior <- list(p = matrix(2, nrow = K, ncol = K))

test_that("prepare_data_MC returns the correct output", {

  expect_null(stopifnot_MC_dataframe(train, K))

  ds1 <- prepare_data_MC(train, test = NULL, K = K)
  check_format(ds1)
  expect_equal(ds1$K, K)
  expect_equal(ds1$N, nrow(train))
  expect_equal(as.numeric(ds1$y0), train[["y0"]])
  expect_equal(as.numeric(ds1$y1), train[["y1"]])
  expect_equal(as.numeric(ds1$dt), train[["dt"]])
  expect_equal(ds1$run, 1)
  expect_equal(ds1$N_test, 0)

  ds2 <- prepare_data_MC(train, test, K = K, prior = prior)
  check_format(ds2)
  expect_equal(ds2$prior_p, prior$p)
  expect_equal(ds2$N_test, nrow(test))
  expect_equal(as.numeric(ds2$y0_test), test[["y0"]])
  expect_equal(as.numeric(ds2$y1_test), test[["y1"]])
  expect_equal(as.numeric(ds2$dt_test), test[["dt"]])

})

test_that("prepare_data_MC catches errors in train", {
  # cf. stopifnot_MC_dataframe
  l <- make_wrong_df(train, K)
  for (i in 1:length(l)) {
    expect_error(prepare_data_MC(l[[i]], test = NULL, K = K))
  }
})

test_that("prepare_data_MC catches errors in test", {
  # cf. stopifnot_MC_dataframe
  l <- make_wrong_df(test, K)
  for (i in 1:length(l)) {
    expect_error(prepare_data_MC(train, test = l[[i]], K = K))
  }
})

test_that("prepate_data_MC catches errors in K", {
  expect_error(prepare_data_MC(train, test = NULL, K = 1))
  expect_error(prepare_data_MC(train, test = NULL, K = 1:5))
})

test_that("prepare_data_MC catches errors in prior", {
  # cf. stopifnot_prior_MC
  l <- make_wrong_prior(K)
  for (i in 1:length(l)) {
    expect_error(prepare_data_MC(train, test = NULL, K = K, prior = l[[i]]))
  }
})

# Test sample_prior_MC ----------------------------------------------------

K1 <- 3
fit_prior <- sample_prior_MC(K = K1, chains = 1, refresh = 0)

test_that("sample_prior_MC returns a stanfit object", {
  expect_true(is_stanfit(fit_prior))
})

test_that("estimates from sample_prior_MC are accurate", {
  p_mean <- apply(rstan::extract(fit_prior, pars = "p")[[1]], c(2, 3), mean)
  p_se <- apply(rstan::extract(fit_prior, pars = "p")[[1]], c(2, 3), function(x) {sd(x) / sqrt(length(x))})
  expect_true(all(abs(c(p_mean) - 1 / K1) < c(p_se) * 2.5)) # cf. default prior is symmetric
})

# Test fit_MC -------------------------------------------------------------

K2 <- 2
N <- 300
p01 <- 0.2
p10 <- 0.4
prob_mis <- 0.2
df2 <- data.frame(t0 = 1:N,
                  y0 = generate_MC2_sequence(N = N, p01 = p01, p10 = p10) + 1) %>%
  filter(!generate_missing(N, type = "random", p_mis = prob_mis)) %>%
  mutate(y1 = lead(y0),
         dt = lead(t0) - t0,
         Label = case_when(t0 <= 0.9 * N ~ "Training",
                           TRUE ~ "Testing")) %>%
  drop_na()
train2 <- df2 %>% filter(Label == "Training")
test2 <- df2 %>% filter(Label == "Testing")

fit <- fit_MC(train = train2, test = test2, K = K2, chains = 1, refresh = 0)

test_that("fit_MC returns a stanfit object", {
  expect_true(is_stanfit(fit))
})

test_that("estimates from fit_MC are accurate", {
  p_smp <- rstan::extract(fit, pars = c("p[1,2]", "p[2,1]"))
  p_mean <- sapply(p_smp, mean)
  p_se <- sapply(p_smp, function(x) {sd(x) / sqrt(x)})

  expect_true(all(abs(p_mean - c(p01, p10)) < 2.5 * p_se))
})

# Test plot_transition_MC -------------------------------------------------

test_that("plot_transition_MC returns a ggplot object", {
  expect_s3_class(plot_transition_MC(fit_prior), "ggplot")
  expect_s3_class(plot_transition_MC(fit), "ggplot")
})
