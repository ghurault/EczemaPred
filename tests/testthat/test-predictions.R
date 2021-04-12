# Initialisation ----------------------------------------------------------

set.seed(2021)

N <- 100
sigma <- 1
max_score <- 100
y0 <- 50
y <- y0 + cumsum(rnorm(N, mean = 0, sd = sigma))

df <- data.frame(Patient = 1,
                 Time = 1:N,
                 Score = y) %>%
  mutate(Iteration = get_fc_iteration(Time, 10))
l <- split_fc_dataset(df, 9)
l2 <- lapply(l, function(x) {mutate(x, Score = round(Score))}) # discretised

fit <- fit_RW(train = l$Training,
              test = l$Testing,
              max_score = max_score,
              discrete = FALSE,
              chains = 1,
              refresh = 0)

# Helpers -----------------------------------------------------------------

test_when_continuous <- function(perf, test) {
  expect_s3_class(perf, "data.frame")
  expect_true(all(c("lpd", "CRPS") %in% colnames(perf)))
  expect_equal(perf %>% select(!all_of(c("lpd", "CRPS"))), test)
  expect_gte(max(perf[["CRPS"]]), 0)
}

test_when_discrete <- function(perf, test) {
  expect_s3_class(perf, "data.frame")
  expect_true(all(c("lpd", "RPS") %in% colnames(perf)))
  expect_equal(perf %>% select(!all_of(c("lpd", "RPS"))), test)
  expect_gte(max(perf[["RPS"]]), 0)
}

# Test uniform predictions ------------------------------------------------

test_that("add_uniform_pred returns a correct dataframe (continuous)", {
  perf1 <- add_uniform_pred(test = l$Testing, max_score = max_score, discrete = FALSE, include_samples = FALSE)
  test_when_continuous(perf1, l$Testing)
  expect_equal(length(unique(perf1[["lpd"]])), 1)
})

test_that("add_uniform_pred returns a correct dataframe (discrete)", {
  perf2 <-  add_uniform_pred(test = l2$Testing, max_score = max_score, discrete = TRUE, include_samples = FALSE)
  test_when_discrete(perf2, l2$Testing)
  expect_equal(length(unique(perf2[["lpd"]])), 1)
})

test_that("add_uniform_pred returns samples when prompted", {
  perf3 <- add_uniform_pred(test = l$Testing, max_score = max_score, discrete = FALSE, include_samples = TRUE, n_samples = 50)
  expect_true(all(c("Samples") %in% colnames(perf3)))
  expect_true(is.list(perf3[["Samples"]]))
  expect_true(all(sapply(perf3[["Samples"]], length) == 50))
})

test_that("add_uniform_pred catches incorrect inputs", {
  expect_error(add_uniform_pred(test = as.list(l$Testing), max_score = max_score, discrete = FALSE, include_samples = FALSE))
  expect_error(add_uniform_pred(test = l$Testing, max_score = max(l$Testing[["Score"]]) - 1, discrete = FALSE, include_samples = FALSE))
  expect_error(add_uniform_pred(test = l$Testing, max_score = max_score, discrete = TRUE, include_samples = FALSE))
  expect_error(add_uniform_pred(test = l$Testing, max_score = max_score, discrete = FALSE, include_samples = 50))
  expect_error(add_uniform_pred(test = l$Testing, max_score = max_score, discrete = FALSE, include_samples = TRUE, n_samples = "all"))
})

# Test historical predictions ---------------------------------------------

test_that("add_historical_pred returns a correct dataframe (continuous)", {
  perf <- add_historical_pred(test = l$Testing, train = l$Training, max_score = max_score, discrete = FALSE, add_uniform = FALSE, include_samples = FALSE)
  test_when_continuous(perf, l$Testing)
})

test_that("add_historical_pred returns a correct dataframe (discrete)", {
  perf <- add_historical_pred(test = l2$Testing, train = l2$Training, max_score = max_score, discrete = TRUE, add_uniform = FALSE, include_samples = FALSE)
  test_when_discrete(perf, l2$Testing)
})

test_that("add_historical_pred works with add_uniform=TRUE for discrete forecasts", {
  perf1 <- add_uniform_pred(test = l2$Testing, max_score = max_score, discrete = TRUE)
  perf2 <- add_historical_pred(test = l2$Testing, train = l2$Training %>% filter(Patient == 0), max_score = max_score, discrete = TRUE, add_uniform = TRUE, include_samples = FALSE)
  expect_equal(perf1, perf2)
})

test_that("add_historical_pred returns samples when prompted", {
  perf <- add_historical_pred(test = l$Testing, train = l$Training, max_score = max_score, discrete = FALSE, include_samples = TRUE, n_samples = 50)
  expect_true(all(c("Samples") %in% colnames(perf)))
  expect_true(is.list(perf[["Samples"]]))
  expect_true(all(sapply(perf[["Samples"]], length) == 50))
  expect_true(all(perf[["Samples"]][[1]] %in% l$Training[["Score"]]))
})

test_that("add_historical_pred catches incorrect inputs", {
  expect_error(add_historical_pred(test = as.list(l$Testing), train = l$Training, max_score = max_score, discrete = FALSE, include_samples = FALSE))
  expect_error(add_historical_pred(test = l$Testing, train = as.list(l$Training), max_score = max_score, discrete = FALSE, include_samples = FALSE))
  expect_error(add_historical_pred(test = l$Testing, train = l$Training, max_score = max(l$Testing[["Score"]]) - 1, discrete = FALSE, include_samples = FALSE))
  expect_error(add_historical_pred(test = l$Testing, train = l$Training, max_score = max_score, discrete = TRUE, include_samples = FALSE))
  expect_error(add_historical_pred(test = l$Testing, train = l$Training, max_score = max_score, discrete = FALSE, include_samples = 50))
  expect_error(compute_historical_performance(test = l$Testing, train = l$Training, max_score = max_score, discrete = FALSE, include_samples = TRUE, n_samples = "all"))
})

# Test add_predictions ------------------------------------------------

#   add_predictions(test, fit, discrete = TRUE, include_samples = FALSE, n_samples = NULL)

test_that("add_predictions returns a correct dataframe (continuous)", {
  perf <- add_predictions(test = l$Testing, fit = fit, discrete = FALSE, include_samples = FALSE)
  test_when_continuous(perf, l$Testing)
})

# In test-metrics for discrete

test_that("add_predictions returns samples when prompted", {
  perf <- add_predictions(test = l$Testing, fit = fit, discrete = FALSE, include_samples = TRUE)
  expect_true(all(c("Samples") %in% colnames(perf)))
  expect_true(is.list(perf[["Samples"]]))
})

test_that("add_predictions catches incorrect inputs", {
  expect_error(add_predictions(test = as.list(l$Testing), fit = fit, discrete = FALSE, include_samples = FALSE))
  expect_error(add_predictions(test = l$Testing, fit = rstan::extract(fit, pars = "y_pred")[[1]], discrete = FALSE, include_samples = FALSE))
  expect_error(add_predictions(test = l$Testing, fit = fit, discrete = TRUE, include_samples = FALSE))
  expect_error(add_predictions(test = l$Testing, fit = fit, discrete = FALSE, include_samples = TRUE, n_samples = "all"))
})
