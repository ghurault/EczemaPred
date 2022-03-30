# Initialisation -----------------------------------------------------------------

RW_split2 <- lapply(RW_split, function(x) {mutate(x, Score = round(Score))}) # discretised

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
  perf1 <- add_uniform_pred(test = RW_split$Testing, max_score = RW_setup$max_score, discrete = FALSE, include_samples = FALSE)
  test_when_continuous(perf1, RW_split$Testing)
  expect_equal(length(unique(perf1[["lpd"]])), 1)
})

test_that("add_uniform_pred returns a correct dataframe (discrete)", {
  perf2 <-  add_uniform_pred(test = RW_split2$Testing, max_score = RW_setup$max_score, discrete = TRUE, include_samples = FALSE)
  test_when_discrete(perf2, RW_split2$Testing)
  expect_equal(length(unique(perf2[["lpd"]])), 1)
})

test_that("add_uniform_pred returns samples when prompted", {

  perf3 <- add_uniform_pred(test = RW_split$Testing,
                            max_score = RW_setup$max_score,
                            discrete = FALSE,
                            include_samples = TRUE,
                            n_samples = 50)
  perf4 <- add_uniform_pred(test = data.frame(Score = rbinom(1e2, 100, .5)),
                            max_score = 100,
                            discrete = TRUE,
                            include_samples = TRUE,
                            n_samples = 50)
  for (x in list(perf3, perf4)) {
    expect_true(all(c("Samples") %in% colnames(x)))
    expect_true(is.list(x[["Samples"]]))
    expect_true(all(vapply(x[["Samples"]], length, numeric(1)) == 50))
  }

})

test_that("add_uniform_pred catches incorrect inputs", {
  expect_error(add_uniform_pred(test = as.list(RW_split$Testing), max_score = RW_setup$max_score, discrete = FALSE, include_samples = FALSE))
  expect_error(add_uniform_pred(test = RW_split$Testing, max_score = max(RW_split$Testing[["Score"]]) - 1, discrete = FALSE, include_samples = FALSE))
  expect_error(add_uniform_pred(test = RW_split$Testing, max_score = RW_setup$max_score, discrete = TRUE, include_samples = FALSE))
  expect_error(add_uniform_pred(test = RW_split$Testing, max_score = RW_setup$max_score, discrete = FALSE, include_samples = 50))
  expect_error(add_uniform_pred(test = RW_split$Testing, max_score = RW_setup$max_score, discrete = FALSE, include_samples = TRUE, n_samples = "all"))
})

# Test historical predictions ---------------------------------------------

test_that("add_historical_pred returns a correct dataframe (continuous)", {
  perf <- add_historical_pred(test = RW_split$Testing, train = RW_split$Training, max_score = RW_setup$max_score, discrete = FALSE, add_uniform = FALSE, include_samples = FALSE)
  test_when_continuous(perf, RW_split$Testing)
})

test_that("add_historical_pred returns a correct dataframe (discrete)", {
  perf <- add_historical_pred(test = RW_split2$Testing, train = RW_split2$Training, max_score = RW_setup$max_score, discrete = TRUE, add_uniform = FALSE, include_samples = FALSE)
  test_when_discrete(perf, RW_split2$Testing)
})

test_that("add_historical_pred works with add_uniform=TRUE for discrete forecasts", {
  perf1 <- add_uniform_pred(test = RW_split2$Testing, max_score = RW_setup$max_score, discrete = TRUE)
  perf2 <- add_historical_pred(test = RW_split2$Testing, train = RW_split2$Training %>% filter(Patient == 0), max_score = RW_setup$max_score, discrete = TRUE, add_uniform = TRUE, include_samples = FALSE)
  expect_equal(perf1, perf2)
})

test_that("add_historical_pred returns samples when prompted", {
  for (ns in list(50, NULL)) {
    perf <- add_historical_pred(test = RW_split$Testing, train = RW_split$Training, max_score = RW_setup$max_score, discrete = FALSE, include_samples = TRUE, n_samples = ns)
    expect_true(all(c("Samples") %in% colnames(perf)))
    expect_true(is.list(perf[["Samples"]]))
    if (!is.null(ns)) {
      expect_true(all(vapply(perf[["Samples"]], length, numeric(1)) == ns))
    }
    expect_true(all(perf[["Samples"]][[1]] %in% RW_split$Training[["Score"]]))
  }
})

test_that("add_historical_pred catches incorrect inputs", {
  expect_error(add_historical_pred(test = as.list(RW_split$Testing), train = RW_split$Training, max_score = RW_setup$max_score, discrete = FALSE, include_samples = FALSE))
  expect_error(add_historical_pred(test = RW_split$Testing, train = as.list(RW_split$Training), max_score = RW_setup$max_score, discrete = FALSE, include_samples = FALSE))
  expect_error(add_historical_pred(test = RW_split$Testing, train = RW_split$Training, max_score = max(RW_split$Testing[["Score"]]) - 1, discrete = FALSE, include_samples = FALSE))
  expect_error(add_historical_pred(test = RW_split$Testing, train = RW_split$Training, max_score = RW_setup$max_score, discrete = TRUE, include_samples = FALSE))
  expect_error(add_historical_pred(test = RW_split$Testing, train = RW_split$Training, max_score = RW_setup$max_score, discrete = FALSE, include_samples = 50))
  expect_error(compute_historical_performance(test = RW_split$Testing, train = RW_split$Training, max_score = RW_setup$max_score, discrete = FALSE, include_samples = TRUE, n_samples = "all"))
})

# Test add_predictions and add_metrics (continuous) ------------------------------------------------
# In test-metrics for discrete

test_that("add_metrics1_c returns a correct dataframe", {
  perf <- add_metrics1_c(df = RW_split$Testing, fit = RW_fit)
  test_when_continuous(perf, RW_split$Testing)
})

test_that("add_metrics2_c returns a correct dataframe", {
  tmp <- RW_split$Testing %>%
    mutate(Samples = samples_to_list(RW_fit, par_name = "y_pred"))

  list(
    add_metrics2_c(tmp),
    add_metrics2_c(tmp, add_samples = 0:RW_setup$max_score),
    add_metrics2_c(tmp, bw = 1)
  ) %>%
    lapply(function(perf) {
      test_when_continuous(select(perf, -Samples),
                           RW_split$Testing)
    })
})

test_that("add_predictions returns a correct dataframe (continuous)", {
  perf <- add_predictions(df = RW_split$Testing, fit = RW_fit, discrete = FALSE, include_samples = FALSE)
  test_when_continuous(perf, RW_split$Testing)
})

test_that("add_predictions returns samples when prompted", {
  for (ns in list(50, NULL)) {
    perf <- add_predictions(df = RW_split$Testing, fit = RW_fit, discrete = FALSE, include_samples = TRUE, n_samples = ns)
    expect_true(all(c("Samples") %in% colnames(perf)))
    expect_true(is.list(perf[["Samples"]]))
  }
})

test_that("add_predictions catches incorrect inputs", {
  expect_error(add_predictions(df = as.list(RW_split$Testing), fit = RW_fit, discrete = FALSE, include_samples = FALSE))
  expect_error(add_predictions(df = RW_split$Testing, fit = rstan::extract(RW_fit, pars = "y_pred")[[1]], discrete = FALSE, include_samples = FALSE))
  expect_error(add_predictions(df = RW_split$Testing, fit = RW_fit, discrete = TRUE, include_samples = FALSE))
  expect_error(add_predictions(df = RW_split$Testing, fit = RW_fit, discrete = FALSE, include_samples = TRUE, n_samples = "all"))
})

# Test samples_to_list ----------------------------------------------------

test_that("samples_to_list works", {
  tmp <- list(
    samples_to_list(RW_fit),
    samples_to_list(rstan::extract(RW_fit, pars = "y_pred")[[1]])
  )
  for (i in 1:length(tmp)) {
    expect_true(is.list(tmp[[i]]))
  }
})
