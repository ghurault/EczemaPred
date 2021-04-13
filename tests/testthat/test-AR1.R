set.seed(2021)
options(warn = -1)

N_patient <- 10
t_max <- rpois(N_patient, 25)
max_score <- 100
param <- c("sigma", "alpha", "y_inf")

wrong_priors <- list(
  1,
  list(sigma = c(0, 0.1)),
  list(a = c(0, 0.1), alpha = c(1, 1), y_inf = c(0.5, 0.25)),
  list(sigma = c("0", "0.1"), alpha = c(1, 1), y_inf = c(0.5, 0.25)),
  list(sigma = 0.1, alpha = c(1, 1), y_inf = c(0.5, 0.25)),
  list(sigma = c(0, -0.1), alpha = c(1, 1), y_inf = c(0.5, 0.25)),
  list(sigma = c(0, 0.1), alpha = -c(1, 1), y_inf = c(0.5, 0.25)),
  list(sigma = c(0, 0.1), y_inf = c(0.5, -0.25))
)

# Test default_prior_AR1 ---------------------------------------------------

test_that("default_prior_AR1 works", {
  expect_null(stopifnot_prior_AR1(default_prior_AR1()))
})

# Test sample_prior_AR1 ---------------------------------------------

fit0 <- sample_prior_AR1(N_patient = N_patient, t_max = t_max, max_score = max_score, chains = 1, refresh = 0)

test_that("sample_prior_AR1 returns a stanfit object", {
  expect_true(is_stanfit(fit0))
})

test_that("sample_prior_AR1 catches errors in prior", {
  for (i in 1:length(wrong_priors)) {
    expect_error(sample_prior_AR1(max_score = max_score, prior = wrong_priors[[i]]))
  }
})

# Test fit_AR1 ------------------------------------------------------

l <- extract_fakedata(fit_prior = fit0,
                      draw = 10,
                      pars = param,
                      N_patient = N_patient,
                      t_max = t_max,
                      horizon = 2)

fit <- fit_AR1(train = l$Train, test = l$Test, max_score = max_score, chains = 1, refresh = 0)

test_that("fit_AR1 returns a stanfit object", {
  expect_true(is_stanfit(fit))
})

test_that("estimates from fit_AR1 are accurate", {
  skip_on_ci()

  par <- HuraultMisc::summary_statistics(fit, pars = param) %>%
    left_join(l$TrueParameters, by = c("Variable" = "Parameter", "Index")) %>%
    rename(True = Value) %>%
    mutate(Coverage90 = (True > `5%` & True < `95%`),
           NormError = abs(Mean - True) / sd)
  expect_lt(max(par[["NormError"]], na.rm = TRUE), 3.0)
})

test_that("fit_AR1 catches errors in prior", {
  # cf. stopifnot_prior_AR1
  for (i in 1:length(wrong_priors)) {
    expect_error(fit_AR1(train = l$Train, test = l$Test, max_score = max_score, prior = wrong_priors[[i]]))
  }
})
