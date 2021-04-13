set.seed(2021)
options(warn = -1)

N_patient <- 10
t_max <- rpois(N_patient, 25)
max_score <- 100
param <- c("sigma", "tau")

wrong_priors <- list(
  1,
  list(sigma_sd = 0.1, log_tau_mean = 1),
  list(a = 0.1, sigma = c(0, 1), tau = c(0, 1)),
  list(sigma_sd = c("0", "1"), tau = c(0, 1)),
  list(sigma = 1, tau = c(0, 1)),
  list(sigma_sd = c(0, -1), tau = c(0, 1)),
  list(sigma_sd = c(0, 1), tau = c(0, -1))
)

# Test default_prior_Smoothing ---------------------------------------------------

test_that("default_prior_Smoothing works", {
  expect_null(stopifnot_prior_Smoothing(default_prior_Smoothing()))
})

# Test sample_prior_Smoothing ---------------------------------------------

fit0 <- sample_prior_Smoothing(N_patient = N_patient, t_max = t_max, max_score = max_score, chains = 1, refresh = 0)

test_that("sample_prior_Smoothing returns a stanfit object", {
  expect_true(is_stanfit(fit0))
})

test_that("sample_prior_Smoothing catches errors in prior", {
  for (i in 1:length(wrong_priors)) {
    expect_error(sample_prior_Smoothing(max_score = max_score, prior = wrong_priors[[i]]))
  }
})

# Test fit_Smoothing ------------------------------------------------------

l <- extract_fakedata(fit_prior = fit0,
                      draw = 5,
                      pars = param,
                      N_patient = N_patient,
                      t_max = t_max,
                      horizon = 2)

fit <- fit_Smoothing(train = l$Train, test = l$Test, max_score = max_score, chains = 1, refresh = 0)

test_that("fit_Smoothing returns a stanfit object", {
  expect_true(is_stanfit(fit))
})

par <- HuraultMisc::summary_statistics(fit, pars = param) %>%
  left_join(l$TrueParameters, by = c("Variable" = "Parameter", "Index")) %>%
  rename(True = Value) %>%
  mutate(Coverage90 = (True > `5%` & True < `95%`),
         NormError = abs(Mean - True) / sd)

test_that("estimates from fit_Smoothing are accurate", {
  expect_true(all(par[["NormError"]] < 2.5))
})

test_that("fit_Smoothing catches errors in prior", {
  # cf. stopifnot_prior_Smoothing
  for (i in 1:length(wrong_priors)) {
    expect_error(fit_Smoothing(train = l$Train, test = l$Test, max_score = max_score, prior = wrong_priors[[i]]))
  }
})
