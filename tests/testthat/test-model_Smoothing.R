set.seed(2021)
options(warn = -1)

N_patient <- 10
t_max <- rpois(N_patient, 25)
max_score <- 100
param <- c("sigma", "tau")

model <- EczemaModel("Smoothing", max_score = max_score)

# Test incorrect priors ---------------------------------------------------

wrong_priors <- list(
  1,
  list(sigma_sd = 0.1, log_tau_mean = 1),
  list(a = 0.1, sigma = c(0, 1), tau = c(0, 1)),
  list(sigma_sd = c("0", "1"), tau = c(0, 1)),
  list(sigma = 1, tau = c(0, 1)),
  list(sigma_sd = c(0, -1), tau = c(0, 1)),
  list(sigma_sd = c(0, 1), tau = c(0, -1))
)

test_that("Smoothing constructor catches errors in prior", {
  for (i in 1:length(wrong_priors)) {
    expect_error(EczemaModel("Smoothing", max_score = max_score, prior = wrong_priors[[i]]))
  }
})

# Test sample_prior ---------------------------------------------

fit0 <- sample_prior(model, N_patient = N_patient, t_max = t_max, chains = 1, refresh = 0)

test_that("sample_prior returns a stanfit object", {
  expect_true(is_stanfit(fit0))
})

# Test fitting ------------------------------------------------------

l <- extract_simulations(fit = fit0,
                         id = get_index2(t_max),
                         draw = 5,
                         pars = param)

fit <- EczemaFit(model, train = l$Data, test = NULL, chains = 1, refresh = 0)

test_that("EczemaFit returns a stanfit object", {
  expect_true(is_stanfit(fit))
})

par <- HuraultMisc::summary_statistics(fit, pars = param) %>%
  left_join(l$Parameters, by = c("Variable" = "Parameter", "Index")) %>%
  rename(True = Value) %>%
  mutate(Coverage90 = (True > `5%` & True < `95%`),
         NormError = abs(Mean - True) / sd)

test_that("estimates from EczemaFit are accurate", {
  expect_true(all(par[["NormError"]] < 2.5))
})
