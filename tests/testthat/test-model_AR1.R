set.seed(2021)
options(warn = -1)

N_patient <- 10
t_max <- rpois(N_patient, 25)
max_score <- 100
param <- c("sigma", "alpha", "y_inf")

model <- EczemaModel("AR1", max_score = max_score)

# Test incorrect priors ---------------------------------------------------

wrong_priors <- list(
  1,
  list(sigma = c(0, 1), sigma = c(0, 1)),
  list(sigma = c("0", "0.1")),
  list(sigma = 0.1),
  list(sigma = c(0, -0.1)),
  list(slope = -c(1, 1)),
  list(y_inf = c(0.5, -0.25))
)

test_that("AR1 constructor catches errors in prior", {
  for (i in 1:length(wrong_priors)) {
    expect_error(EczemaModel("AR1", max_score = max_score, prior = wrong_priors[[i]]))
  }
})

# Test sample_prior ---------------------------------------------

fit0 <- sample_prior(model, N_patient = N_patient, t_max = t_max, chains = 1, refresh = 0)

test_that("sample_prior returns a stanfit object", {
  expect_true(is_stanfit(fit0))
})

# Test extract_simulations ------------------------------------------------

l <- extract_simulations(fit = fit0,
                         id = get_index2(t_max),
                         draw = 10,
                         pars = param)

test_that("extract_simulations works", {
  expect_true(is.list(l))
  expect_true(all(c("Data", "Parameters") %in% names(l)))
  lapply(l, function(x) {expect_s3_class(x, "data.frame")})
  expect_true(all(c("Patient", "Time", "Score") %in% colnames(l$Data)))
  expect_true(all(c("Draw", "Index", "Value", "Parameter") %in% colnames(l$Parameters)))
})

test_that("extract_simulations catches errors in inputs", {
  expect_error(extract_simulations(fit = rstan::extract(fit0, pars = "y_rep"), id = get_index2(t_max), draw = 10, pars = param))
  expect_error(extract_simulations(fit = fit0, id = t_max, draw = 10, pars = param))
  expect_error(extract_simulations(fit = fit0, id = get_index2(t_max), draw = -1, pars = param))
  expect_error(extract_simulations(fit = fit0, id = get_index2(t_max), draw = 10, pars = y_rep))
})

# Test fitting ------------------------------------------------------

fit <- EczemaFit(model, train = l$Data, test = NULL, chains = 1, refresh = 0)

test_that("EczemaFit returns a stanfit object", {
  expect_true(is_stanfit(fit))
})

test_that("estimates from EczemaFit are accurate", {
  skip_on_ci()

  par <- HuraultMisc::summary_statistics(fit, pars = param) %>%
    left_join(l$Parameters, by = c("Variable" = "Parameter", "Index")) %>%
    rename(True = Value) %>%
    mutate(Coverage90 = (True > `5%` & True < `95%`),
           NormError = abs(Mean - True) / sd)
  expect_lt(max(par[["NormError"]], na.rm = TRUE), 3.0)
})
