set.seed(2021)
options(warn = -1)

N_patient <- 10
t_max <- rpois(N_patient, 25)
max_score <- 100
slope <- 0.8
intercept <- 2
y_inf <- intercept / (1 - slope)
sigma <- 2
y0 <- round(rbeta(N_patient, 5, 5) * max_score)
param <- c("sigma", "intercept", "slope")

model <- EczemaModel("AR1", max_score = max_score)

# Test extract_simulations ------------------------------------------------

fit0 <- sample_prior(model, N_patient = N_patient, t_max = t_max, chains = 1, refresh = 0)

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

test_that("estimates of AR1 by EczemaFit are accurate", {
  skip_on_cran()
  # skip_on_ci()

  df <- generate_fakedata(N_pt = N_patient,
                          t_max = t_max,
                          max_score = max_score,
                          params = list(alpha = 1,
                                        intercept = rep(intercept, N_patient),
                                        slope = rep(slope, N_patient),
                                        y0 = y0,
                                        sigma = sigma))

  fit <- EczemaFit(model, train = df, test = NULL, chains = 1, refresh = 0)

  post <- rstan::extract(fit, pars = c("sigma", "slope", "intercept"))
  post_mean <- vapply(post, mean, numeric(1))
  post_sd <- vapply(post, sd, numeric(1))

  expect_true(all(abs(c(sigma, slope, intercept) - post_mean) / post_sd < 2.5))

})
