set.seed(2021)
options(warn = -1)

# Test fitting ------------------------------------------------------

test_that("estimates of AR1 by EczemaFit are accurate", {
  skip_on_cran()
  # skip_on_ci()

  N_patient <- 10
  t_max <- rpois(N_patient, 25)

  slope <- 0.8
  intercept <- 2
  # y_inf <- intercept / (1 - slope)
  sigma <- 2

  model <- EczemaModel("AR1", max_score = 100)

  df <- generate_fakedata(N_pt = N_patient,
                          t_max = t_max,
                          max_score = model$max_score,
                          params = list(alpha = 1,
                                        intercept = rep(intercept, N_patient),
                                        slope = rep(slope, N_patient),
                                        y0 = round(rbeta(N_patient, 5, 5) * model$max_score),
                                        sigma = sigma))

  fit <- EczemaFit(model, train = df, test = NULL, chains = 1, refresh = 0)

  post <- rstan::extract(fit, pars = c("sigma", "slope", "intercept"))
  post_mean <- vapply(post, mean, numeric(1))
  post_sd <- vapply(post, sd, numeric(1))

  expect_true(all(abs(c(sigma, slope, intercept) - post_mean) / post_sd < 2.5))

})
