# Testing inference of "continuous" reference models (RW, Smoothing, AR1, MixedAR1)

options(warn = -1)

# RW ----------------------------------------------------------------------

test_that("estimate of sigma from EczemaFit is accurate", {

  post_sigma <- rstan::extract(RW_fit, pars = "sigma")[[1]]
  sigma_mean <- mean(post_sigma)
  sigma_sd <- sd(post_sigma)

  expect_lt(abs(RW_params$sigma - sigma_mean), 2.5 * sigma_sd)

})

# Smoothing ---------------------------------------------------------------

test_that("estimates of Smoothing parameters by EczemaFit are accurate", {

  skip_on_cran()

  set.seed(2021)

  N_patient <- 10
  t_max <- rpois(N_patient, 25)
  max_score <- 100
  sigma <- 2
  alpha <- 0.5
  tau <- -1 / log(1 - alpha)
  y0 <- round(rbeta(N_patient, 5, 5) * max_score)

  model <- EczemaModel("Smoothing", max_score = max_score)

  df <- generate_fakedata(N_pt = N_patient,
                          t_max = t_max,
                          max_score = max_score,
                          params = list(alpha = alpha,
                                        intercept = rep(0, N_patient),
                                        slope = rep(1, N_patient),
                                        y0 = y0,
                                        sigma = sigma))

  fit <- EczemaFit(model, train = df, test = NULL, chains = 1, refresh = 0)

  post <- rstan::extract(fit, pars = c("sigma", "tau"))
  post_mean <- vapply(post, mean, numeric(1))
  post_sd <- vapply(post, sd, numeric(1))

  expect_true(all(abs(c(sigma, tau) - post_mean) / post_sd < 2.5))

})

# AR1 ---------------------------------------------------------------------

test_that("estimates of AR1 by EczemaFit are accurate", {

  skip_on_cran()
  # skip_on_ci()

  set.seed(2021)

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

# MixedAR1 ----------------------------------------------------------------

test_that("estimates of MixedAR1 by EczemaFit are accurate", {

  skip_on_cran()
  # skip_on_ci()

  set.seed(2021)

  N_patient <- 30
  t_max <- rpois(N_patient, 25)
  max_score <- 100
  param <- list_parameters("MixedAR1")

  model <- EczemaModel("MixedAR1", max_score = max_score)

  y0 <- rbeta(N_patient, 5, 5) * max_score
  sigma <- 2

  mu_logit_slope <- 1
  sigma_logit_slope <- 1
  slope <- HuraultMisc::inv_logit(rnorm(N_patient, mu_logit_slope, sigma_logit_slope))

  mu_inf <- 0.1 * max_score
  sigma_inf <- 0.05 * max_score
  y_inf <- rnorm(N_patient, mu_inf, sigma_inf)

  intercept <- y_inf * (1 - slope)

  df <- generate_fakedata(N_pt = N_patient,
                          t_max = t_max,
                          max_score = max_score,
                          params = list(alpha = 1,
                                        intercept = intercept,
                                        slope = slope,
                                        y0 = y0,
                                        sigma = sigma))

  fit <- EczemaFit(model, train = df, chains = 1, refresh = 0)

  expect_true(is_stanfit(fit)) # need it otherwise empty test

  test_that("estimate of population parameters from EczemaFit is accurate", {
    post <- rstan::extract(fit, pars = param$Population)
    post_mean <- vapply(post, mean, numeric(1))
    post_sd <- vapply(post, sd, numeric(1))
    truth <- c(sigma, mu_logit_slope, sigma_logit_slope, mu_inf, sigma_inf)

    expect_true(all(abs(post_mean - truth) / post_sd < 2.5))
  })

  test_that("estimates of slope from EczemaFit are accurate", {
    post_slope <- rstan::extract(fit, pars = "slope")[[1]]
    slope_mean <- apply(post_slope, 2, mean)
    slope_sd <- apply(post_slope, 2, sd)

    expect_true(all(abs(slope_mean - slope) / slope_sd < 2.5))
  })

  test_that("estimates of intercept from EczemaFit are accurate", {
    post_intercept <- rstan::extract(fit, pars = "intercept")[[1]]
    intercept_mean <- apply(post_intercept, 2, mean)
    intercept_sd <- apply(post_intercept, 2, sd)

    expect_true(all(abs(intercept_mean - intercept) / intercept_sd < 2.5))
  })

})
