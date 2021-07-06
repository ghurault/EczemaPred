set.seed(2021)
options(warn = -1)

# Test incorrect priors ---------------------------------------------------

wrong_priors <- list(
  1,
  list(sigma = c("0", "1"), tau = c(0, 1)),
  list(sigma = 1, tau = c(0, 1)),
  list(sigma = c(0, -1), tau = c(0, 1)),
  list(sigma = c(0, 1), tau = c(0, -1))
)

test_that("Smoothing constructor catches errors in prior", {
  for (i in 1:length(wrong_priors)) {
    expect_error(EczemaModel("Smoothing", max_score = 100, prior = wrong_priors[[i]]))
  }
})

# Test fitting ------------------------------------------------------

test_that("estimates of Smoothing parameters by EczemaFit are accurate", {

  skip_on_cran()

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
