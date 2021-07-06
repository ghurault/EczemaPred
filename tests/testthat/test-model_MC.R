set.seed(2021)
options(warn = -1)

# Test sample_prior ----------------------------------------------------

K1 <- 3
model <- EczemaModel("MC", K = K1)
fit_prior <- sample_prior(model, chains = 1, refresh = 0)

test_that("sample_prior_MC returns a stanfit object", {
  expect_true(is_stanfit(fit_prior))
})

test_that("estimates from sample_prior_MC are accurate", {
  p_mean <- apply(rstan::extract(fit_prior, pars = "p")[[1]], c(2, 3), mean)
  p_se <- apply(rstan::extract(fit_prior, pars = "p")[[1]], c(2, 3), function(x) {sd(x) / sqrt(length(x))})
  expect_true(all(abs(c(p_mean) - 1 / K1) < c(p_se) * 2.5)) # cf. default prior is symmetric
})

# Test fitting -------------------------------------------------------------

test_that("EczemaFit returns a stanfit object", {
  expect_true(is_stanfit(MC_fit))
})

test_that("estimates from EczemaFit are accurate", {
  p_smp <- rstan::extract(MC_fit, pars = c("p[1,2]", "p[2,1]"))
  p_mean <- vapply(p_smp, mean, numeric(1))
  p_sd <- vapply(p_smp, sd, numeric(1))

  expect_true(all(abs(p_mean - unlist(MC_params[c("p01", "p10")])) < 2 * p_sd))
})

# Test plot_transition_MC -------------------------------------------------

test_that("plot_transition_MC returns a ggplot object", {
  expect_s3_class(plot_transition_MC(fit_prior), "ggplot")
  expect_s3_class(plot_transition_MC(MC_fit), "ggplot")
})
