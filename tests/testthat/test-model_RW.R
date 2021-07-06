
test_that("estimate of sigma from EczemaFit is accurate", {

  post_sigma <- rstan::extract(RW_fit, pars = "sigma")[[1]]
  sigma_mean <- mean(post_sigma)
  sigma_sd <- sd(post_sigma)

  expect_lt(abs(RW_params$sigma - sigma_mean), 2.5 * sigma_sd)

})
