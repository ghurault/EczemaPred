# Test add_prior ----------------------------------------------------------

test_that("add_prior works", {

  data_stan <- list()
  val_mu <- 1
  val_eps <- matrix(stats::rnorm(4), nrow = 2, ncol = 2)
  prior <- list(mu = val_mu, eps = val_eps)

  x <- add_prior(data_stan, prior)

  expect_true(is.list(x))
  expect_equal(length(x), length(data_stan) + length(prior))
  expect_true("prior_mu" %in% names(x))
  expect_equal(x$prior_mu, val_mu)
  expect_equal(x$prior_eps, val_eps)

})
