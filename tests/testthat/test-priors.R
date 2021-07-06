# Testing incorrect priors

max_score <- 100

# RW -------------------------------------------------------------------------

test_that("RW constructor catches errors in prior", {

  wrong_priors <- list(
    1,
    list(sigma = c(0, 1), sigma = c(0, 1)),
    list(sigma = 1),
    list(sigma = c("0", "1"))
  )

  for (i in 1:length(wrong_priors)) {
    expect_error(EczemaModel("RW", max_score = max_score, discrete = FALSE, prior = wrong_priors[[i]]))
  }

})

# Smoothing ---------------------------------------------------------------

test_that("Smoothing constructor catches errors in prior", {

  wrong_priors <- list(
    1,
    list(sigma = c("0", "1"), tau = c(0, 1)),
    list(sigma = 1, tau = c(0, 1)),
    list(sigma = c(0, -1), tau = c(0, 1)),
    list(sigma = c(0, 1), tau = c(0, -1))
  )

  for (i in 1:length(wrong_priors)) {
    expect_error(EczemaModel("Smoothing", max_score = max_score, prior = wrong_priors[[i]]))
  }

})

# AR1 ---------------------------------------------------------------------

test_that("AR1 constructor catches errors in prior", {

  wrong_priors <- list(
    1,
    list(sigma = c(0, 1), sigma = c(0, 1)),
    list(sigma = c("0", "0.1")),
    list(sigma = 0.1),
    list(sigma = c(0, -0.1)),
    list(slope = -c(1, 1)),
    list(y_inf = c(0.5, -0.25))
  )

  for (i in 1:length(wrong_priors)) {
    expect_error(EczemaModel("AR1", max_score = max_score, prior = wrong_priors[[i]]))
  }

})

# MixedAR1 ----------------------------------------------------------------

test_that("MixedAR1 constructor catches errors in prior", {

  wrong_priors <- list(
    1,
    list(sigma = c("0", "1")),
    list(sigma = 1),
    list(sigma = c(0, -1)),
    list(mu_logit_slope = c(0, -1)),
    list(sigma_logit_slope = c(0, -1)),
    list(mu_inf = c(0, -1)),
    list(sigma_inf = c(0, -1))
  )

  for (i in 1:length(wrong_priors)) {
    expect_error(EczemaModel("MixedAR1", max_score = max_score, prior = wrong_priors[[i]]))
  }

})

# BinMC -------------------------------------------------------------------

test_that("BinMC constructor catches errors in prior", {

  wrong_priors <- list(
    1:4,
    list(sigma = c("0", "1")),
    list(sigma = 1),
    list(sigma = c(0, -1)),
    list(mu_logit_p10 = c(0, -1)),
    list(sigma_logit_p10 = c(0, -1)),
    list(logit_tss1_0 = c(0, -1))
  )

  for (i in 1:length(wrong_priors)) {
    expect_error(EczemaModel("BinMC", max_score = max_score, prior = wrong_priors[[i]]))
  }

})

# BinRW -------------------------------------------------------------------

test_that("BinRW constructor catches errors in prior", {

  wrong_priors <- list(
    1:4,
    list(sigma = 1),
    list(sigma = c("0", "1")),
    list(sigma = c(0, -1)),
    list(mu_logit_y0 = c(0, -1)),
    list(sigma_logit_y0 = c(0, -1))
  )

  for (i in 1:length(wrong_priors)) {
    expect_error(EczemaModel("BinRW", max_score = max_score, prior = wrong_priors[[i]]))
  }

})

# OrderedRW ---------------------------------------------------------------

test_that("OrderedRW constructor catches errors in prior", {

  dprior <- EczemaModel("OrderedRW", max_score = max_score)$prior

  wrong_priors <- list(
    1:4,
    list(delta = as.character(dprior$delta)),
    list(delta = rep(-1, max_score - 1)),
    list(sigma_meas = 1),
    list(sigma_meas = c(0, -1)),
    list(sigma_lat = c(0, -1)),
    list(mu_y0 = c(0, -1)),
    list(mu_y0 = c(0, -1))
  )

  for (i in 1:length(wrong_priors)) {
    expect_error(EczemaModel("OrderedRW", max_score = max_score, prior = wrong_priors[[i]]))
  }

})

# MC ----------------------------------------------------------------------

wrong_prior_MC <- function(K) {
  # Make a list of incorrect prior inputs

  list(
    matrix(1, nrow = K, ncol = K),
    list(p = as.data.frame(matrix(1, nrow = K, ncol = K))),
    list(p = 1),
    list(p = matrix(-1, nrow = K, ncol = K))
  )

}

test_that("MC constructor catches errors in prior", {
  for (K in c(2, 5, 10)) {
    wrong_priors <- wrong_prior_MC(K)
    for (i in 1:length(wrong_priors)) {
      expect_error(EczemaModel("MC", K = K, prior = wrong_priors[[i]]))
    }
  }
})
