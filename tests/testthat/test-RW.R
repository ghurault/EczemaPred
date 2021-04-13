set.seed(2021)
options(warn = -1)

wrong_priors <- list(1,
                     list(sigma_mean = 0, sigma_sd = 1),
                     list(sigma = 1),
                     list(sigma = c("0", "1")),
                     list(sigma_sd = c(0, -1)))

# Test default_prior_RW ---------------------------------------------------

test_that("default_prior_RW works", {
  expect_null(stopifnot_prior_RW(default_prior_RW()))
})

# Test fit_RW -------------------------------------------------------------

N_patient <- 10
t_max <- rpois(N_patient, 50)
max_score <- 100
y0 <- round(rbeta(N_patient, 5, 5) * max_score)
sigma <- 2

df <- do.call(bind_rows,
              lapply(1:N_patient,
                     function(i) {
                       # Generate random walk
                       # Truncate time-series if outside range
                       # Generate missing values

                       data.frame(Patient = i,
                                  Time = 1:t_max[i],
                                  Score = y0[i] + cumsum(c(0, rnorm(t_max[i] - 1, 0, sigma)))) %>%
                         mutate(OutsideRange = (Score < 0 | Score > max_score),
                                OutsideRange = cumsum(OutsideRange)) %>%
                         filter(OutsideRange == 0) %>%
                         mutate(Missing = generate_missing(n())) %>%
                         filter(!Missing)
                     }))

fit <- fit_RW(df, max_score = max_score, discrete = FALSE, chains = 1, refresh = 0)

test_that("fit_RW returns a stanfit object", {
  expect_true(is_stanfit(fit))
})

test_that("estimate of sigma from fit_RW is accurate", {

  post_sigma <- rstan::extract(fit, pars = "sigma")[[1]]
  sigma_mean <- mean(post_sigma)
  sigma_sd <- sd(post_sigma)

  expect_lt(abs(sigma - sigma_mean), 2.5 * sigma_sd)

})

test_that("fit_RW catches errors in prior", {
  # cf. stopifnot_prior_RW
  for (i in 1:length(wrong_priors)) {
    expect_error(fit_RW(df, max_score = max_score, discrete = FALSE, prior = wrong_priors[[i]]))
  }
})

# Test sample_prior_RW ----------------------------------------------------

fit_prior <- sample_prior_RW(max_score = 100, discrete = FALSE, chains = 1, refresh = 0)

test_that("sample_prior_RW returns a stanfit object", {
  expect_true(is_stanfit(fit_prior))
})

test_that("sample_prior_RW catches errors in prior", {
  for (i in 1:length(wrong_priors)) {
    expect_error(sample_prior_RW(max_score = 100, discrete = FALSE, prior = wrong_priors[[i]]))
  }
})
