set.seed(2021)
options(warn = -1)

N_patient <- 10
t_max <- rpois(N_patient, 50)
max_score <- 100
y0 <- round(rbeta(N_patient, 5, 5) * max_score)
sigma <- 2

model <- EczemaModel("RW", max_score = max_score, discrete = FALSE)

# Test incorrect priors ---------------------------------------------------

wrong_priors <- list(
  1,
  list(sigma = c(0, 1), sigma = c(0, 1)),
  list(sigma = 1),
  list(sigma = c("0", "1"))
)

test_that("RW constructor catches errors in prior", {
  for (i in 1:length(wrong_priors)) {
    expect_error(EczemaModel("RW", max_score = max_score, discrete = FALSE, prior = wrong_priors[[i]]))
  }
})

# Test fitting -------------------------------------------------------------

df <- lapply(1:N_patient,
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
             }) %>%
  bind_rows()

fit <- EczemaFit(model, train = df, chains = 1, refresh = 0)

test_that("EczemaFit returns a stanfit object", {
  expect_true(is_stanfit(fit))
})

test_that("estimate of sigma from EczemaFit is accurate", {

  post_sigma <- rstan::extract(fit, pars = "sigma")[[1]]
  sigma_mean <- mean(post_sigma)
  sigma_sd <- sd(post_sigma)

  expect_lt(abs(sigma - sigma_mean), 2.5 * sigma_sd)

})
