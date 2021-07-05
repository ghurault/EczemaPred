set.seed(2021)
options(warn = -1)

N_patient <- 30
t_max <- rpois(N_patient, 25)
max_score <- 100
param <- list_parameters("MixedAR1")

model <- EczemaModel("MixedAR1", max_score = max_score)

# Test incorrect priors ---------------------------------------------------

dprior <- model$prior

wrong_priors <- list(
  1,
  replace(dprior, which(names(dprior) == "sigma"), as.character(dprior$sigma)),
  replace(dprior, which(names(dprior) == "sigma"), 1),
  replace(dprior, which(names(dprior) == "sigma"), c(0, -1)),
  replace(dprior, which(names(dprior) == "mu_logit_slope"), c(0, -1)),
  replace(dprior, which(names(dprior) == "sigma_logit_slope"), c(0, -1)),
  replace(dprior, which(names(dprior) == "mu_inf"), c(0, -1)),
  replace(dprior, which(names(dprior) == "sigma_inf"), c(0, -1))
)

test_that("MixedAR1 constructor catches errors in prior", {
  for (i in 1:length(wrong_priors)) {
    expect_error(EczemaModel("MixedAR1", max_score = max_score, prior = wrong_priors[[i]]))
  }
})

# Test fitting ------------------------------------------------------

# Generate fake data manually for speed

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

test_that("EczemaFit returns a stanfit object", {
  expect_true(is_stanfit(fit))
})

test_that("estimate of population parameters from EczemaFit is accurate", {

  post <- rstan::extract(fit, pars = param$Population)
  post_mean <- vapply(post, mean, numeric(1))
  post_sd <- vapply(post, sd, numeric(1))
  truth <- c(sigma, mu_logit_slope, sigma_logit_slope, mu_inf, sigma_inf)

  expect_true(all(abs(post_mean - truth) / post_sd < 2.5))

})

test_that("estimates of slope from EczemaFit are accurate", {
  skip_on_ci()

  post_slope <- rstan::extract(fit, pars = "slope")[[1]]
  slope_mean <- apply(post_slope, 2, mean)
  slope_sd <- apply(post_slope, 2, sd)

  expect_true(all(abs(slope_mean - slope) / slope_sd < 2.5))
})

test_that("estimates of intercept from EczemaFit are accurate", {
  skip_on_ci()

  post_intercept <- rstan::extract(fit, pars = "intercept")[[1]]
  intercept_mean <- apply(post_intercept, 2, mean)
  intercept_sd <- apply(post_intercept, 2, sd)

  expect_true(all(abs(intercept_mean - intercept) / intercept_sd < 2.5))
})
