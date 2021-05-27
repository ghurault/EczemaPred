set.seed(2021)
options(warn = -1)

N_patient <- 30
t_max <- rpois(N_patient, 25)
max_score <- 100
param_pop <- c("sigma", "mu_logit_alpha", "sigma_logit_alpha", "mu_inf", "sigma_inf")
param <- c(param_pop, "alpha", "y_inf", "b")

model <- EczemaModel("MixedAR1", max_score = max_score)

# Test incorrect priors ---------------------------------------------------

dprior <- model$prior

wrong_priors <- list(
  1,
  list(1),
  as.list(1:5),
  replace(dprior, which(names(dprior) == "sigma"), as.character(dprior$sigma)),
  replace(dprior, which(names(dprior) == "sigma"), 1),
  replace(dprior, which(names(dprior) == "sigma"), c(0, -1)),
  replace(dprior, which(names(dprior) == "mu_logit_alpha"), c(0, -1)),
  replace(dprior, which(names(dprior) == "sigma_logit_alpha"), c(0, -1)),
  replace(dprior, which(names(dprior) == "mu_inf"), c(0, -1)),
  replace(dprior, which(names(dprior) == "sigma_inf"), c(0, -1))
)

test_that("MixedAR1 constructor catches errors in prior", {
  for (i in 1:length(wrong_priors)) {
    expect_error(EczemaModel("MixedAR1", max_score = max_score, prior = wrong_priors[[i]]))
  }
})

# Test sample_prior ---------------------------------------------

test_that("sample_prior returns a stanfit object", {
  fit0 <- sample_prior(model, chains = 1, iter = 50, refresh = 0)
  expect_true(is_stanfit(fit0))
})

# Test fitting ------------------------------------------------------

# Generate fake data manually for speed

y0 <- rbeta(N_patient, 5, 5) * max_score
sigma <- 2

mu_logit_alpha <- 1
sigma_logit_alpha <- 1
alpha <- HuraultMisc::inv_logit(rnorm(N_patient, mu_logit_alpha, sigma_logit_alpha))

mu_inf <- 0.1 * max_score
sigma_inf <- 0.05 * max_score
y_inf <- rnorm(N_patient, mu_inf, sigma_inf)

b <- y_inf * (1 - alpha)

df <- lapply(1:N_patient,
             function(i) {

               err <- rnorm(t_max[i] - 1, 0, sigma)
               y <- rep(NA, t_max[i])
               y[1] <- y0[i]
               for (t in 2:t_max[i]) {
                 y[t] = alpha[i] * y[t - 1] + b[i] + err[t - 1]
               }

               out <- data.frame(Patient = i,
                                 Time = 1:t_max[i],
                                 Score = y) %>%
                 mutate(OutsideRange = (Score < 0 | Score > max_score),
                        OutsideRange = cumsum(OutsideRange)) %>%
                 filter(OutsideRange == 0)

               return(out)
             }) %>%
  bind_rows()

fit <- EczemaFit(model, train = df, chains = 1, refresh = 0)

test_that("EczemaFit returns a stanfit object", {
  expect_true(is_stanfit(fit))
})

test_that("estimate of population parameters from EczemaFit is accurate", {

  post <- rstan::extract(fit, pars = c("sigma", "mu_logit_alpha", "sigma_logit_alpha", "mu_inf", "sigma_inf"))
  post_mean <- sapply(post, mean)
  post_sd <- sapply(post, sd)
  truth <- c(sigma, mu_logit_alpha, sigma_logit_alpha, mu_inf, sigma_inf)

  expect_true(all(abs(post_mean - truth) / post_sd < 2.5))

})

test_that("estimates of alpha from EczemaFit are accurate", {
  skip_on_ci()

  post_alpha <- rstan::extract(fit, pars = "alpha")[[1]]
  alpha_mean <- apply(post_alpha, 2, mean)
  alpha_sd <- apply(post_alpha, 2, sd)

  expect_true(all(abs(alpha_mean - alpha) / alpha_sd < 2.5))
})

test_that("estimates of b from EczemaFit are accurate", {
  skip_on_ci()

  post_b <- rstan::extract(fit, pars = "b")[[1]]
  b_mean <- apply(post_b, 2, mean)
  b_sd <- apply(post_b, 2, sd)

  expect_true(all(abs(b_mean - b) / b_sd < 2.5))
})
