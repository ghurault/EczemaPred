set.seed(2021)
options(warn = -1)

N_patient <- 30
t_max <- rpois(N_patient, 25)
max_score <- 100
param_pop <- c("sigma", "mu_logit_alpha", "sigma_logit_alpha", "mu_inf", "sigma_inf")
param <- c(param_pop, "alpha", "y_inf", "b")

# Test default_prior_MixedAR1 ---------------------------------------------------

test_that("default_prior_MixedAR1 works", {
  expect_null(stopifnot_prior_MixedAR1(default_prior_MixedAR1()))
  expect_equal(default_prior_MixedAR1(), default_prior(model = "MixedAR1"))
})

dprior <- default_prior_MixedAR1()

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

# Test sample_prior_MixedAR1 ---------------------------------------------

test_that("sample_prior_MixedAR1 returns a stanfit object", {
  expect_true(is_stanfit(sample_prior_MixedAR1(max_score = max_score, chains = 1, refresh = 0)))
})

test_that("sample_prior_MixedAR1 catches errors in prior", {
  for (i in 1:length(wrong_priors)) {
    expect_error(sample_prior_MixedAR1(max_score = max_score, prior = wrong_priors[[i]]))
  }
})

# Test fit_MixedAR1 ------------------------------------------------------

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

df <- do.call(bind_rows,
              lapply(1:N_patient,
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
                     }))

fit <- fit_MixedAR1(df, max_score = max_score, chains = 1, refresh = 0)

test_that("fit_MixedAR1 returns a stanfit object", {
  expect_true(is_stanfit(fit))
})

test_that("estimate of population parameters from fit_MixedAR1 is accurate", {

  post <- rstan::extract(fit, pars = c("sigma", "mu_logit_alpha", "sigma_logit_alpha", "mu_inf", "sigma_inf"))
  post_mean <- sapply(post, mean)
  post_sd <- sapply(post, sd)
  truth <- c(sigma, mu_logit_alpha, sigma_logit_alpha, mu_inf, sigma_inf)

  expect_true(all(abs(post_mean - truth) / post_sd < 2.5))

})

if (FALSE) {
  test_that("estimates of alpha from fit_MixedAR1 are accurate", {

    post_alpha <- rstan::extract(fit, pars = "alpha")[[1]]
    alpha_mean <- apply(post_alpha, 2, mean)
    alpha_sd <- apply(post_alpha, 2, sd)

    expect_true(all(abs(alpha_mean - alpha) / alpha_sd < 2.5))

  })

  test_that("estimates of b from fit_MixedAR1 are accurate", {

    post_b <- rstan::extract(fit, pars = "b")[[1]]
    b_mean <- apply(post_b, 2, mean)
    b_sd <- apply(post_b, 2, sd)

    expect_true(all(abs(b_mean - b) / b_sd < 2.5))

  })
}

test_that("fit_MixedAR1 catches errors in prior", {
  # cf. stopifnot_prior_MixedAR1
  for (i in 1:length(wrong_priors)) {
    expect_error(fit_MixedAR1(train = l$Train, test = l$Test, max_score = max_score, prior = wrong_priors[[i]]))
  }
})
