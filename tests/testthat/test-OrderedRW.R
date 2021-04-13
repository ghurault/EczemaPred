set.seed(2021)
options(warn = -1)

N_patient <- 10
t_max <- rpois(N_patient, 25)
param <- c("delta", "sigma", "mu_y0", "sigma_y0", "y0")
max_score <- 5

# Test default_prior_OrderedRW ---------------------------------------------------

test_that("default_prior_OrderedRW works", {
  for (m in seq(2, 10, 2)) {
    expect_null(stopifnot_prior_OrderedRW(default_prior_OrderedRW(m), m))
  }
})

dprior <- default_prior_OrderedRW(max_score)

wrong_priors <- list(
  1:4,
  as.list(1:4),
  c(dprior[names(dprior) != "delta"], list(a = dprior$delta)),
  c(dprior[names(dprior) != "delta"], list(delta = matrix(rep(c(0, 3.5), max_score - 1)))),
  c(dprior[names(dprior) != "delta"], list(delta = as.character(dprior$delta))),
  c(dprior[names(dprior) != "delta"], list(delta = matrix(rep(c(0, -3.5), max_score - 1),
                                                          nrow = 2, byrow = FALSE))),
  c(dprior[names(dprior) != "sigma"], list(sigma = 1)),
  c(dprior[names(dprior) != "sigma"], list(sigma = c(0, -1))),
  c(dprior[names(dprior) != "mu_y0"], list(mu_y0 = c(0, -1))),
  c(dprior[names(dprior) != "sigma_y0"], list(sigma_y0 = c(0, -1)))
)

# Test sample_prior_OrderedRW ---------------------------------------------

fit0 <- sample_prior_OrderedRW(N_patient = N_patient, t_max = t_max, max_score = max_score, chains = 1, refresh = 0)

test_that("sample_prior_OrderedRW returns a stanfit object", {
  expect_true(is_stanfit(fit0))
})

test_that("sample_prior_OrderedRW catches errors in prior", {
  # cf. stopifnot_prior_OrderedRW
  for (i in 1:length(wrong_priors)) {
    expect_error(sample_prior_OrderedRW(df, max_score = max_score, prior = wrong_priors[[i]]))
  }
})

# Test fit_OrderedRW ------------------------------------------------------

l <- extract_fakedata(fit_prior = fit0,
                      draw = 5,
                      pars = param,
                      N_patient = N_patient,
                      t_max = t_max,
                      horizon = 2)

fit <- fit_OrderedRW(train = l$Train, test = l$Test, max_score = max_score, chains = 1, refresh = 0)

test_that("fit_OrderedRW returns a stanfit object", {
  expect_true(is_stanfit(fit))
})

par <- HuraultMisc::summary_statistics(fit, pars = param) %>%
  left_join(l$TrueParameters, by = c("Variable" = "Parameter", "Index")) %>%
  rename(True = Value) %>%
  mutate(Coverage90 = (True > `5%` & True < `95%`),
         NormError = abs(Mean - True) / sd)

test_that("fit_OrderedRW has 90% coverage over all parameters", {
  par %>%
    summarise(Coverage90 = mean(Coverage90),
              SE = sqrt(Coverage90 * (1 - Coverage90) / n()),
              Valid = abs(Coverage90 - 0.9) < 2.5 * SE) %>%
    pull(Valid) %>%
    expect_true()
})

test_that("fit_OrderedRW catches errors in prior", {
  # cf. stopifnot_prior_OrderedRW
  for (i in 1:length(wrong_priors)) {
    expect_error(fit_BinRW(train = l$Train, test = l$Test, max_score = max_score, prior = wrong_priors[[i]]))
  }
})

# Test plot_latent_OrderedRW ----------------------------------------------

test_that("plot_latent_OrderedRW returns a ggplot object", {
  expect_s3_class(plot_latent_OrderedRW(fit, id = get_index(train = l$Train, test = l$Test), patient_id = 1), "ggplot")
})

test_that("plot_latent_OrderedRW catches errors in inputs", {
  expect_error(plot_latent_OrderedRW(rstan::extract(fit, pars = "y_rep"), id = get_index(train = l$Train), patient_id = 1))
  expect_error(plot_latent_OrderedRW(fit, id = get_index(train = l$Train), patient_id = 1))
  expect_error(plot_latent_OrderedRW(fit, id = get_index(train = l$Train, test = l$Test), patient_id = N_patient + 1))
})
