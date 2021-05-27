set.seed(2021)
options(warn = -1)

N_patient <- 10
t_max <- rpois(N_patient, 25)
param <- c("delta", "sigma", "mu_y0", "sigma_y0", "y0")
max_score <- 5

model <- EczemaModel("OrderedRW", max_score = max_score)

# Test incorrect priors ---------------------------------------------------

dprior <- model$prior

wrong_priors <- list(
  1:4,
  as.list(1:4),
  c(dprior[names(dprior) != "delta"], list(a = dprior$delta)),
  c(dprior[names(dprior) != "delta"], list(delta = as.character(dprior$delta))),
  c(dprior[names(dprior) != "delta"], list(delta = matrix(rep(c(0, -3.5), max_score - 1),
                                                          nrow = 2, byrow = FALSE))),
  c(dprior[names(dprior) != "sigma"], list(sigma = 1)),
  c(dprior[names(dprior) != "sigma"], list(sigma = c(0, -1))),
  c(dprior[names(dprior) != "mu_y0"], list(mu_y0 = c(0, -1))),
  c(dprior[names(dprior) != "sigma_y0"], list(sigma_y0 = c(0, -1)))
)

test_that("OrderedRW constructor catches errors in prior", {
  for (i in 1:length(wrong_priors)) {
    expect_error(EczemaModel("OrderedRW", max_score = max_score, prior = wrong_priors[[i]]))
  }
})

# Test sample_prior ---------------------------------------------

fit0 <- sample_prior(model, N_patient = N_patient, t_max = t_max, chains = 1, refresh = 0)

test_that("sample_prior returns a stanfit object", {
  expect_true(is_stanfit(fit0))
})

# Test fit ------------------------------------------------------

l <- extract_simulations(fit = fit0,
                         id = get_index2(t_max),
                         draw = 5,
                         pars = param)

fit <- EczemaFit(model, train = l$Data, test = NULL, chains = 1, refresh = 0)

test_that("EczemaFit returns a stanfit object", {
  expect_true(is_stanfit(fit))
})

par <- HuraultMisc::summary_statistics(fit, pars = param) %>%
  left_join(l$Parameters, by = c("Variable" = "Parameter", "Index")) %>%
  rename(True = Value) %>%
  mutate(Coverage90 = (True > `5%` & True < `95%`),
         NormError = abs(Mean - True) / sd)

test_that("OrderedRW has 90% coverage over all parameters", {
  par %>%
    summarise(Coverage90 = mean(Coverage90),
              SE = sqrt(Coverage90 * (1 - Coverage90) / n()),
              Valid = abs(Coverage90 - 0.9) < 2.5 * SE) %>%
    pull(Valid) %>%
    expect_true()
})

# Test plot_latent_OrderedRW ----------------------------------------------

test_that("plot_latent_OrderedRW returns a ggplot object", {
  expect_s3_class(plot_latent_OrderedRW(fit, id = get_index(train = l$Data, test = l$Test), patient_id = 1), "ggplot")
})

test_that("plot_latent_OrderedRW catches errors in inputs", {
  id <- get_index(l$Data)
  expect_error(plot_latent_OrderedRW(rstan::extract(fit, pars = "y_rep"), id = id, patient_id = 1))
  expect_error(plot_latent_OrderedRW(fit, id = filter(id, Patient > 1), patient_id = 1))
  expect_error(plot_latent_OrderedRW(fit, id = id, patient_id = N_patient + 1))
})
