set.seed(2021)
options(warn = -1)

N_patient <- 10
t_max <- rpois(N_patient, 25)
max_score <- 100

# Test default_prior_BinMC ---------------------------------------------------

test_that("default_prior_BinMC works", {
  expect_null(stopifnot_prior_BinMC(default_prior_BinMC()))
  expect_equal(default_prior_BinMC(), default_prior(model = "BinMC"))
})

dprior <- default_prior_BinMC()

wrong_priors <- list(
  1:4,
  list(1, 2),
  c(dprior[names(dprior) != "sigma"], list(a = dprior$sigma)),
  c(dprior[names(dprior) != "sigma"], list(sigma = as.character(dprior$sigma))),
  c(dprior[names(dprior) != "sigma"], list(sigma = 1)),
  c(dprior[names(dprior) != "sigma"], list(sigma = c(0, -1))),
  c(dprior[names(dprior) != "mu_logit_p10"], list(mu_logit_y0 = c(0, -1))),
  c(dprior[names(dprior) != "sigma_logit_p10"], list(sigma_logit_y0 = c(0, -1))),
  c(dprior[names(dprior) != "logit_tss1_0"], list(logit_tss1_0 = c(0, -1)))
)

# Test sample_prior_BinMC ---------------------------------------------

fit0 <- sample_prior_BinMC(N_patient = N_patient, t_max = t_max, max_score = max_score, chains = 1, refresh = 0)

test_that("sample_prior_BinMC returns a stanfit object", {
  expect_true(is_stanfit(fit0))
})

test_that("sample_prior_BinMC catches errors in prior", {
  # cf. sample_prior_BinMC
  for (i in 1:length(wrong_priors)) {
    expect_error(sample_prior_BinMC(df, max_score = max_score, prior = wrong_priors[[i]]))
  }
})

# Test fit_BinMC ------------------------------------------------------

l <- extract_simulations(fit = fit0,
                         id = get_index2(t_max),
                         draw = 5,
                         pars = unlist(list_parameters("BinMC")))

fit <- fit_BinMC(train = l$Data, test = NULL, max_score = max_score, chains = 1, refresh = 0)

test_that("fit_BinMC returns a stanfit object", {
  expect_true(is_stanfit(fit))
})

par <- HuraultMisc::summary_statistics(fit,
                                       pars = unlist(list_parameters("BinMC")[c("Population", "Patient")])) %>%
  left_join(l$Parameters, by = c("Variable" = "Parameter", "Index")) %>%
  rename(True = Value) %>%
  mutate(Coverage90 = (True > `5%` & True < `95%`),
         NormError = abs(Mean - True) / sd)

test_that("sigma estimate from fit_BinMC is accurate", {
  par %>%
    filter(Variable == "sigma") %>%
    pull(NormError) %>%
    expect_lte(., 2.5)
})

test_that("fit_BinMC catches errors in prior", {
  # cf. stopifnot_prior_BinMC
  for (i in 1:length(wrong_priors)) {
    expect_error(fit_BinMC(train = l$Data, test = NULL, max_score = max_score, prior = wrong_priors[[i]]))
  }
})

# Test extract_parameters -------------------------------------------------

id <- get_index(l$Data)
par <- extract_parameters(fit, pars = list_parameters("BinMC"), id = id)

par <- list(
  extract_parameters(fit, pars = list_parameters("BinMC"), id = id),
  extract_parameters(fit, pars = list_parameters("BinMC")[c("Population", "PatientTime")], id = id)
)

test_that("extract_parameters works", {
  for (i in 1:length(par)) {
    expect_s3_class(par[[i]], "data.frame")
    expect_true(all(c("Patient", "Time", "Index", "Mean") %in% colnames(par[[i]])))
  }
})

test_that("extract_parameters extracts patient-dependent parameters", {
  par_p10 <- filter(par[[1]], Variable == "p10")
  expect_true(all(!is.na(par_p10[["Patient"]])))
  expect_true(all(is.na(par_p10[["Time"]])))
  expect_equal(nrow(par_p10), N_patient)

  par2_p10 <- extract_parameters(fit, pars = list("p10"))
  expect_equal(par2_p10, select(par_p10, -Patient, -Time))
})

test_that("extract_parameters extracts patient+time -dependent parameters", {
  par_ylat <- filter(par[[1]], Variable == "y_lat")
  expect_true(all(!is.na(unlist(par_ylat[, c("Patient", "Time")]))))
  expect_equal(nrow(par_ylat), nrow(id))
})
