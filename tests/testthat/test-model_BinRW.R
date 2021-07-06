set.seed(2021)
options(warn = -1)

test_that("estimates of BinRW by EczemaFit are accurate", {

  skip_on_cran()

  N_patient <- 10
  t_max <- rpois(N_patient, 25)
  max_score <- 100

  param <- c("sigma", "mu_logit_y0", "sigma_logit_y0", "logit_y0")

  model <- EczemaModel("BinRW", max_score = max_score)

  fit0 <- sample_prior(model, N_patient = N_patient, t_max = t_max, chains = 1, refresh = 0)

  l <- extract_fakedata(fit = fit0,
                        id = get_index2(t_max),
                        draw = 5,
                        pars = param,
                        horizon = 2)

  fit <- EczemaFit(model, train = l$Train, test = l$Test, chains = 1, refresh = 0)

  par <- HuraultMisc::summary_statistics(fit, pars = param) %>%
    left_join(l$TrueParameters, by = c("Variable" = "Parameter", "Index")) %>%
    rename(True = Value) %>%
    mutate(Coverage90 = (True > `5%` & True < `95%`),
           NormError = abs(Mean - True) / sd)

  # Check sigma values
  par %>%
    filter(Variable == "sigma") %>%
    pull(NormError) %>%
    expect_lte(., 2.5)

})
