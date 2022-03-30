set.seed(2021)
options(warn = -1)

test_that("estimates of BinMC parameters by EczemaFit are accurate", {

  skip_on_cran()

  N_patient <- 10
  t_max <- rpois(N_patient, 20)
  max_score <- 100

  model <- EczemaModel("BinMC", max_score = max_score)

  fit0 <- sample_prior(model, N_patient = N_patient, t_max = t_max, chains = 1, refresh = 0)

  l <- extract_simulations(fit = fit0,
                           id = get_index2(t_max),
                           draw = 5,
                           pars = unlist(list_parameters("BinMC")))

  fit <- EczemaFit(model, train = l$Data, test = NULL, chains = 1, refresh = 0)

  par <- HuraultMisc::summary_statistics(fit,
                                         pars = unlist(list_parameters("BinMC")[c("Population", "Patient")])) %>%
    left_join(l$Parameters, by = c("Variable" = "Parameter", "Index")) %>%
    rename(True = Value) %>%
    mutate(Coverage90 = (True > `5%` & True < `95%`),
           NormError = abs(Mean - True) / sd)

  # Check sigma value
  par %>%
    filter(Variable == "sigma") %>%
    pull(NormError) %>%
    expect_lte(., 2.5)

})
