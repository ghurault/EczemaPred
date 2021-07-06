set.seed(2021)
options(warn = -1)

test_that("OrderedRW inference are correct and plot function works", {

  skip_on_cran()

  N_patient <- 10
  t_max <- rpois(N_patient, 25)
  param <- list_parameters("OrderedRW")[c("Population", "Patient")]
  max_score <- 5

  model <- EczemaModel("OrderedRW", max_score = max_score)

  fit0 <- sample_prior(model, N_patient = N_patient, t_max = t_max, chains = 1, refresh = 0)

  expect_true(is_stanfit(fit0)) # need it otherwise empty test (top level)

  l <- extract_simulations(fit = fit0,
                           id = get_index2(t_max),
                           draw = 5,
                           pars = unlist(param))

  fit <- EczemaFit(model, train = l$Data, test = NULL, chains = 1, refresh = 0)

  expect_true(is_stanfit(fit)) # need it otherwise empty test (top level)

  par <- HuraultMisc::summary_statistics(fit, pars = unlist(param)) %>%
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

  test_that("plot_latent_OrderedRW returns a ggplot object", {
    expect_s3_class(plot_latent_OrderedRW(fit, id = get_index(train = l$Data, test = l$Test), patient_id = 1), "ggplot")
  })

  test_that("plot_latent_OrderedRW catches errors in inputs", {
    id <- get_index(l$Data)
    expect_error(plot_latent_OrderedRW(rstan::extract(fit, pars = "y_rep"), id = id, patient_id = 1))
    expect_error(plot_latent_OrderedRW(fit, id = filter(id, Patient > 1), patient_id = 1))
    expect_error(plot_latent_OrderedRW(fit, id = id, patient_id = N_patient + 1))
  })

})
