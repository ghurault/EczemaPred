set.seed(2021)
options(warn = -1)

N_patient <- 10
t_max <- rpois(N_patient, 25)
max_score <- 100

param <- c("sigma", "mu_logit_y0", "sigma_logit_y0", "logit_y0")

model <- EczemaModel("BinRW", max_score = max_score)

# Test sample_prior ----------------------------------------------------

fit0 <- sample_prior(model, N_patient = N_patient, t_max = t_max, chains = 1, refresh = 0)

test_that("sample_prior returns a stanfit object", {
  expect_true(is_stanfit(fit0))
})

# Test fit ----------------------------------------------------

l <- extract_fakedata(fit = fit0,
                      id = get_index2(t_max),
                      draw = 5,
                      pars = param,
                      horizon = 2)

fit <- EczemaFit(model, train = l$Train, test = l$Test, chains = 1, refresh = 0)

id <- get_index(train = l$Train, test = l$Test)

test_that("EczemaFit returns a stanfit object", {
  expect_true(is_stanfit(fit))
})

par <- HuraultMisc::summary_statistics(fit, pars = param) %>%
  left_join(l$TrueParameters, by = c("Variable" = "Parameter", "Index")) %>%
  rename(True = Value) %>%
  mutate(Coverage90 = (True > `5%` & True < `95%`),
         NormError = abs(Mean - True) / sd)

test_that("sigma estimate from EczemaFit is accurate", {
  par %>%
    filter(Variable == "sigma") %>%
    pull(NormError) %>%
    expect_lte(., 2.5)
})
