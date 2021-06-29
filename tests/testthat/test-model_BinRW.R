set.seed(2021)
options(warn = -1)

N_patient <- 10
t_max <- rpois(N_patient, 25)
max_score <- 100

param <- c("sigma", "mu_logit_y0", "sigma_logit_y0", "logit_y0")

model <- EczemaModel("BinRW", max_score = max_score)

# Test incorrect priors ---------------------------------------------------

dprior <- model$prior

wrong_priors <- list(
  1:4,
  list(sigma = 1),
  # c(dprior[names(dprior) != "sigma"], list(a = dprior$sigma)),
  c(dprior[names(dprior) != "sigma"], list(sigma = as.character(dprior$sigma))),
  c(dprior[names(dprior) != "sigma"], list(sigma = 1)),
  c(dprior[names(dprior) != "sigma"], list(sigma = c(0, -1))),
  c(dprior[names(dprior) != "mu_logit_y0"], list(mu_logit_y0 = c(0, -1))),
  c(dprior[names(dprior) != "sigma_logit_y0"], list(sigma_logit_y0 = c(0, -1)))
)

test_that("BinRW constructor catches errors in prior", {
  for (i in 1:length(wrong_priors)) {
    expect_error(EczemaModel("BinRW", max_score = max_score, prior = wrong_priors[[i]]))
  }
})

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

# Test plot_ppc --------------------------------------------------

wrong_max_score <- list(
  as.character(max_score),
  rep(max_score, 2)
)

## Test plot_post_traj*
for (f in c(plot_post_traj_pmf, plot_post_traj_fanchart)) {

  test_that("plot_post_traj_* returns a ggplot object", {
    expect_s3_class(f(fit, id = id, patient_id = 1, max_score = max_score), "ggplot")
    expect_s3_class(f(rstan::extract(fit, pars = "y_rep")[[1]], id = id, patient_id = 1, max_score = max_score), "ggplot")
  })

  test_that("plot_post_traj_* catches errors in main inputs", {
    expect_error(f("fit", id = id, patient_id = 1, max_score = max_score))
    expect_error(f(rstan::extract(fit, pars = "y_rep")[[1]][, 1:t_max[1]], id = id, patient_id = 1, max_score = max_score))
    expect_error(f(fit, id = get_index(train = l$Train, test = NULL), patient_id = 1, max_score = max_score))
    expect_error(f(fit, id = id, patient_id = -1, max_score = max_score))
  })

}

test_that("plot_*_traj_pmf works when max_score is NA", {
  expect_s3_class(plot_ppc_traj_pmf(fit, train = l$Train, test = l$Test, patient_id = 1, max_score = NA), "ggplot")
})

test_that("plot_*_traj_pmf works when test is NULL", {
  expect_s3_class(plot_ppc_traj_pmf(fit, train = bind_rows(l$Train, l$Test), test = NULL, patient_id = 1, max_score = max_score), "ggplot")
})

test_that("add_trajectory works when missing values in df", {
  l$Train %>%
    mutate(Label = "Training") %>%
    slice_sample(prop = 0.5) %>%
    add_trajectory(df = .) %>%
    expect_s3_class(., "ggplot")
})

## Test plot_ppc_traj_*
for (g in c(plot_ppc_traj_pmf, plot_ppc_traj_fanchart)) {

  test_that("plot_ppc_traj_* returns a ggplot object", {
    expect_s3_class(g(fit, train = l$Train, test = l$Test, patient_id = 1, max_score = max_score), "ggplot")
    expect_s3_class(g(rstan::extract(fit, pars = "y_rep")[[1]], train = l$Train, test = l$Test, patient_id = 1, max_score = max_score), "ggplot")
  })

  test_that("plot_ppc_traj_* catches errors in main inputs", {
    expect_error(g("fit", train = l$Train, test = l$Test, patient_id = 1, max_score = max_score))
    expect_error(g(rstan::extract(fit, pars = "y_rep")[[1]][, 1:t_max[1]], train = l$Train, test = l$Test, patient_id = 1, max_score = max_score))
    expect_error(g(fit, train = l$Train, test = l$Test, patient_id = -1, max_score = max_score))
    expect_error(g(fit, train = l$Train, test = NULL, patient_id = 1, max_score = max_score))
  })

}

test_that("plot_*_traj_pmf warns catches errors and warnings in max_score", {
  for (wms in wrong_max_score) {
    expect_warning(plot_post_traj_pmf(fit, id = id, patient_id = 1, max_score = wms))
    expect_error(plot_ppc_traj_pmf(fit, train = l$Train, test = l$Test, patient_id = 1, max_score = wms))
  }
})

test_that("plot_*_traj_fanchart catches errors in max_score", {
  for (wms in wrong_max_score) {
    expect_error(plot_post_traj_fanchart(fit, id = id, patient_id = 1, max_score = wms))
    expect_error(plot_ppc_traj_fanchart(fit, train = l$Train, test = l$Test, patient_id = 1, max_score = wms))
  }
})

test_that("plot_*_traj_pmf catches errors with max_scale", {

  wrong_max_scale <- list(
    100,
    c(0, 1),
    0.01 / (max_score + 1)
  )

  for (wms in wrong_max_scale) {
    expect_error(plot_post_traj_pmf(fit, id = id, patient_id = 1, max_score = max_score, max_scale = wms))
    expect_error(plot_ppc_traj_pmf(fit, id = id, patient_id = 1, max_score = max_score, max_scale = wms))
  }

})

test_that("plot_*_traj_pmf warns when max_score is not a wholenumber and support is not supplied", {
  expect_warning(plot_post_traj_pmf(fit, id = id, patient_id = 1, max_score = max_score + 0.1))
  expect_warning(plot_ppc_traj_pmf(fit, train = l$Train, test = l$Test, patient_id = 1, max_score = max_score + 0.1))
})

test_that("plot_*_traj_fanchart returns a ggplot object for both eti and hdi intervals", {
  for (itv in c("eti", "hdi")) {
    expect_s3_class(plot_post_traj_fanchart(fit, id = id, patient_id = 1, max_score = max_score, interval = itv),
                    "ggplot")
    expect_s3_class(plot_ppc_traj_fanchart(fit, train = l$Train, test = l$Test, patient_id = 1, max_score = max_score, interval = itv),
                    "ggplot")
  }
})

test_that("plot_*_traj_fanchart catches errors with intervals", {
  expect_error(plot_post_traj_fanchart(fit, id = id, patient_id = 1, max_score = max_score, interval = "a"))
  expect_error(plot_ppc_traj_fanchart(fit, train = l$Train, test = l$Test, patient_id = 1, max_score = max_score, interval = "a"))
})

test_that("plot_*_traj_fanchart returns a ggplot object for a correct CI_level other than the default", {
  ci <- seq(0.2, 0.8, 0.2)
  expect_s3_class(plot_post_traj_fanchart(fit, id = id, patient_id = 1, max_score = max_score, CI_level = ci),
                  "ggplot")
  expect_s3_class(plot_ppc_traj_fanchart(fit, train = l$Train, test = l$Test, patient_id = 1, max_score = max_score, CI_level = ci),
                  "ggplot")
})

test_that("plot_*_traj_fanchart catches errors with CI_level", {
  wrong_ci <- list(
    c("0.2", "0.4"),
    0.9,
    seq(5, 95, 5)
  )
  for (wci in wrong_ci) {
    expect_error(plot_ppc_traj_fanchart(fit, train = l$Train, test = l$Test, patient_id = 1, max_score = max_score, CI_level = wci))
  }
})