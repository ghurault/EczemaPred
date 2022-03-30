# Test list_parameters ----------------------------------------------------

test_that("list_parameters works", {
  for (model_name in c("MC", "BinRW", "BinMC", "OrderedRW", "RW", "Smoothing", "AR1", "MixedAR1")) {
    pars <- list_parameters(model_name)
    expect_true(is.list(pars))
    expect_true(all(is.character(unlist(pars))))
  }
})

# Test extract_parameters -------------------------------------------------

id <- get_index(RW_split$Training, RW_split$Testing)

param <- list_parameters("RW")
param$Patient <- param$Test # test parameters in RW_fit are like patient parameters

par <- list(
  extract_parameters(RW_fit, pars = param, id = id),
  extract_parameters(RW_fit, pars = param[c("Population", "PatientTime")], id = id)
)

par2 <- list(
  extract_parameters(RW_fit, id = id),
  extract_parameters(RW_fit)
)

test_that("extract_parameters works", {
  for (i in 1:length(par)) {
    expect_s3_class(par[[i]], "data.frame")
    expect_true(all(c("Patient", "Time", "Index", "Mean") %in% colnames(par[[i]])))
  }
  for (i in 1:length(par2)) {
    expect_s3_class(par[[i]], "data.frame")
  }
})

test_that("extract_parameters extracts patient-dependent parameters", {
  # NB: test parameters in RW_fit are like patient parameters

  sub_par <- filter(par[[1]], Variable == "lpd")
  expect_true(all(!is.na(sub_par[["Patient"]])))
  expect_true(all(is.na(sub_par[["Time"]])))
  expect_equal(nrow(sub_par), RW_setup$N_patient)

  sub_par2 <- extract_parameters(RW_fit, pars = list("lpd"))
  expect_equal(sub_par2, select(sub_par, -Patient, -Time))
})

test_that("extract_parameters extracts patient+time -dependent parameters", {
  par_yrep <- filter(par[[1]], Variable == "y_rep")
  expect_true(all(!is.na(unlist(par_yrep[, c("Patient", "Time")]))))
  expect_equal(nrow(par_yrep), nrow(id))
})
