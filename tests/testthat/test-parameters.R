# Test list_parameters ----------------------------------------------------

test_that("list_parameters works", {
  for (model_name in c("MC", "BinRW", "BinMC", "OrderedRW", "RW", "Smoothing", "AR1", "MixedAR1")) {
    pars <- list_parameters(model_name)
    expect_true(is.list(pars))
    expect_true(all(is.character(unlist(pars))))
  }
})

# Test extract_parameters -------------------------------------------------

# In test-model_BinMC.R
