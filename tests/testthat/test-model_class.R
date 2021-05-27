# General tests -----------------------------------------------------------

for (model_name in c("BinRW", "OrderedRW", "BinMC", "RW")) {
  for (max_score in c(10, 100)) {

    model <- EczemaModel(model_name,
                         max_score = max_score,
                         discrete = !(model_name %in% c("RW") && max_score > 50))

    test_that(paste0("We can construct a ", model_name, " object"), {
      expect_true(all(c(model_name, "EczemaModel") %in% class(model)))
    })

    test_that(paste0("The default prior of ", model_name, " is a correct prior"), {
      model$prior <- default_prior(model)
      expect_null(validate_prior(model))
    })

    test_that("The print method returns invisible", {
      expect_output(print(model), paste0("^", model_name, " model"))
    })

  }

  test_that(paste0("Missing max_score argument when constructing ", model_name, "throws an error"), {
    expect_error(EczemaModel(model_name))
  })

  test_that(paste0("Incorrect max_score when constructing ", model_name, "throws an error"), {
    # Change when including continuous models
    wrong_maxscore <- list(0,
                           c(10, 10),
                           10.1)
    for (wms in wrong_maxscore) {
      expect_error(EczemaModel(model_name, max_score = wms))
    }

  })

}

# Test MC separately
