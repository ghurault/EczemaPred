for (model_name in c("BinRW")) {
  for (max_score in c(10, 100)) {

    model <- EczemaModel(model_name, max_score = max_score)

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
}
