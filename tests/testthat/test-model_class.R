
main_models <- c("BinRW", "OrderedRW", "BinMC")
ref_models <- c("RW", "Smoothing", "AR1", "MixedAR1")

for (model_name in c(main_models, ref_models, "MC")) {

  ms <- c(10, 100)
  ks <- c(5, 10)

  for (i in 1:length(ms)) {

    if (model_name == "MC") {
      model <- EczemaModel(model_name, K = ks[i])
    } else {
      model <- EczemaModel(model_name,
                           max_score = ms[i],
                           discrete = !(model_name %in% c(ref_models) && ms[i] > 50))
    }

    test_that(paste0("We can construct a ", model_name, " object"), {
      expect_true(all(c(model_name, "EczemaModel") %in% class(model)))
    })

    test_that("Content of the EczemaModel is correct", {
      expect_equal(model$name, model_name)
      if (model_name == "MC") {
        expect_equal(model$K, ks[i])
      } else {
        expect_equal(model$max_score, ms[i])
      }
    })

    test_that(paste0("The default prior of ", model_name, " is a correct prior"), {
      model$prior <- default_prior(model)
      expect_null(validate_prior(model))
    })

    test_that("The print method returns invisible", {
      expect_output(print(model), paste0("^", model_name, " model"))
    })

    test_that("The discrete attributes of model is correct", {
      if (model_name %in% c("BinRW", "OrderedRW", "BinMC", "MC")) {
        expect_true(model$discrete)
      }
      if (model_name %in% ref_models) {
        expect_equal(model$discrete, !(ms[i] > 50))
      }
    })

  }

  if (model_name != "MC") {

    test_that(paste0("Incorrect max_score when constructing ", model_name, "throws an error"), {
      wrong_maxscore <- list(NULL,
                             0,
                             c(10, 10))
      if (model_name %in% c(main_models, "MixedAR1")) {
        wrong_maxscore <- c(wrong_maxscore, list(10.1))
      }
      for (wms in wrong_maxscore) {
        expect_error(EczemaModel(model_name, max_score = wms))
      }
    })

    test_that("We can change priors", {

      model0 <- EczemaModel(model_name, max_score = 10)

      new_prior <- list(c(0, 10.2))
      par_name <- ifelse(model_name == "OrderedRW", "sigma_lat", "sigma")
      names(new_prior) <- par_name

      model1 <- EczemaModel(model_name, max_score = 10, prior = new_prior)
      model2 <- expect_warning(EczemaModel(model_name, max_score = 10, prior = list(parameter_not_in_model = new_prior[[1]])))

      expect_equal(model1$prior[[par_name]], new_prior[[1]])
      expect_equal(model0$prior, model2$prior)

    })

  }

}
