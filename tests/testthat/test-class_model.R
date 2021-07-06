# Initialisation ----------------------------------------------------------

options(warn = -1)

main_models <- c("BinRW", "OrderedRW", "BinMC")
ref_models <- c("RW", "Smoothing", "AR1", "MixedAR1")

# Test EczemaModel --------------------------------------------------------------------

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

  if (model_name == "MC") {

    test_that("We can change priors in MC", {
      K <- 5
      prior <- list(p = matrix(2, nrow = K, ncol = K))
      model <- EczemaModel("MC", K = K, prior = prior)
      expect_equal(model$prior, prior)
    })

    test_that("Incorrect K when constructing MC model", {
      wrong_K <- list(NULL, 1, -1, c(5, 5), "2")
      for (k in seq_along(wrong_K)) {
        expect_error(EczemaModel("MC", K = wrong_K[[i]]))
      }
    })

  }

}

# Test inference methods --------------------------------------------------
# Similar tests for MC are located in test-model_MC.R

ms <- 10

N_pt <- 10
t_max <- rpois(N_pt, 20)
df <- lapply(1:N_pt,
             function(i) {
               data.frame(Patient = i,
                          Time = 1:t_max[i],
                          Score = rbinom(t_max[i], ms, .5))
             }) %>%
  bind_rows()
train <- df %>% filter(Time <= 20)
test <- df %>% filter(Time > 20)

cond <- expand_grid(Model = c(main_models, ref_models),
                  Discrete = c(TRUE, FALSE)) %>%
  filter(Discrete | Model %in% ref_models)

for (i in 1:nrow(cond)) {

  model <- EczemaModel(cond$Model[i], max_score = ms, discrete = cond$Discrete[i])

  test_that("sample_prior returns a stanfit object", {
    fit0 <- sample_prior(model, chains = 1, iter = 2, refresh = 0)
    expect_true(is_stanfit(fit0))
  })

  test_that("EczemaFit returns a stanfit object", {
    fit1 <- EczemaFit(model, train = df, chains = 1, iter = 2, refresh = 0)
    fit2 <- EczemaFit(model, train = train, test = test, chains = 1, iter = 2, refresh = 0)
    expect_true(is_stanfit(fit1))
    expect_true(is_stanfit(fit2))
  })

}
