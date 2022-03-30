# Test add_prior ----------------------------------------------------------

test_that("add_prior works", {

  data_stan <- list()
  val_mu <- 1
  val_eps <- matrix(stats::rnorm(4), nrow = 2, ncol = 2)
  prior <- list(mu = val_mu, eps = val_eps)

  x <- add_prior(data_stan, prior)

  expect_true(is.list(x))
  expect_equal(length(x), length(data_stan) + length(prior))
  expect_true("prior_mu" %in% names(x))
  expect_equal(x$prior_mu, val_mu)
  expect_equal(x$prior_eps, val_eps)

})

# Test get_compiled_model -------------------------------------------------

test_that("get_compiled_model works", {
  expect_true(class(get_compiled_model("BinRW")) == "stanmodel")
  expect_error(get_compiled_model("model_not_existing"))
})

# Test samples_to_list ----------------------------------------------------

# In test-predictions.R

# Test get_index ----------------------------------------------------------

test_that("get_index works", {

  N_pt <- 10
  t_max <- rpois(N_pt, 20)

  id2 <- get_index2(t_max)

  df <- lapply(1:N_pt,
               function(i) {
                 data.frame(Patient = i, Time = 1:t_max[i]) %>%
                   filter(!generate_missing(n()))
               }) %>%
    bind_rows()
  id <- get_index(df %>% filter(Time <= 10),
                  df %>% filter(Time > 10))

  for (x in list(id2, id)) {
    expect_s3_class(x, "data.frame")
    expect_true(all(c("Patient", "Time", "Index") %in% colnames(x)))
    expect_equal(nrow(x), sum(t_max))
    expect_equal(length(unique(x[["Index"]])), nrow(x))
  }

})

# Test extract_simulation -------------------------------------------------

id <- get_index(RW_split$Training, RW_split$Testing)
param <- c("sigma", "y_mis")

l <- extract_simulations(fit = RW_fit,
                         id = id,
                         draw = 10,
                         pars = param)

test_that("extract_simulations works", {
  expect_true(is.list(l))
  expect_true(all(c("Data", "Parameters") %in% names(l)))
  lapply(l, function(x) {expect_s3_class(x, "data.frame")})
  expect_true(all(c("Patient", "Time", "Score") %in% colnames(l$Data)))
  expect_true(all(c("Draw", "Index", "Value", "Parameter") %in% colnames(l$Parameters)))
})

test_that("extract_simulations catches errors in inputs", {
  expect_error(extract_simulations(fit = rstan::extract(RW_fit, pars = "y_rep"), id = id, draw = 10, pars = param))
  expect_error(extract_simulations(fit = RW_fit, id = RW_setup$t_max, draw = 10, pars = param))
  expect_error(extract_simulations(fit = RW_fit, id = id, draw = -1, pars = param))
  expect_error(extract_simulations(fit = RW_fit, id = id, draw = 10, pars = y_rep))
})
