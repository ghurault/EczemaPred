# Test get_fc_iteration ---------------------------------------------------

test_that("get_fc_iteration works", {
  for (h in c(1, 3, 5)) {
    expect_equal(get_fc_iteration(c(1, 1 + h, 2 + h), h), c(0, 1, 2))
  }
})

test_that("get_fc_iteration catches errors in t", {
  wrong_t <- list(-1, 1.5)
  for (wt in wrong_t) {
    expect_error(get_fc_iteration(wt, 2))
  }
})

test_that("get_fc_iteration catches errors in horizon", {
  wrong_horizon <- list(1.5, c(1, 2), -1)
  for (wh in wrong_horizon) {
    expect_error(get_fc_iteration(1:5, wh))
  }
})

# Test split_fc_dataset ---------------------------------------------------

h <- 5
df <- get_index2(t_max = rpois(10, 10)) %>%
  mutate(Score = rnorm(nrow(.)),
         Iteration = get_fc_iteration(Time, h))

test_that("split_fc_dataset works", {
  sp1 <- split_fc_dataset(df, 1)
  expect_type(sp1, "list")
  expect_true(all(c("Training", "Testing") %in% names(sp1)))
  expect_lte(max(sp1$Training[["Iteration"]]), 1)
  expect_true(all(sp1$Testing[["Iteration"]] == 1))
  expect_equal(max(sp1$Training[["Time"]]) + sp1$Testing[["Horizon"]], sp1$Testing[["Time"]])
  expect_true(all(c("LastTime", "LastScore") %in% colnames(sp1$Testing)))
})

test_that("split_fc_dataset catches errors in df", {
  wrong_input <- list(as.matrix(df),
                      rename(df, Day = Time),
                      rename(df, y = Score),
                      select(df, -Iteration))
  for (wi in wrong_input) {
    expect_error(split_fc_dataset(wi, 1))
  }
})

test_that("split_fc_dataset catches errors in it", {
  wrong_input <- list(c(1, 2), 1.5, -1)
  for (wi in wrong_input) {
    expect_error(split_fc_dataset(df, wi))
  }
})

# Test get_fc_training_iteration ------------------------------------------

test_that("get_fc_training_iteration works", {
  it <- c(0, 0, 1, 3, 5)
  sol <- c(0, 2, 4)
  expect_equal(get_fc_training_iteration(it), sol)
})

test_that("get_fc_training_iteration catches error in inputs", {
  expect_error(get_fc_training_iteration(c("0", "1")))
  expect_error(get_fc_training_iteration(c(0, 1.5)))
})

# Test detail_fc_dataset --------------------------------------------------

test_that("detail_fc_training works", {
  dt <- df %>%
    select(-Patient, -Iteration, -Score) %>%
    detail_fc_training(., h)
  expect_true(all(c("Iteration", "N", "Proportion", "LastTime") %in% colnames(dt)))
  expect_equal(dt[["Proportion"]], dt[["N"]] / nrow(df))

  sp1 <- split_fc_dataset(df, 1)
  expect_equal(dt %>% filter(Iteration == 1) %>% pull(LastTime), max(sp1$Training[["Time"]]))
  expect_equal(dt %>% filter(Iteration == 1) %>% pull(N), nrow(sp1$Training))
})

test_that("detail_fc_training catches errors in df", {
  wrong_input <- list(as.matrix(df),
                      rename(df, Day = Time))
  for (wi in wrong_input) {
    expect_error(detail_fc_training(wi, 2))
  }
})

test_that("detail_fc_training catches errors in horizon", {
  wrong_horizon <- list(1.5, c(1, 2), -1)
  for (wh in wrong_horizon) {
    expect_error(detail_fc_training(df, wh))
  }
})
