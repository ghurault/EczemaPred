test_that("generate_MC2_sequence works", {
  N <- 1e3
  p01 <- 0.2
  p10 <- 0.4
  tol <- 0.1

  for (t0 in list(NULL, 0, 1)) {

    df <- data.frame(x = generate_MC2_sequence(N, p01 = p01, p10 = p10, t0 = t0)) %>%
      mutate(x_next = lead(x))
    x0_next <- df %>% filter(x == 0) %>% drop_na() %>% pull(x_next)
    x1_next <- df %>% filter(x == 1) %>% drop_na() %>% pull(x_next)

    expect_equal(nrow(df), N)
    expect_lt(abs(p01 - mean(x0_next)), tol)
    expect_lt(abs(1 - p10 - mean(x1_next)), tol)
    if(!is.null(t0)) {
      expect_equal(t0, df$x[1])
    }
  }

})

test_that("generate_missing works", {
  N <- 1e3
  p_mis <- 0.25
  p_obs_obs <- 0.75
  tol <- 0.1

  for (tp in c("random", "markovchain")) {
    x <- generate_missing(N, type = tp, p_mis = p_mis, p_obs_obs = p_obs_obs)
    expect_true(is.logical(x))
    expect_equal(N, length(x))
    expect_lt(abs(mean(x) - p_mis), tol)
    if (tp == "markovchain") {
      freq_obs_obs <- data.frame(x) %>%
        mutate(x_next = lead(x)) %>%
        drop_na() %>%
        summarise(sum(!x & !x_next) / sum(!x)) %>%
        pull()
      expect_lt(abs(freq_obs_obs - p_obs_obs), tol)
    }
  }

})
