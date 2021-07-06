# Test extract_lpd and extract_RPS ----------------------------------------

lpd <- extract_lpd(MC_fit)
RPS <- extract_RPS(MC_fit)

test_that("extract_lpd and extract_RPS works", {
  expect_equal(length(lpd), nrow(MC_split$Testing))
  expect_equal(length(RPS), nrow(MC_split$Testing))
  expect_true(is.numeric(lpd))
  expect_true(is.numeric(RPS))
  expect_true(all(RPS >= 0))
  expect_true(all(lpd <= 0))
})

perf <- MC_split$Testing %>%
  mutate(lpd = lpd,
         RPS = RPS,
         y0 = y0 - 1) %>%
  filter(dt == 1) %>%
  select(y0, lpd, RPS) %>%
  pivot_longer(-y0, names_to = "Metric", values_to = "Value") %>%
  group_by(y0, Metric) %>%
  summarise(Mean = mean(Value),
            SD = sd(Value),
            N = n(),
            SE = SD / sqrt(N))

test_that("lpd estimates from fit are accurate", {
  elpd <- function(p) {p * log(p) + (1 - p) * log(1 - p)}

  elpd0_est <- perf %>% filter(Metric == "lpd" & y0 == 0)
  expect_lte(abs(elpd(MC_params$p01) - elpd0_est$Mean), 2.5 * elpd0_est$SE)
  elpd1_est <- perf %>% filter(Metric == "lpd" & y0 == 1)
  expect_lte(abs(elpd(MC_params$p10) - elpd1_est$Mean), 2.5 * elpd1_est$SE)
})

test_that("RPS estimates from fit are accurate", {
  # For K=2, RPS is twice the Brier Score, and is the expected BS is p(1 - p)
  eRPS <- function(p) {p * (1 - p)}

  eRPS0_est <- perf %>% filter(Metric == "RPS" & y0 == 0)
  expect_lte(abs(eRPS(MC_params$p01) - eRPS0_est$Mean), 2.5 * eRPS0_est$SE)
  eRPS1_est <- perf %>% filter(Metric == "RPS" & y0 == 1)
  expect_lte(abs(eRPS(MC_params$p10) - eRPS1_est$Mean), 2.5 * eRPS1_est$SE)
})

# Test add_predictions and add_metrics (discrete) -------------------------------------

test_metrics_d <- function(perf, test) {
  expect_s3_class(perf, "data.frame")
  expect_true(all(c("lpd", "RPS") %in% colnames(perf)))
  expect_equal(perf %>% select(!all_of(c("lpd", "RPS"))), test)
  expect_gte(max(perf[["RPS"]]), 0)
}

test_that("add_predictions returns a correct dataframe (discrete)", {
  perf <- add_predictions(df = MC_split$Testing, fit = MC_fit, discrete = TRUE, include_samples = FALSE)
  test_metrics_d(perf, MC_split$Testing)
})

test_that("add_metrics1_d returns a correct dataframe", {
  perf <- add_metrics1_d(df = MC_split$Testing, fit = MC_fit)
  test_metrics_d(perf, MC_split$Testing)
})

test_that("add_metrics2_d returns a correct dataframe", {
  perf <- MC_split$Testing %>%
    rename(Score = y1) %>%
    mutate(Samples = samples_to_list(MC_fit, par_name = "y_pred")) %>%
    add_metrics2_d(support = 1:MC_setup$K) %>%
    rename(y1 = Score) %>%
    select(-Samples)
  test_metrics_d(perf, MC_split$Testing)
})
