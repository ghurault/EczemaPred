# Fit Markov Chain --------------------------------------------------------

K2 <- 2
N <- 300
p01 <- 0.2
p10 <- 0.4
df2 <- data.frame(t0 = 1:N,
                  y0 = generate_MC2_sequence(N = N, p01 = p01, p10 = p10) + 1) %>%
  mutate(y1 = lead(y0),
         dt = lead(t0) - t0,
         Label = case_when(t0 <= 0.9 * N ~ "Training",
                           TRUE ~ "Testing")) %>%
  drop_na()
train2 <- df2 %>% filter(Label == "Training")
test2 <- df2 %>% filter(Label == "Testing")

fit <- fit_MC(train = train2, test = test2, K = K2, chains = 1, refresh = 0)

# Test extract_lpd and extract_RPS ----------------------------------------

lpd <- extract_lpd(fit)
RPS <- extract_RPS(fit)

test_that("extract_lpd and extract_RPS works", {
  expect_equal(length(lpd), nrow(test2))
  expect_equal(length(RPS), nrow(test2))
  expect_true(is.numeric(lpd))
  expect_true(is.numeric(RPS))
  expect_true(all(RPS >= 0))
  expect_true(all(lpd <= 0))
})

perf <- test2 %>%
  mutate(lpd = lpd,
         RPS = RPS,
         y0 = y0 - 1) %>%
  select(y0, lpd, RPS) %>%
  pivot_longer(-y0, names_to = "Metric", values_to = "Value") %>%
  group_by(y0, Metric) %>%
  summarise(Mean = mean(Value),
            SD = sd(Value),
            SE = SD / sqrt(n()))

test_that("lpd estimates from fit_MC are accurate", {
  elpd <- function(p) {p * log(p) + (1 - p) * log(1 - p)}

  elpd0_est <- perf %>% filter(Metric == "lpd" & y0 == 0)
  expect_lte(abs(elpd(p01) - elpd0_est$Mean), 2.5 * elpd0_est$SE)
  elpd1_est <- perf %>% filter(Metric == "lpd" & y0 == 1)
  expect_lte(abs(elpd(p10) - elpd1_est$Mean), 2.5 * elpd1_est$SE)
})

test_that("RPS estimates from fit are accurate", {
  # For K2=2, RPS is twice the Brier Score, and is the expected BS is p(1 - p)
  eRPS <- function(p) {p * (1 - p)}

  eRPS0_est <- perf %>% filter(Metric == "RPS" & y0 == 0)
  expect_lte(abs(eRPS(p01) - eRPS0_est$Mean), 2.5 * eRPS0_est$SE)
  eRPS1_est <- perf %>% filter(Metric == "RPS" & y0 == 1)
  expect_lte(abs(eRPS(p10) - eRPS1_est$Mean), 2.5 * eRPS1_est$SE)
})

# Test add_predictions (discrete) -------------------------------------

test_that("add_predictions returns a correct dataframe (discrete)", {
  perf <- add_predictions(test = test2, fit = fit, discrete = TRUE, include_samples = FALSE)
  test_when_discrete <- function(perf, test) {
    expect_s3_class(perf, "data.frame")
    expect_true(all(c("lpd", "RPS") %in% colnames(perf)))
    expect_equal(perf %>% select(!all_of(c("lpd", "RPS"))), test)
    expect_gte(max(perf[["RPS"]]), 0)
  }
  test_when_discrete(perf, test2)
})
