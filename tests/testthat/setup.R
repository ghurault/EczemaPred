
set.seed(2021)
options(warn = -1)

# RW ----------------------------------------------------------------------

RW_setup <- list(
  N_patient = 10,
  max_score = 100
)
RW_setup$t_max <- rpois(RW_setup$N_patient, 50)

RW_params <- list(
  alpha = 1,
  intercept = rep(0, RW_setup$N_patient),
  slope = rep(1, RW_setup$N_patient),
  y0 = round(rbeta(RW_setup$N_patient, 5, 5) * RW_setup$max_score),
  sigma = 2
)

RW_model <- EczemaModel("RW", max_score = RW_setup$max_score, discrete = FALSE)

RW_df <- generate_fakedata(N_pt = RW_setup$N_patient,
                           t_max = RW_setup$t_max,
                           max_score = RW_setup$max_score,
                           params = RW_params)

if (FALSE) {
  # Add missing values
  RW_df <- lapply(1:RW_setup$N_patient,
               function(i) {
                 RW_df %>%
                   filter(Patient == i) %>%
                   mutate(Missing = generate_missing(n())) %>%
                   filter(!Missing)
               }) %>%
    bind_rows()
}

# Split so that test parameters are like patient parameters
tmp <- RW_df %>%
  group_by(Patient) %>%
  mutate(Label = case_when(Time == max(Time) ~ "Testing",
                           TRUE ~ "Training")) %>%
  ungroup()
RW_split <- list(Training = tmp %>% filter(Label == "Training") %>% select(-Label),
                 Testing = tmp %>% filter(Label == "Testing") %>% select(-Label))
rm(tmp)

RW_fit <- EczemaFit(RW_model, train = RW_split$Training, test = RW_split$Testing, chains = 1, iter = 1000, refresh = 0)

# MC ----------------------------------------------------------------------

MC_setup <- list(
  N = 400,
  K = 2,
  prob_mis = 0.2
)

MC_params <- list(
  p01 = 0.2,
  p10 = 0.4
)

MC_df <- data.frame(t0 = 1:MC_setup$N,
                    y0 = generate_MC2_sequence(N = MC_setup$N, p01 = MC_params$p01, p10 = MC_params$p10) + 1) %>%
  filter(!generate_missing(MC_setup$N, type = "random", p_mis = MC_setup$prob_mis)) %>%
  mutate(y1 = lead(y0),
         dt = lead(t0) - t0)

tmp <- MC_df %>%
  mutate(Label = case_when(t0 <= 0.9 * MC_setup$N ~ "Training",
                           TRUE ~ "Testing")) %>%
  drop_na()
MC_split <- list(Training = tmp %>% filter(Label == "Training") %>% select(-Label),
                 Testing = tmp %>% filter(Label == "Testing") %>% select(-Label))
rm(tmp)

MC_model <- EczemaModel("MC", K = MC_setup$K)
MC_fit <- EczemaFit(MC_model, train = MC_split$Training, test = MC_split$Testing, chains = 1, refresh = 0)
