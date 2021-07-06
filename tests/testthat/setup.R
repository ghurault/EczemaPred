
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

RW_df <- RW_df %>% mutate(Iteration = get_fc_iteration(Time, 2))
RW_split <- split_fc_dataset(RW_df, 25)

RW_fit <- EczemaFit(RW_model, train = RW_split$Training, test = RW_split$Testing, chains = 1, iter = 1000, refresh = 0)
