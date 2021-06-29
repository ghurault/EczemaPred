// "Naive" AR1 model for continuous values in [0, M]
// The model is naive as it uses a non-truncated distribution
// Patient-dependent autocorrelation and intercept
// However, predictions are truncated for proper evaluation
// This inconsistency could cause error in prior predictive and faka data checks

functions {
#include /include/truncated_normal.stan
#include /include/get_ts_length.stan
#include /include/get_ragged_bounds.stan
}

data {
#include /include/data_lgtd_continuous.stan

  real prior_sigma[2];
  real prior_mu_logit_slope[2];
  real prior_sigma_logit_slope[2];
  real prior_mu_inf[2];
  real prior_sigma_inf[2];

}

transformed data {
  int N_mis; // Number of missing observations
#include /include/tdata_lgtd.stan // Compute id of start/end/observations of time-series

  N_mis = N - N_obs;
}

parameters {
  real<lower = 0, upper = M> y_mis[N_mis]; // Missing values (including test)
  real<lower = 0> sigma; // Standard deviation

  // Population autocorrelation parameters
  real mu_logit_slope; // Logit mean
  real<lower = 0> sigma_logit_slope; // Logit std
  real eta_slope[N_pt]; // Error term

  // Population autoregression mean
  real mu_inf; // Population mean
  real<lower = 0> sigma_inf; // Population std
  real eta_inf[N_pt]; // Error term
}

transformed parameters {
  real slope[N_pt];
  real y_inf[N_pt];
  real intercept[N_pt];
#include /include/tparameters_missing.stan // Concatenate missing and observed values in y

  for (k in 1:N_pt) {
    slope[k] = inv_logit(mu_logit_slope + sigma_logit_slope * eta_slope[k]);
    y_inf[k] = mu_inf + sigma_inf * eta_inf[k];
    intercept[k] = y_inf[k] * (1 - slope[k]);
  }

}

model {
  eta_slope ~ std_normal();
  eta_inf ~ std_normal();

  sigma / M ~ normal(prior_sigma[1], prior_sigma[2]);
  mu_logit_slope ~ normal(prior_mu_logit_slope[1], prior_mu_logit_slope[2]);
  sigma_logit_slope ~ normal(prior_sigma_logit_slope[1], prior_sigma_logit_slope[2]);
  mu_inf / M ~ normal(prior_mu_inf[1], prior_mu_inf[2]);
  sigma_inf / M ~ normal(prior_sigma_inf[1], prior_sigma_inf[2]);

  for (k in 1:N_pt) {
    // Naive random walk (not truncated); vectorised for efficiency
    to_vector(y[(id_ts[k, 1] + 1):id_ts[k, 2]]) ~ normal(slope[k] * to_vector(y[id_ts[k, 1]:(id_ts[k, 2] - 1)]) + intercept[k], sigma);
  }

}

generated quantities {
  real y_rep[N]; // Replications (of the entire time-series, not just observations)
  real lpd[N_test]; // Log predictive density of predictions
  real y_pred[N_test]; // Predictive sample of y_test

  for (k in 1:N_pt) {
    y_rep[id_ts[k, 1]] = y[id_ts[k, 1]];
    for (t in id_ts[k, 1]:(id_ts[k, 2] - 1)) {
      y_rep[t + 1] = truncated_normal_rng(M, slope[k] * y[t] + intercept[k], sigma);
    }
  }
  y_pred = y_rep[idx_test];

  for (i in 1:N_test) {
    if (id_ts[k_test[i], 1] == idx_test[i]) {
      // cf. autoregressive model doesn't work for t=1, assume uniform distribution
      lpd[i] = -log(M);
    } else {
      lpd[i] = truncated_normal_lpdf(y_test[i] | M, slope[k_test[i]] * y[idx_test[i] - 1] + intercept[k_test[i]], sigma);
    }
  }

}
