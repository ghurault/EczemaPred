// "Naive" AR1 model for continuous values in [0, M]
// The model is naive as it uses a non-truncated distribution
// Patient-dependent autocorrelation and intercept
// However, predictions are truncated for proper evaluation
// This inconsistency could cause error in prior predictive and faka data checks

functions {
#include /include/truncated_normal.stan
}

data {
#include /include/data_lgtd_continuous.stan

  real prior_sigma[2];
  real prior_mu_logit_alpha[2];
  real prior_sigma_logit_alpha[2];
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
  real mu_logit_alpha; // Logit mean
  real<lower = 0> sigma_logit_alpha; // Logit std
  real eta_alpha[N_pt]; // Error term

  // Population autoregression mean
  real mu_inf; // Population mean
  real<lower = 0> sigma_inf; // Population std
  real eta_inf[N_pt]; // Error term
}

transformed parameters {
  real alpha[N_pt];
  real y_inf[N_pt];
  real b[N_pt];
#include /include/tparameters_missing.stan // Concatenate missing and observed values in y

  for (k in 1:N_pt) {
    alpha[k] = inv_logit(mu_logit_alpha + sigma_logit_alpha * eta_alpha[k]);
    y_inf[k] = mu_inf + sigma_inf * eta_inf[k];
    b[k] = y_inf[k] * (1 - alpha[k]);
  }

}

model {
  eta_alpha ~ std_normal();
  eta_inf ~ std_normal();

  sigma / M ~ normal(prior_sigma[1], prior_sigma[2]);
  mu_logit_alpha ~ normal(prior_mu_logit_alpha[1], prior_mu_logit_alpha[2]);
  sigma_logit_alpha ~ normal(prior_sigma_logit_alpha[1], prior_sigma_logit_alpha[2]);
  mu_inf / M ~ normal(prior_mu_inf[1], prior_mu_inf[2]);
  sigma_inf / M ~ normal(prior_sigma_inf[1], prior_sigma_inf[2]);

  for (k in 1:N_pt) {
    // Naive random walk (not truncated); vectorised for efficiency
    to_vector(y[(id_start[k] + 1):id_end[k]]) ~ normal(alpha[k] * to_vector(y[id_start[k]:(id_end[k] - 1)]) + b[k], sigma);
  }

}

generated quantities {
  real y_rep[N]; // Replications (of the entire time-series, not just observations)
  real lpd[N_test]; // Log predictive density of predictions
  real y_pred[N_test]; // Predictive sample of y_test

  for (k in 1:N_pt) {
    y_rep[id_start[k]] = y[id_start[k]];
    for (t in id_start[k]:(id_end[k] - 1)) {
      y_rep[t + 1] = truncated_normal_rng(M, alpha[k] * y[t] + b[k], sigma);
    }
  }
  y_pred = y_rep[idx_test];

  for (i in 1:N_test) {
    if (id_start[k_test[i]] == idx_test[i]) {
      // cf. autoregressive model doesn't work for t=1, assume uniform distribution
      lpd[i] = -log(M);
    } else {
      lpd[i] = truncated_normal_lpdf(y_test[i] | M, alpha[k_test[i]] * y[idx_test[i] - 1] + b[k_test[i]], sigma);
    }
  }

}
