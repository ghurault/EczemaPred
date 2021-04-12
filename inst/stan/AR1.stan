// "Naive" AR1 model for continuous values in [0, M]
// The model is naive as it uses a non-truncated distribution
// No mixed effect
// However, predictions are truncated for proper evaluation
// This inconsistency could cause error in prior predictive and faka data checks

functions {
#include /include/truncated_normal.stan
}

data {
#include /include/data_lgtd_continuous.stan

  real prior_sigma[2];
  real<lower = 0> prior_alpha[2];
  real prior_y_inf[2];
}

transformed data {
  int N_mis; // Number of missing observations
#include /include/tdata_lgtd.stan // Compute id of start/end/observations of time-series

  N_mis = N - N_obs;
}

parameters {
  real<lower = 0, upper = M> y_mis[N_mis]; // Missing values (including test)
  real<lower = 0> sigma; // Standard deviation
  real<lower = 0, upper = 1> alpha; // Autocorrelation parameter
  real y_inf; // Autoregression mean
}

transformed parameters {
  real b = y_inf * (1 - alpha);
#include /include/tparameters_missing.stan // Concatenate missing and observed values in y
}

model {
  sigma / M ~ normal(prior_sigma[1], prior_sigma[2]);
  alpha ~ beta(prior_alpha[1], prior_alpha[2]);
  y_inf / M ~ normal(prior_y_inf[1], prior_y_inf[2]);

  for (k in 1:N_pt) {
    // Naive random walk (not truncated); vectorised for efficiency
    to_vector(y[(id_start[k] + 1):id_end[k]]) ~ normal(alpha * to_vector(y[id_start[k]:(id_end[k] - 1)]) + b, sigma);
  }

}

generated quantities {
  real y_rep[N]; // Replications (of the entire time-series, not just observations)
  real lpd[N_test]; // Log predictive density of predictions
  real y_pred[N_test]; // Predictive sample of y_test

  for (k in 1:N_pt) {
    y_rep[id_start[k]] = y[id_start[k]];
    for (t in id_start[k]:(id_end[k] - 1)) {
      y_rep[t + 1] = truncated_normal_rng(M, alpha * y[t] + b, sigma);
    }
  }
  y_pred = y_rep[idx_test];

  for (i in 1:N_test) {
    if (id_start[k_test[i]] == idx_test[i]) {
      // cf. autoregressive model doesn't work for t=1, assume uniform distribution
      lpd[i] = -log(M);
    } else {
      lpd[i] = truncated_normal_lpdf(y_test[i] | M, alpha * y[idx_test[i] - 1] + b, sigma);
    }
  }

}
