// "Naive" random walk model with optional exponential smoothing, for values in [0, M]
// The model is naive as it uses a non-truncated continuous distribution
// Predictions can be continuous or discretised (rounded) if the outcome is continous or discrete, respectively, for proper evaluation
// This inconsistency could cause error in prior predictive and faka data checks

functions {
#include /include/bin_search.stan
#include /include/truncated_normal.stan
#include /include/discrete_normal.stan
#include /include/get_ts_length.stan
#include /include/get_ragged_bounds.stan
}

data {
#include /include/data_lgtd.stan

  int<lower = 1> M; // Upper bound of observations

  int<lower = 0, upper = 1> discrete; // Switch indicating whether the outcome is discrete or continuous (only relevant for predictions)
  int<lower = 0, upper = 1> alpha_known; // Whether smoothing factor is know
  real<lower = 0, upper = 1> alpha_data[alpha_known];

  real prior_sigma[2]; // Prior for sigma / M
  real prior_tau[alpha_known ? 0 : 2]; // Prior for smoothing time constant

  real<lower = 0, upper = M> y_obs[N_obs]; // Observation (should be discrete when discrete = 1 but constraint not enforced)
  real<lower = 0, upper = M> y_test[N_test]; // True value (rounded for discrete=1)

}

transformed data {
  int N_mis; // Number of missing observations
  int yi_test[N_test * discrete]; // y_test converted to int
#include /include/tdata_lgtd.stan // Compute id of start/end/observations of time-series

  N_mis = N - N_obs;

  if (discrete == 1) {
    for (i in 1:N_test) {
      yi_test[i] = bin_search(y_test[i], 0, M);
    }
  }

}

parameters {
  real<lower = 0, upper = M> y_mis[N_mis]; // Missing values (including test)
  real<lower = 0> sigma;
  real<lower = 0> tau_param[1 - alpha_known];
}

transformed parameters {
  real alpha; // Smoothing factor
  real tau; // Time constant of smoothing
  real L[N]; // Level, smoothed y
#include /include/tparameters_missing.stan // Concatenate missing and observed values in y

  if (alpha_known) {
    alpha = alpha_data[1];
  } else {
    tau = tau_param[1];
    alpha = 1 - exp(-1 / tau);
  }

  for (k in 1:N_pt) {
    L[id_ts[k, 1]] = y[id_ts[k, 1]];
    for (t in (id_ts[k, 1] + 1):id_ts[k, 2]){
      L[t] = alpha * y[t] + (1 - alpha) * L[t - 1];
    }
  }

}

model {
  sigma / (M + 0.0) ~ normal(prior_sigma[1], prior_sigma[2]);
  if (!alpha_known) {
    tau_param[1] ~ lognormal(prior_tau[1], prior_tau[2]);
  }

  for (k in 1:N_pt) {
    // Naive random walk (not truncated, not discretised); vectorised for efficiency
    to_vector(y[(id_ts[k, 1] + 1):id_ts[k, 2]]) ~ normal(to_vector(L[id_ts[k, 1]:(id_ts[k, 2] - 1)]), sigma);
  }

}

generated quantities {
  real y_rep[N]; // Replications (of the entire time-series, not just observations)
  real lpd[N_test]; // Log predictive density of predictions
  real cum_err[N_test * discrete, M + 1]; // Cumulative error (useful to compute RPS)
  real y_pred[N_test]; // Predictive sample of y_test

  // Replications
  for (k in 1:N_pt) {
    y_rep[id_ts[k, 1]] = round(y[id_ts[k, 1]]);
    for (t in id_ts[k, 1]:(id_ts[k, 2] - 1)) {
      if (discrete) {
        y_rep[t + 1] = discrete_normal_rng(M, L[t], sigma);
      } else {
        y_rep[t + 1] = truncated_normal_rng(M, L[t], sigma);
      }
    }
  }
  y_pred = y_rep[idx_test];

  // Performance
  if (discrete == 1) {

    for (i in 1:N_test) {
      if (id_ts[k_test[i], 1] == idx_test[i]) {
        // cf. autoregressive model doesn't work for t=1, assume uniform distribution
        lpd[i] = -log(M + 1.0);
        for (j in 0:M) {
          cum_err[i, j + 1] = (j + 1.0) / (M + 1.0) - step(j - yi_test[i]);
        }
      } else {
        lpd[i] = discrete_normal_lpmf(yi_test[i] | M, L[idx_test[i] - 1], sigma);
        for (j in 0:M) {
          cum_err[i, j + 1] = discrete_normal_cdf(j , M, L[idx_test[i] - 1], sigma) - step(j - yi_test[i]);
        }
      }
    }

  } else {

    for (i in 1:N_test) {
      if (id_ts[k_test[i], 1] == idx_test[i]) {
        // cf. autoregressive model doesn't work for t=1, assume uniform distribution
        lpd[i] = -log(M + 0.0);
      } else {
        lpd[i] = truncated_normal_lpdf(y_test[i] + 0.0 | M, L[idx_test[i] - 1], sigma);
      }
    }

  }

}
