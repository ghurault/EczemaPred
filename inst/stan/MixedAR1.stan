// Mixed AR1 model for values in [0, M], with Patient-dependent autocorrelation and intercept.
// The model is naive as it uses a non-truncated distribution.
// Predictions can be continuous or discretised (rounded) if the outcome is continous or discrete, respectively, for proper evaluation.
// This inconsistency could cause error in prior predictive and faka data checks.

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

  array[N_obs] real<lower = 0, upper = M> y_obs; // Observation (should be discrete when discrete = 1 but constraint not enforced)
  array[N_test] real<lower = 0, upper = M> y_test; // True value (rounded for discrete=1)

  // Priors
  array[2] real prior_sigma;
  array[2] real prior_mu_logit_slope;
  array[2] real prior_sigma_logit_slope;
  array[2] real prior_mu_inf;
  array[2] real prior_sigma_inf;

}

transformed data {
  int N_mis; // Number of missing observations
  array[N_test * discrete] int yi_test; // y_test converted to int
#include /include/tdata_lgtd.stan // Compute id of start/end/observations of time-series

  N_mis = N - N_obs;

  if (discrete == 1) {
    for (i in 1:N_test) {
      yi_test[i] = bin_search(y_test[i], 0, M);
    }
  }

}

parameters {
  array[N_mis] real<lower = 0, upper = M> y_mis; // Missing values (including test)
  real<lower = 0> sigma; // Standard deviation

  // Population autocorrelation parameters
  real mu_logit_slope; // Logit mean
  real<lower = 0> sigma_logit_slope; // Logit std
  array[N_pt] real eta_slope; // Error term

  // Population autoregression mean
  real mu_inf; // Population mean
  real<lower = 0> sigma_inf; // Population std
  array[N_pt] real eta_inf; // Error term
}

transformed parameters {
  array[N_pt] real slope;
  array[N_pt] real y_inf;
  array[N_pt] real intercept;
  array[N] real linpred; // Linear predictor
#include /include/tparameters_missing.stan // Concatenate missing and observed values in y

  for (k in 1:N_pt) {
    slope[k] = inv_logit(mu_logit_slope + sigma_logit_slope * eta_slope[k]);
    y_inf[k] = mu_inf + sigma_inf * eta_inf[k];
    intercept[k] = y_inf[k] * (1 - slope[k]);
    linpred[id_ts[k, 1]] = y[id_ts[k, 1]]; // Not used
    for (t in (id_ts[k, 1] + 1):id_ts[k, 2]){
      linpred[t] = slope[k] * y[t - 1] + intercept[k];
    }

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
    // NB: Likelihood not truncated (or discretised); vectorised for efficiency
    to_vector(y[(id_ts[k, 1] + 1):id_ts[k, 2]]) ~ normal(to_vector(linpred[(id_ts[k, 1] + 1):id_ts[k, 2]]), sigma);
  }

}

generated quantities {
  array[N] real y_rep; // Replications (of the entire time-series, not just observations)
  array[N_test] real lpd; // Log predictive density of predictions
  array[N_test * discrete, M + 1] real cum_err; // Cumulative error (useful to compute RPS)
  array[N_test] real y_pred; // Predictive sample of y_test

  for (k in 1:N_pt) {
    y_rep[id_ts[k, 1]] = y[id_ts[k, 1]];
    if (discrete) {
      y_rep[id_ts[k, 1]] = round(y_rep[id_ts[k, 1]]);
    }
    for (t in id_ts[k, 1]:(id_ts[k, 2] - 1)) {
      if (discrete) {
        y_rep[t + 1] = discrete_normal_rng(M, linpred[t + 1], sigma);
      } else {
        y_rep[t + 1] = truncated_normal_rng(M, linpred[t + 1], sigma);
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
        lpd[i] = discrete_normal_lpmf(yi_test[i] | M, linpred[idx_test[i]], sigma);
        for (j in 0:M) {
          cum_err[i, j + 1] = discrete_normal_cdf(j , M, linpred[idx_test[i]], sigma) - step(j - yi_test[i]);
        }
      }
    }

  } else {

    for (i in 1:N_test) {
      if (id_ts[k_test[i], 1] == idx_test[i]) {
        // cf. autoregressive model doesn't work for t=1, assume uniform distribution
        lpd[i] = -log(M + 0.0);
      } else {
        lpd[i] = truncated_normal_lpdf(y_test[i] + 0.0 | M, linpred[idx_test[i]], sigma);
      }
    }

  }

}
