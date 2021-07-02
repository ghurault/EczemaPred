// "Naive" AR1 model for continuous values in [0, M]
// The model is naive as it uses a non-truncated distribution
// Patient-dependent autocorrelation and intercept
// However, predictions are truncated for proper evaluation
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

  real<lower = 0, upper = M> y_obs[N_obs]; // Observation (should be discrete when discrete = 1 but constraint not enforced)
  real<lower = 0, upper = M> y_test[N_test]; // True value (rounded for discrete=1)

  real prior_sigma[2];
  real prior_mu_logit_slope[2];
  real prior_sigma_logit_slope[2];
  real prior_mu_inf[2];
  real prior_sigma_inf[2];

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
  real linpred[N]; // Linear predictor
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
    // Naive random walk (not truncated or discretised); vectorised for efficiency
    to_vector(y[(id_ts[k, 1] + 1):id_ts[k, 2]]) ~ normal(to_vector(linpred[(id_ts[k, 1] + 1):id_ts[k, 2]]), sigma);
  }

}

generated quantities {
  real y_rep[N]; // Replications (of the entire time-series, not just observations)
  real lpd[N_test]; // Log predictive density of predictions
  real cum_err[N_test * discrete, M + 1]; // Cumulative error (useful to compute RPS)
  real y_pred[N_test]; // Predictive sample of y_test

  for (k in 1:N_pt) {
    y_rep[id_ts[k, 1]] = y[id_ts[k, 1]];
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
