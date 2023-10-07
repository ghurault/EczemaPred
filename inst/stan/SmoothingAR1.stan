// Model combining exponential smoothing and AR1 forecast, for values in [0, M].
// The time-series is first smoothed before applying AR1 model.
// The model is naive as it uses a non-truncated continuous distribution.
// Predictions can be continuous or discretised (rounded) if the outcome is continous or discrete, respectively, for proper evaluation.
// This inconsistency could cause error in prior predictive and faka data checks.
// The model allows optional parameters:
// - `alpha` (smoothing factor), but if not supplied as data, the priors is put on the time constant
// - `slope` (autocorrelation)
// - `intercept`, but if not supplied as data the prior is put on the autoregressive mean `y_inf`

functions {
#include /include/bin_search.stan
#include /include/truncated_normal.stan
#include /include/discrete_normal.stan
#include /include/get_ts_length.stan
#include /include/get_ragged_bounds.stan
}

data {
#include /include/data_lgtd.stan

  real<lower = 0> M; // Upper bound of observations

  real<lower = 0, upper = M> y_obs[N_obs]; // Observation (should be discrete when discrete = 1 but constraint not enforced)
  real<lower = 0, upper = M> y_test[N_test]; // True value (rounded for discrete=1)

  int<lower = 0, upper = 1> discrete; // Switch indicating whether the outcome is discrete or continuous (only relevant for predictions)

  // Optional parameters
  int<lower = 0, upper = 1> alpha_known; // Whether smoothing factor is know
  real<lower = 0, upper = 1> alpha_data[alpha_known];
  int<lower = 0, upper = 1> intercept_known; // Whether the intercept is known
  real intercept_data[intercept_known];
  int<lower = 0, upper = 1> slope_known; // Whether the slope is known
  real<lower = 0, upper = 1> slope_data[slope_known];

  // Priors
  real prior_sigma[2]; // Prior for sigma / M
  real prior_tau[alpha_known ? 0 : 2]; // Prior for smoothing time constant
  real prior_y_inf[intercept_known ? 0 : 2]; // Prior for AR1 intercept
  real<lower = 0> prior_slope[slope_known ? 0 : 2]; // Pror for AR1 slope

}

transformed data {
  int N_mis; // Number of missing observations
  int M_int = bin_search(M, 0, 1048576);
  int yi_test[N_test * discrete]; // y_test converted to int
#include /include/tdata_lgtd.stan // Compute id of start/end/observations of time-series

  N_mis = N - N_obs;

  if (discrete == 1) {
    if (M_int <= 0 || (M_int != M)) {
      reject("When discrete=TRUE, M should be a stricly positive integer.");
    }
    for (i in 1:N_test) {
      yi_test[i] = bin_search(y_test[i], 0, M_int);
    }
  }

}

parameters {
  real<lower = 0, upper = M> y_mis[N_mis]; // Missing values (including test)
  real<lower = 0> sigma; // Standard deviation
  real<lower = 0> tau_param[1 - alpha_known];
  real<lower = 0, upper = 1> slope_param[1 - slope_known];
  real y_inf_param[1 - intercept_known];
}

transformed parameters {
  real alpha; // Smoothing factor
  real tau; // Time constant of smoothing
  real slope; // Autocorrelation parameter
  real intercept; // Intercept / bias term
  real y_inf; // Autoregression mean
  real L[N]; // Level, smoothed y
  real linpred[N]; // Linear predictor
#include /include/tparameters_missing.stan // Concatenate missing and observed values in y

  if (alpha_known) {
    alpha = alpha_data[1];
    tau = -1 / log(1 - alpha);
  } else {
    tau = tau_param[1];
    alpha = 1 - exp(-1 / tau);
  }

  if (slope_known) {
    slope = slope_data[1];
  } else {
    slope = slope_param[1];
  }

  if (intercept_known) {
    intercept = intercept_data[1];
    if (intercept == 0 && slope == 1) {
      y_inf = 0.5 * M; // y_inf undefined, give arbitrary value
    } else {
      y_inf = intercept / (1 - slope);
    }
  } else {
    y_inf = y_inf_param[1];
    intercept = y_inf * (1 - slope);
  }

  for (k in 1:N_pt) {
    L[id_ts[k, 1]] = y[id_ts[k, 1]];
    linpred[id_ts[k, 1]] = y[id_ts[k, 1]]; // Not used
    for (t in (id_ts[k, 1] + 1):id_ts[k, 2]){
      L[t] = alpha * y[t] + (1 - alpha) * L[t - 1];
      linpred[t] = slope * L[t - 1] + intercept;
    }
  }

}

model {
  // Priors
  sigma / M ~ normal(prior_sigma[1], prior_sigma[2]);
  if (!alpha_known) {
    tau_param[1] ~ lognormal(prior_tau[1], prior_tau[2]);
  }
  if (!slope_known) {
    slope_param[1] ~ beta(prior_slope[1], prior_slope[2]);
  }
  if (!intercept_known) {
    y_inf_param[1] / M ~ normal(prior_y_inf[1], prior_y_inf[2]);
  }

  for (k in 1:N_pt) {
    // NB: Likelihood not truncated (or discretised); vectorised for efficiency
    to_vector(y[(id_ts[k, 1] + 1):id_ts[k, 2]]) ~ normal(to_vector(linpred[(id_ts[k, 1] + 1):id_ts[k, 2]]), sigma);
  }

}

generated quantities {
  real y_rep[N]; // Replications (of the entire time-series, not just observations)
  real lpd[N_test]; // Log predictive density of predictions
  real cum_err[N_test * discrete, M_int + 1]; // Cumulative error (useful to compute RPS)
  real y_pred[N_test]; // Predictive sample of y_test

  // Replications
  for (k in 1:N_pt) {
    y_rep[id_ts[k, 1]] = y[id_ts[k, 1]];
    if (discrete) {
      y_rep[id_ts[k, 1]] = round(y_rep[id_ts[k, 1]]);
    }
    for (t in id_ts[k, 1]:(id_ts[k, 2] - 1)) {
      if (discrete) {
        y_rep[t + 1] = discrete_normal_rng(M_int, linpred[t + 1], sigma);
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
        for (j in 0:M_int) {
          cum_err[i, j + 1] = (j + 1.0) / (M + 1.0) - step(j - yi_test[i]);
        }
      } else {
        lpd[i] = discrete_normal_lpmf(yi_test[i] | M_int, linpred[idx_test[i]], sigma);
        for (j in 0:M_int) {
          cum_err[i, j + 1] = discrete_normal_cdf(j , M_int, linpred[idx_test[i]], sigma) - step(j - yi_test[i]);
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
