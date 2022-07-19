// Ordered Logistic Random Walk model
// - Measurement error by ordered distribution
// - Latent dynamic by normal random walk
// The observations are assumed to take discrete values in [0, M] (M + 1 categories)
// The model allows to choose:
// - between ordered logistic or ordered probit as a measurement distribution
// - whether the distances between cutpoints are known
// NB: by choosing equally distance cutpoints, this is equivalent of using a discretised and censored logistic or normal distribution

functions {
#include /include/functions_OrderedRW.stan
#include /include/get_ts_length.stan
#include /include/get_ragged_bounds.stan
}

data {
#include /include/data_lgtd.stan

  int<lower = 2> M; // Upper bound of observations

  int<lower = 0, upper = M> y_obs[N_obs]; // Observations
  int<lower = 0, upper = M> y_test[N_test]; // True value

  int<lower = 0, upper = 1> run; // Switch to evaluate the likelihood
  int<lower = 0, upper = 1> measurement_distribution; // 0: ordered logistic; 1: ordered probit

  // Optional parameters
  int<lower = 0, upper = 1> delta_known; // Indicating whether delta is provided or a parameter
  simplex[M - 1] delta_data[delta_known ? 1 : 0]; // delta if delta_known=1

  // Priors
  vector<lower = 0>[delta_known ? 0 : (M - 1)] prior_delta;
  real prior_sigma_meas[2];
  real prior_sigma_lat[2];
  real prior_mu_y0[2];
  real<lower = 0> prior_sigma_y0[2];

}

transformed data {
  int yc_obs[N_obs]; // Categorical y_obs
#include /include/tdata_lgtd.stan // Compute id of start/end/observations of time-series

  for (i in 1:N_obs) {
    yc_obs[i] = y_obs[i] + 1;
  }

}

parameters {
  real eta[N]; // Error term, non-centered parametrisation for random walk
  real<lower = 0> sigma_lat; // Standard deviation of random walk
  real<lower = 0> sigma_meas; // Equivalent standard deviation (not scale) of logistic distribution
  simplex[M - 1] delta_param[delta_known ? 0 : 1]; // Relative difference between cutpoints
  real mu_y0; // Intercept / Population mean of y_lat at t0 (offset)
  real<lower = 0> sigma_y0; // Population standard deviation of y_lat at t0
}

transformed parameters {
  vector[M - 1] delta;
  real s; // scale of measurement distribution
  vector[M] ct; // Cutpoints in [0, M] space
  vector[M] z_ct; // Cutpoints in affinity space
  vector[N] y_lat; // Latent score in ~ [0, M] space
  vector[N] z_lat; // Latent score in affinity space
  real y0[N_pt]; // Initial latent score

  if (delta_known == 0) {
    delta = delta_param[1];
  } else {
    delta = delta_data[1];
  }

  if (measurement_distribution == 0) {
    s = sigma_meas * sqrt(3) / pi();
  } else {
    s = sigma_meas;
  }

  // Cutpoints
  ct = make_ct(delta);
  z_ct = ct / s;

  for (k in 1:N_pt) {
    y0[k] = mu_y0 + sigma_y0 * eta[id_ts[k, 1]];
    y_lat[id_ts[k, 1]] = y0[k];
    for (t in (id_ts[k, 1] + 1):id_ts[k, 2]) {
      y_lat[t] = y_lat[t - 1] + sigma_lat * eta[t]; // Random Walk
    }
  }
  z_lat = y_lat / s;
}

model {
  eta ~ std_normal();
  // Priors
  if (delta_known == 0) {
    delta_param[1] ~ dirichlet(prior_delta);
  }
  sigma_meas / M ~ lognormal(prior_sigma_meas[1], prior_sigma_meas[2]);
  sigma_lat / M ~ lognormal(prior_sigma_lat[1], prior_sigma_lat[2]);
  mu_y0 / M ~ normal(prior_mu_y0[1], prior_mu_y0[2]);
  sigma_y0 / M ~ normal(prior_sigma_y0[1], prior_sigma_y0[2]);

  if (run == 1) {
    if (measurement_distribution == 0) {
      yc_obs ~ ordered_logistic(z_lat[idx_obs], z_ct);
    } else {
      yc_obs ~ ordered_probit(z_lat[idx_obs], z_ct);
    }
  }
}

generated quantities {
  real sigma_tot = sqrt(sigma_meas^2 + sigma_lat^2);
  real rho2 = square(sigma_meas / sigma_tot);
  real y_rep[N]; // Replications (of the entire time-series, not just observations)
  real lpmf[N, M + 1]; // Log probability mass function
  real log_lik[N_obs]; // Log Likelihood
  real lpd[N_test]; // Log predictive density of predictions
  real cum_err[N_test, M + 1]; // Cumulative error (useful to compute RPS)
  real y_pred[N_test]; // Predictive sample of y_test

  for (i in 1:N) {
    if (measurement_distribution == 0) {
      y_rep[i] = ordered_logistic_rng(z_lat[i], z_ct) - 1;
      for (m in 0:M) {
        lpmf[i, m + 1] = ordered_logistic_lpmf(m + 1 | z_lat[i], z_ct);
      }
    } else {
      y_rep[i] = ordered_probit_rng(z_lat[i], z_ct) - 1;
      for (m in 0:M) {
        lpmf[i, m + 1] = ordered_probit_lpmf(m + 1 | z_lat[i], z_ct);
      }
    }
  }
  y_pred = y_rep[idx_test];

  for (i in 1:N_obs) {
    log_lik[i] = lpmf[idx_obs[i], yc_obs[i]];
  }

  for (i in 1:N_test) {
    lpd[i] = lpmf[idx_test[i], y_test[i] + 1];
    cum_err[i] = compute_cumulative_error(y_test[i] + 1, exp(lpmf[idx_test[i]]));
  }

}
