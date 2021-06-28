// Ordered Logistic Random Walk model
// - Measurement error by ordered logistic distribution
// - Latent dynamic by normal random walk

// The observations are assumed to take discrete values in [0, M] (M + 1 categories)

functions {
#include /include/get_ts_length.stan
}

data {
#include /include/data_lgtd.stan

  int<lower = 2> M; // Upper bound of observations

  int<lower = 0, upper = M> y_obs[N_obs]; // Observations
  int<lower = 0, upper = M> y_test[N_test]; // True value

  // Priors
  real prior_delta[2, M - 1];
  real prior_sigma[2];
  real prior_mu_y0[2];
  real prior_sigma_y0[2];

  int<lower = 0, upper = 1> run; // Switch to evaluate the likelihood

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
  real<lower = 0> sigma; // Standard deviation of random walk
  real<lower = 0> delta[M - 1]; // Difference between cutpoints
  real mu_y0; // Intercept / Population mean of y_lat at t0 (offset)
  real<lower = 0> sigma_y0; // Population standard deviation of y_lat at t0
}

transformed parameters {
  vector[M] ct; // Cutpoints
  vector[N] y_lat; // Latent score
  real y0[N_pt]; // Initial latent score

  // Cutpoints
  ct[1] = 0;
  for (i in 2:M) {
    ct[i] = ct[i - 1] + delta[i -1];
  }

  for (k in 1:N_pt) {
    y0[k] = mu_y0 + sigma_y0 * eta[id_start[k]];
    y_lat[id_start[k]] = y0[k];
    for (t in (id_start[k] + 1):id_end[k]) {
      y_lat[t] = y_lat[t - 1] + sigma * eta[t]; // Random Walk
    }
  }
  y_lat = y_lat * ct[M]; // Scale latent score (and y0, sigma) by the range

}

model {
  eta ~ std_normal();
  delta ~ normal(prior_delta[1], prior_delta[2]);
  sigma ~ normal(prior_sigma[1], prior_sigma[2]);
  mu_y0 ~ normal(prior_mu_y0[1], prior_mu_y0[2]);
  sigma_y0 ~ normal(prior_sigma_y0[1], prior_sigma_y0[2]);

  if (run == 1) {
    yc_obs ~ ordered_logistic(y_lat[idx_obs], ct);
  }
}

generated quantities {
  real p0[M + 1]; // Probability distribution for average patient at t0
  real y_rep[N]; // Replications (of the entire time-series, not just observations)
  real lpd[N_test]; // Log predictive density of predictions
  real cum_err[N_test, M + 1]; // Cumulative error (useful to compute RPS)
  real y_pred[N_test]; // Predictive sample of y_test

  for (i in 1:(M + 1)) {
    p0[i] = exp(ordered_logistic_lpmf(i | mu_y0, ct));
  }

  for (i in 1:N) {
    y_rep[i] = ordered_logistic_rng(y_lat[i], ct) - 1;
  }
  y_pred = y_rep[idx_test];

  for (i in 1:N_test) {
    lpd[i] = ordered_logistic_lpmf(y_test[i] + 1 | y_lat[idx_test[i]], ct);
    // Compute cdf first
    cum_err[i, 1] = exp(ordered_logistic_lpmf(1 | y_lat[idx_test[i]], ct));
    for (j in 1:M) {
      cum_err[i, j + 1] = cum_err[i, j] + exp(ordered_logistic_lpmf(j + 1 | y_lat[idx_test[i]], ct));
    }
    // Substract step to get cumulative error
    for (j in 0:M) {
      cum_err[i, j + 1] = cum_err[i, j + 1] - step(j - y_test[i]);
    }

  }

}
