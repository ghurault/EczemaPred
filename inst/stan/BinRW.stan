// Binomial Random Walk model
// - Measurement error by binomial
// - Latent dynamic by logit normal random walk

functions {
#include /include/get_ts_length.stan
#include /include/get_ragged_bounds.stan
}

data {
#include /include/data_lgtd_discrete.stan

  real prior_sigma[2];
  real prior_mu_logit_y0[2];
  real prior_sigma_logit_y0[2];

  int<lower = 0, upper = 1> run; // Switch to evaluate the likelihood

}

transformed data {
#include /include/tdata_lgtd.stan // Compute id of start/end/observations of time-series
}

parameters {
  real eta[N]; // Error term, non-centered parametrisation
  real<lower = 0> sigma; // Standard deviation of random walk
  real mu_logit_y0; // Population mean initial condition (logit scale)
  real<lower = 0> sigma_logit_y0; // Population std initial condition (logit scale)
}

transformed parameters {
  vector[N] logit_lat; // Latent score (logit scale)
  real logit_y0[N_pt]; // Initial latent score

  for (k in 1:N_pt) {
    logit_y0[k] = mu_logit_y0 + sigma_logit_y0 * eta[id_ts[k, 1]]; // Initial condition, prior is logit normal
    logit_lat[id_ts[k, 1]] = logit_y0[k];
    for (t in (id_ts[k, 1] + 1):id_ts[k, 2]) {
      logit_lat[t] = logit_lat[t - 1] + sigma * eta[t]; // Random walk
    }
  }
}

model {
  eta ~ std_normal();
  // Priors
  sigma ~ normal(prior_sigma[1], prior_sigma[2]);
  mu_logit_y0 ~ normal(prior_mu_logit_y0[1], prior_mu_logit_y0[2]);
  sigma_logit_y0 ~ normal(prior_sigma_logit_y0[1], prior_sigma_logit_y0[2]);

  if (run == 1) {
    y_obs ~ binomial_logit(M, logit_lat[idx_obs]);
  }
}

generated quantities {
  vector[N] y_lat = inv_logit(logit_lat); // Latent score (0-1 scale; cf. logit-normal distribution)
  real y_rep[N]; // Replications (of the entire time-series, not just observations)
  real log_lik[N_obs]; // Log Likelihood
  real lpd[N_test]; // Log predictive density of predictions
  real cum_err[N_test, M + 1]; // Cumulative error (useful to compute RPS)
  real y_pred[N_test]; // Predictive sample of y_test

  for (i in 1:N) {
    y_rep[i] = binomial_rng(M, y_lat[i]);
  }
  y_pred = y_rep[idx_test];

  for (i in 1:N_obs) {
    log_lik[i] = binomial_lpmf(y_obs[i] | M, y_lat[idx_obs[i]]);
  }

  for (i in 1:N_test) {
    lpd[i] = binomial_lpmf(y_test[i] | M, y_lat[idx_test[i]]);
    for (j in 0:M) {
      cum_err[i, j + 1] = binomial_cdf(j, M, y_lat[idx_test[i]]) - step(j - y_test[i]);
    }
  }

}
