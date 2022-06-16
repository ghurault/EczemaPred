// Binomial Multivariate Random Walk model
// - Measurement error by binomial
// - Latent dynamic by logit normal random walk

functions {
#include /include/get_ts_length.stan
#include /include/get_ragged_bounds.stan
}

data {
  int<lower = 0> N_obs; // Number of non-missing observations
  int<lower = 0> N_pt; // Number of patients
  int<lower = 0> M; // Upper bound of observations
  int<lower = 1> D; // Number of signs

  int<lower = 1, upper = D> d_obs[N_obs]; // Sign index
  int<lower = 1, upper = N_pt> k_obs[N_obs]; // Patient index
  int<lower = 1> t_obs[N_obs]; // Time of observation (from 1 to t_max)
  int<lower = 0, upper = M> y_obs[N_obs]; // Observations

  int<lower = 0, upper = 1> run; // Switch to evaluate the likelihood
  int<lower = 0, upper = 1> independent_components; // Whether to have diagonal correlation matrices or not

  int<lower = 0> N_test; // Number of predictions to evaluate
  int<lower = 1, upper = D> d_test[N_test]; // Sign index
  int<lower = 1, upper = N_pt> k_test[N_test]; // Patient index
  int<lower = 1> t_test[N_test]; // Time of prediction
  int<lower = 0, upper = M> y_test[N_test]; // True value

  // Priors
  real prior_Omega;
  real prior_Omega0;
  vector[D] prior_sigma[2];
  vector[D] prior_mu_logit_y0[2];
  vector[D] prior_sigma_logit_y0[2];
}

transformed data {
#include /include/tdata_lgtd.stan // Compute id of start/end/observations of time-series
}

parameters {
  cholesky_factor_corr[D] L_param; // Cholesky decomposition of correlation matrix
  vector<lower = 0>[D] sigma; // Vector of standard deviation
  vector[D] eta[N]; // Error term, non-centered parametrisation
  cholesky_factor_corr[D] L0_param; // Cholesky decomposition of initial condition correlation matrix
  vector[D] mu_logit_y0; // Population mean initial condition (logit scale)
  vector<lower = 0>[D] sigma_logit_y0; // Population std initial condition (logit scale)
}

transformed parameters {
  matrix[D, D] L;
  matrix[D, D] L0;
  vector[D] logit_y0[N_pt]; // Initial latent score
  vector[D] logit_lat[N]; // Latent score (logit scale)
  vector[D] y_lat[N]; // Latent score (0-1 scale)

  if (independent_components == 0) {
    L = L_param;
    L0 = L0_param;
  } else {
    L = diag_matrix(rep_vector(1, D));
    L0 = diag_matrix(rep_vector(1, D));
  }

  for (k in 1:N_pt) {
    logit_y0[k] = mu_logit_y0 + sigma_logit_y0 .* (L0 * eta[id_ts[k, 1]]);// Initial condition, prior is multivariate logit normal
    logit_lat[id_ts[k, 1]] = logit_y0[k];
    for (t in (id_ts[k, 1] + 1):id_ts[k, 2]) {
      logit_lat[t] = logit_lat[t - 1] + sigma .* (L * eta[t]); // Multivariate Random walk
    }
  }
  y_lat = inv_logit(logit_lat); // cf. y_lat follow a logit normal distribution
}

model {
  for (i in 1:N) {
    eta[i] ~ std_normal();
  }
  // Priors
  L ~ lkj_corr_cholesky(prior_Omega); // LKJ prior for correlation matrix
  L0 ~ lkj_corr_cholesky(prior_Omega0); // LKJ prior for correlation matrix
  sigma ~ normal(prior_sigma[1], prior_sigma[2]);
  mu_logit_y0 ~ normal(prior_mu_logit_y0[1], prior_mu_logit_y0[2]);
  sigma_logit_y0 ~ normal(prior_sigma_logit_y0[1], prior_sigma_logit_y0[2]);

  if (run == 1) {
    for (i in 1:N_obs) {
      y_obs[i] ~ binomial(M, y_lat[idx_obs[i]][d_obs[i]]);
    }
  }
}

generated quantities {
  matrix[D, D] Omega; // Correlation matrix
  matrix[D, D] Sigma; // Covariance matrix
  matrix[D, D] Omega0; // Correlation matrix of initial condition
  real y_rep[N, D]; // Replications (of the entire time-series, not just observations)
  real ytot_rep[N]; // Replications of sum of y
  real lpd[N_test]; // Log predictive density of predictions
  real cum_err[N_test, M + 1]; // Cumulative error distribution
  real y_pred[N_test]; // Predictive sample of y_test

  Omega = multiply_lower_tri_self_transpose(L);
  Omega0 = multiply_lower_tri_self_transpose(L0);
  Sigma = quad_form_diag(Omega, sigma);

  for (i in 1:N) {
    for (d in 1:D) {
      y_rep[i, d] = binomial_rng(M, y_lat[i][d]);
    }
    ytot_rep[i] = sum(y_rep[i]);
  }

  for (i in 1:N_test) {
    y_pred[i] = y_rep[idx_test[i], d_test[i]];
    lpd[i] = binomial_lpmf(y_test[i] | M, y_lat[idx_test[i]][d_test[i]]);
    for (j in 0:M) {
      cum_err[i, j + 1] = binomial_cdf(j, M, y_lat[idx_test[i]][d_test[i]]) - step(j - y_test[i]);
    }
  }

}
