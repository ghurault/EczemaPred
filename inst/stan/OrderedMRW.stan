// Ordered Logistic Multivariate Random Walk model
// - Measurement error by ordered logistic distribution
// - Latent dynamic by multivariate normal random walk

// The observations are assumed to take discrete values in [0, M] (M + 1 categories)

functions {
#include /include/functions_OrderedRW.stan
#include /include/get_ts_length.stan
#include /include/get_ragged_bounds.stan
}

data {
#include /include/data_lgtd.stan

  int<lower = 2> M; // Upper bound of observations
  int<lower = 1> D; // Number of signs
  
  int<lower = 1, upper = D> d_obs[N_obs]; // Sign index
  int<lower = 0, upper = M> y_obs[N_obs]; // Observations

  int<lower = 1, upper = D> d_test[N_test]; // Sign index
  int<lower = 0, upper = M> y_test[N_test]; // True value
  
  int<lower = 0, upper = 1> run; // Switch to evaluate the likelihood
  int<lower = 0, upper = 1> independent_components; // Whether to have diagonal correlation matrices or not

  // Priors
  vector<lower = 0>[M - 1] prior_delta[D];
  vector[D] prior_sigma_meas[2];
  vector[D] prior_sigma_lat[2];
  real prior_Omega; 
  real prior_Omega0;
  vector[D] prior_mu_y0[2];
  vector<lower = 0>[D] prior_sigma_y0[2];

}

transformed data {
  int yc_obs[N_obs]; // Categorical y_obs
#include /include/tdata_lgtd.stan // Compute id of start/end/observations of time-series

  for (i in 1:N_obs) {
    yc_obs[i] = y_obs[i] + 1;
  }

}

parameters {
  vector[D] eta[N]; // Error term, non-centered parametrisation
  cholesky_factor_corr[D] L_param; // Cholesky decomposition of correlation matrix
  vector<lower = 0>[D] sigma_lat; // Vector of standard deviation
  vector<lower = 0>[D] sigma_meas; // Equivalent standard deviation (not scale) of logistic distribution
  simplex[M - 1] delta[D]; // Difference between relative cutpoints
  cholesky_factor_corr[D] L0_param; // Cholesky decomposition of initial condition correlation matrix
  vector[D] mu_y0; // Population mean y_lat at t0
  vector<lower = 0>[D] sigma_y0; // Population standard deviation of y_lat at t0
}

transformed parameters {
  matrix[D, D] L;
  matrix[D, D] L0;
  vector[D] s = sigma_meas * sqrt(3) / pi(); // scale of measurement distribution
  vector[M] ct[D]; // Cutpoints in [0, M] space
  vector[M] z_ct[D]; // Cutpoints in affinity space
  vector[D] y_lat[N]; // Latent score
  vector[D] z_lat[N]; // Latent score in affinity space
  vector[D] y0[N_pt]; // Initial latent score
  
  if (independent_components == 0) {
    L = L_param;
    L0 = L0_param;
  } else {
    L = diag_matrix(rep_vector(1, D));
    L0 = diag_matrix(rep_vector(1, D));
  }
  
  // Cutpoints
  for (d in 1:D) {
    ct[d] = make_ct(delta[d]);
    z_ct[d] = ct[d] / s[d];
  }

  for (k in 1:N_pt) {
    y0[k] = mu_y0 + sigma_y0 .* (L0 * eta[id_ts[k, 1]]);
    y_lat[id_ts[k, 1]] = y0[k];
    for (t in (id_ts[k, 1] + 1):id_ts[k, 2]) {
      y_lat[t] = y_lat[t - 1] + sigma_lat .* (L * eta[t]); // Multivariate Random walk
    }
  }
  for (i in 1:N) {
    z_lat[i] = y_lat[i] ./ s;
  }

}

model {
  for (i in 1:N) {
    eta[i] ~ std_normal();
  }
  // Priors
  L ~ lkj_corr_cholesky(prior_Omega); // LKJ prior for correlation matrix
  L0 ~ lkj_corr_cholesky(prior_Omega0); // LKJ prior for correlation matrix
  for (d in 1:D) {
    delta[d] ~ dirichlet(prior_delta[d]);
  }
  sigma_meas / M ~ lognormal(prior_sigma_meas[1], prior_sigma_meas[2]);
  sigma_lat / M ~ lognormal(prior_sigma_lat[1], prior_sigma_lat[2]);
  mu_y0 / M ~ normal(prior_mu_y0[1], prior_mu_y0[2]);
  sigma_y0 / M ~ normal(prior_sigma_y0[1], prior_sigma_y0[2]);

  if (run == 1) {
    for (i in 1:N_obs) {
      yc_obs[i] ~ ordered_logistic(z_lat[idx_obs[i]][d_obs[i]], z_ct[d_obs[i]]);
    }
  }
}

generated quantities {
  matrix[D, D] Omega = multiply_lower_tri_self_transpose(L); // Correlation matrix
  matrix[D, D] Sigma_lat = quad_form_diag(Omega, sigma_lat); // Covariance matrix
  matrix[D, D] Omega0 = multiply_lower_tri_self_transpose(L0); // Correlation matrix of initial condition
  vector[D] sigma_tot = sqrt(square(sigma_meas) + square(sigma_lat));
  vector[D] rho2 = square(sigma_meas ./ sigma_tot);
  real y_rep[N, D]; // Replications (of the entire time-series, not just observations)
  real ytot_rep[N]; // Replications of sum of y
  real lpd[N_test]; // Log predictive density of predictions
  real cum_err[N_test, M + 1]; // Cumulative error (useful to compute RPS)
  real y_pred[N_test]; // Predictive sample of y_test

  for (i in 1:N) {
    for (d in 1:D) {
      y_rep[i, d] = ordered_logistic_rng(z_lat[i][d], z_ct[d]) - 1;
    }
    ytot_rep[i] = sum(y_rep[i]);
  }
  
  for (i in 1:N_test) {
    y_pred[i] = y_rep[idx_test[i], d_test[i]];
    // Store log pmf in cum_err[i]
    for (m in 0:M) {
      cum_err[i, m + 1] = ordered_logistic_lpmf(m + 1 | z_lat[idx_test[i]][d_test[i]], z_ct[d_test[i]]);
    }
    // Compute metrics
    lpd[i] = cum_err[i, y_test[i] + 1];
    cum_err[i] = compute_cumulative_error(y_test[i] + 1, exp(cum_err[i]));
    
  }

}
