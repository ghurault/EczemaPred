// Binomal Markov Chain model
// - Measurement error by binomial
// - Latent dynamic by Markov Chain, parametrised by steady state ss1 (random walk) and p10 (patient-dependent)

functions {
#include /include/get_ts_length.stan
#include /include/get_ragged_bounds.stan
}

data {
#include /include/data_lgtd_discrete.stan

  real prior_sigma[2];
  real prior_mu_logit_p10[2];
  real prior_sigma_logit_p10[2];
  real prior_logit_tss1_0[2];

  int<lower = 0, upper = 1> run; // Switch to evaluate the likelihood

}

transformed data {
#include /include/tdata_lgtd.stan // Compute id of start/end/observations of time-series
}

parameters {
  real eta[N]; // Error term for ss1, non-centered parametrisation
  real<lower = 0> sigma; // Standard deviation for evolution of ss1

  real eta_logit_p10[N_pt]; // Error term for p10, non-centered parametrisation
  real mu_logit_p10; // Mean of logit normal population prior for p10
  real<lower = 0> sigma_logit_p10; // Sigma of logit normal population prior for p10
}

transformed parameters {
  real logit_p10[N_pt];
  real p10[N_pt];
  real p11[N_pt];
  real logit_tss1_0[N_pt]; // Initial condition of logit_tss1
  vector[N] logit_tss1; // logit(ss1 * (1 + p10))
  vector[N] ss1; // Steady state probability of having a lesion
  vector[N] p01;
  vector[N] lambda; // Eigenvalue of the Markov Chain, measure the "mobility" of the chain (closer to 0 converges quicker to ss1)
  vector[N] y_lat; // Probability of a given area being affected

  for (k in 1:N_pt) {
    // p10 and p11
    logit_p10[k] = mu_logit_p10 + sigma_logit_p10 * eta_logit_p10[k];
    p10[k] = inv_logit(logit_p10[k]);
    p11[k] = 1 - p10[k];
    logit_tss1_0[k] = prior_logit_tss1_0[1] + prior_logit_tss1_0[2] * eta[id_ts[k, 1]]; // Logit normal prior for tss1
    for (t in id_ts[k, 1]:id_ts[k, 2]) {
      if (t == id_ts[k, 1]) {
        logit_tss1[id_ts[k, 1]] = logit_tss1_0[k];
      } else {
        logit_tss1[t] = logit_tss1[t - 1] + sigma * eta[t]; // Random walk for ss1
      }
      ss1[t] = inv_logit(logit_tss1[t]) / (1 + p10[k]); // Divide (1 + p10) to make sure p01 between 0 and 1 (NB: change interpretation of sigma)
      p01[t] = p10[k] * ss1[t] / (1 - ss1[t]);
      lambda[t] =  1 - p10[k] - p01[t];
      if (t == id_ts[k, 1]) {
        y_lat[id_ts[k, 1]] = ss1[id_ts[k, 1]]; // Start at steady state
      } else {
        y_lat[t] = p01[t] * (1 - y_lat[t -1]) + p11[k] * y_lat[t - 1]; // Markov Chain
      }
    }
  }
}

model {
  eta ~ std_normal();
  eta_logit_p10 ~ std_normal();
  // Priors
  sigma ~ normal(prior_sigma[1], prior_sigma[2]);
  mu_logit_p10 ~ normal(prior_mu_logit_p10[1], prior_mu_logit_p10[2]);
  sigma_logit_p10 ~ normal(prior_sigma_logit_p10[1], prior_sigma_logit_p10[2]);

  if (run == 1) {
    y_obs ~ binomial(M, y_lat[idx_obs]);
  }
}

generated quantities {
  real y_rep[N]; // Replications (of the entire time-series, not just observations)
  real lpd[N_test]; // Log predictive density of predictions
  real cum_err[N_test, M + 1]; // Cumulative error (useful to compute RPS)
  real y_pred[N_test]; // Predictive sample of y_test

  for (i in 1:N) {
    y_rep[i] = binomial_rng(M, y_lat[i]);
  }
  y_pred = y_rep[idx_test];

  for (i in 1:N_test) {
    lpd[i] = binomial_lpmf(y_test[i] | M, y_lat[idx_test[i]]);
    for (j in 0:M) {
      cum_err[i, j + 1] = binomial_cdf(j, M, y_lat[idx_test[i]]) - step(j - y_test[i]);
    }
  }

}
