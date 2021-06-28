// Markov Chain model with K states

functions {
#include /include/get_ts_length.stan
}

data {
  int<lower = 1> K; // Number of categories

  int<lower = 0> N; // Number of observations
  int<lower = 1, upper = K> y0[N]; // Initial states
  int<lower = 1, upper = K>  y1[N]; // End states
  int<lower = 1> dt[N]; // Transition delay
  real<lower = 0> prior_p[K, K]; // Dirichlet prior (row i correspond to transition from state i)

  int<lower = 0, upper = 1> run; // Switch to evaluate the likelihood

  int<lower = 0> N_test; // Number of predictions to evaluate
  int<lower = 1, upper = K> y0_test[N_test]; // Initial states of predictions
  int<lower = 1, upper = K>  y1_test[N_test]; // End states of predictions
  int<lower = 1> dt_test[N_test]; // Transition delay of predictions
}

transformed data {
  int M = max(append_array(dt, append_array({1}, dt_test))); // Maximum exponent to compute
  int y[K, M, K] = rep_array(0, K, M, K); // Array (initial state * transition delay * Final state) of observation counts

  for (i in 1:N) {
    y[y0[i], dt[i], y1[i]] += 1;
  }

}

parameters {
  simplex[K] p[K]; // Array of K-simplex (each simplex give the transition probabilities from the initial states)
}

transformed parameters {
  matrix[K, K] P[M]; // Array (for each exponent) of transition matrices

  if (M > 0) {
    // Fill transition matrix
    for (k in 1:K) {
      P[1][k] = to_row_vector(p[k]); // Put the K-simplex p[k] in row k
    }
    // Pre-compute matrix exponent
    for (i in 2:M) {
      P[i] = P[1] * P[i - 1];
    }
  }

}

model {
  // Prior
  for (k in 1:K) {
    p[k] ~ dirichlet(to_vector(prior_p[k]));
  }

  // Markov chain model
  if (run == 1) {
    for (k in 1:K) {
      for (m in 1:M) {
        if (sum(y[k, m]) > 0) {
          y[k, m] ~ multinomial(to_vector(P[m][k]));
        }
      }
    }
  }

}

generated quantities {
  real y_rep[N]; // Replications of y1 (observations)
  real y_pred[N_test]; // Predictive sample of y_test
  real lpd[N_test]; // Log predictive density of predictions
  real cum_err[N_test, K]; // Cumulative error (useful to compute RPS)

  for (i in 1:N) {
    y_rep[i] = categorical_rng(to_vector(P[dt[i]][y0[i]]));
  }

  {
    vector[K] theta;
    vector[K] cdf;
    for (i in 1:N_test) {
      theta = to_vector(P[dt_test[i]][y0_test[i]]);
      y_pred[i] = categorical_rng(theta);
      lpd[i] = log(theta[y1_test[i]]);
      cdf = cumulative_sum(theta);
      for (k in 1:K) {
        cum_err[i, k] = cdf[k] - step(k - y1_test[i]);
      }
    }
  }

}
