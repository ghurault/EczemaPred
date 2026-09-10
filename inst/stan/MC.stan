// Markov Chain model with K states

data {
  int<lower = 1> K; // Number of categories

  int<lower = 0> N; // Number of observations
  array[N] int<lower = 1, upper = K> y0; // Initial states
  array[N] int<lower = 1, upper = K>  y1; // End states
  array[N] int<lower = 1> dt; // Transition delay
  array[K, K] real<lower = 0> prior_p; // Dirichlet prior (row i correspond to transition from state i)

  int<lower = 0, upper = 1> run; // Switch to evaluate the likelihood

  int<lower = 0> N_test; // Number of predictions to evaluate
  array[N_test] int<lower = 1, upper = K> y0_test; // Initial states of predictions
  array[N_test] int<lower = 1, upper = K>  y1_test; // End states of predictions
  array[N_test] int<lower = 1> dt_test; // Transition delay of predictions
}

transformed data {
  int M = max(append_array(dt, append_array({1}, dt_test))); // Maximum exponent to compute
  array[K, M, K] int y = rep_array(0, K, M, K); // Array (initial state * transition delay * Final state) of observation counts

  for (i in 1:N) {
    y[y0[i], dt[i], y1[i]] += 1;
  }

}

parameters {
  array[K] simplex[K] p; // Array of K-simplex (each simplex give the transition probabilities from the initial states)
}

transformed parameters {
  array[M] matrix[K, K] P; // Array (for each exponent) of transition matrices

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
  array[N] real y_rep; // Replications of y1 (observations)
  array[N_test] real y_pred; // Predictive sample of y_test
  array[N_test] real lpd; // Log predictive density of predictions
  array[N_test, K] real cum_err; // Cumulative error (useful to compute RPS)

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
