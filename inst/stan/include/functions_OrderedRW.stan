vector make_ct(vector delta) {
  // Define cutpoints from simplex of difference between cutpoints (delta)
  int M = num_elements(delta) + 1;
  vector[M] ct;
  ct = append_row(0, delta);
  ct = cumulative_sum(ct);
  ct = ct * (M - 1) + 0.5; // If cutpoints are equally spaced expected value of k integer = k
  return(ct);
}

real[] compute_cumulative_error(int y, real[] pmf) {
  // Return cumulative error distribution for the distribution pmf and observation y
  // Observations are assumed to be between 1 and M and pmf is an array of length M such as pmf[i] = prob(y = i)

  int M = size(pmf);
  real cum_err[M];

  if (y < 1 && y > M) {
    reject("y is not in 1:M");
  }

  // Compute cdf first
  cum_err[1] = pmf[1];
  for (j in 2:M) {
    cum_err[j] = cum_err[j - 1] + pmf[j];
  }
  // Substract step to get cumulative error
  for (j in 1:M) {
    cum_err[j] = cum_err[j] - step(j - y);
  }

  return(cum_err);
}
