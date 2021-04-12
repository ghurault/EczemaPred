real discrete_normal_lpmf(int y, int M, real mu, real sigma) {
  // (Log) probability mass function of a truncated discrete (cf. rounding) normal distribution
  //
  // Args:
  // y: observation
  // M: upper bound of distribution
  // mu: mean of the normal distribution
  // sigma: standard deviation of the normal

  real Z = normal_cdf(M, mu, sigma) - normal_cdf(0, mu, sigma); // Normalisation constant
  real f = normal_cdf(fmin(y + 0.5, M), mu, sigma) - normal_cdf(fmax(y - 0.5, 0), mu, sigma); // Unnormalised pdf
  return(log(f) - log(Z));
}

real discrete_normal_cdf(int y, int M, real mu, real sigma) {
  // Cumulative distribution function of a truncated discrete (cf. rounding) normal distribution
  //
  // Args:
  // y: observation
  // M: upper bound of distribution
  // mu: mean of the normal distribution
  // sigma: standard deviation of the normal

  real Z = normal_cdf(M, mu, sigma) - normal_cdf(0, mu, sigma); // Normalisation constant
  real f = normal_cdf(fmin(y + 0.5, M), mu, sigma) - normal_cdf(0, mu, sigma); // Unnormalised cdf
  return(f / Z);
}

real discrete_normal_rng(int M, real mu, real sigma) {
  // Sample from truncated discrete (cf. rounding) normal distribution
  //
  // Args:
  // M: upper bound of distribution
  // mu: mean of the normal distribution
  // sigma: standard deviation of the normal

  real p1 = normal_cdf(0, mu, sigma);  // cdf with lower bound
  real p2 = normal_cdf(M, mu, sigma);  // cdf with upper bound
  real u = uniform_rng(p1, p2);
  real x = (sigma * inv_Phi(u)) + mu; // Sample between 0 and M
  return(round(x));
}
