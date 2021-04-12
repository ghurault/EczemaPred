real truncated_normal_lpdf(real y, real M, real mu, real sigma) {
  // (Log) probability density function of a truncated normal distribution
  //
  // Args:
  // y: observation
  // M: upper bound of distribution
  // mu: mean of the normal distribution
  // sigma: standard deviation of the normal

  real Z = normal_cdf(M, mu, sigma) - normal_cdf(0, mu, sigma); // Normalisation constant
  real f = normal_lpdf(y | mu, sigma); // Unnormalised pdf
  return(f - log(Z));
}

real truncated_normal_rng(real M, real mu, real sigma) {
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
  return(x);
}
