// Common input in the data block of longitudinal continuous data models

#include /include/data_lgtd.stan

real<lower = 0> M; // Upper bound of observations
real<lower = 0, upper = M> y_obs[N_obs]; // Observation
real<lower = 0, upper = M> y_test[N_test]; // True value
