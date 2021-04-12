// Common input in the data block of longitudinal discrete data models

#include /include/data_lgtd.stan

int<lower = 1> M; // Upper bound of observations
int<lower = 0, upper = M> y_obs[N_obs]; // Observation
int<lower = 0, upper = M> y_test[N_test]; // True test value
