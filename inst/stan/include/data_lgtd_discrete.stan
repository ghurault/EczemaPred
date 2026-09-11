// Common input in the data block of longitudinal discrete data models

#include /include/data_lgtd.stan

int<lower = 1> M; // Upper bound of observations
array[N_obs] int<lower = 0, upper = M> y_obs; // Observation
array[N_test] int<lower = 0, upper = M> y_test; // True test value
