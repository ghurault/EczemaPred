// Common input in the data block of longitudinal data models

int<lower = 0> N_obs; // Number of non-missing observations
int<lower = 0> N_pt; // Number of patients

array[N_obs] int<lower = 1, upper = N_pt> k_obs; // Patient index
array[N_obs] int<lower = 1> t_obs; // Time of observation (from 1 to t_max)

int<lower = 0> N_test; // Number of predictions to evaluate
array[N_test] int<lower = 1, upper = N_pt> k_test; // Patient index
array[N_test] int<lower = 1> t_test; // Time of prediction
