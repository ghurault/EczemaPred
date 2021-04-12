// Common input in the data block of longitudinal data models

int<lower = 0> N_obs; // Number of non-missing observations
int<lower = 0> N_pt; // Number of patients

int<lower = 1, upper = N_pt> k_obs[N_obs]; // Patient index
int<lower = 1> t_obs[N_obs]; // Time of observation (from 1 to t_max)

int<lower = 0> N_test; // Number of predictions to evaluate
int<lower = 1, upper = N_pt> k_test[N_test]; // Patient index
int<lower = 1> t_test[N_test]; // Time of prediction
