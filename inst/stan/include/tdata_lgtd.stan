// Transformed data block for longitudinal data models
// NB: for t_max[k] = 0, id_ts[k, 1] = id_ts[k, 2] + 1 and the for loop t in id_ts[k, 1]:id_ts[k, 2] will not execute

array[N_pt] int t_max = get_ts_length(append_array(k_obs, k_test), append_array(t_obs, t_test)); // Length of each time series
int N = sum(t_max); // Total number of observations
array[N_pt, 2] int id_ts = get_ragged_bounds(t_max); // Index of first and last observation of each patient time-series
array[N_obs] int idx_obs; // index of non-missing observations
array[N_test] int idx_test; // index of predictions

for (i in 1:N_obs) {
  idx_obs[i] = id_ts[k_obs[i], 1] - 1 + t_obs[i];
}
for (i in 1:N_test) {
  idx_test[i] = id_ts[k_test[i], 1] - 1 + t_test[i];
}
