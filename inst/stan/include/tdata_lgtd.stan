// Transformed data block for longitudinal data models
// NB: for t_max[k] = 0, id_start[k] = id_end[k] + 1 and the for loop t in id_start[k]:id_end[k] will not execute
// and id_end[k - 1] = id_end[k], so no index is "wasted"

int t_max[N_pt] = rep_array(0, N_pt); // Length of each time series
int N; // Total number of observations
int id_start[N_pt]; // index of first observation for each patient
int id_end[N_pt]; // index of last observation for each patient
int idx_obs[N_obs]; // index of non-missing observations
int idx_test[N_test]; // index of predictions

for (i in 1:N_obs) {
  t_max[k_obs[i]] = max(t_obs[i], t_max[k_obs[i]]);
}
for (i in 1:N_test) {
  t_max[k_test[i]] = max(t_test[i], t_max[k_test[i]]);
}
N = sum(t_max);

for (k in 1:N_pt) {
  if (k == 1) {
    id_start[k] = 1;
  } else {
    id_start[k] = id_end[k - 1] + 1;
  }
  id_end[k] = id_start[k] - 1 + t_max[k];
}

for (i in 1:N_obs) {
  idx_obs[i] = id_start[k_obs[i]] - 1 + t_obs[i];
}
for (i in 1:N_test) {
  idx_test[i] = id_start[k_test[i]] - 1 + t_test[i];
}
