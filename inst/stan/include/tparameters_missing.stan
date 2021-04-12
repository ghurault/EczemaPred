// Missing values processing in the transformed parameters (for models without measurement error)

real y[N] = rep_array(-1.0, N);

y[idx_obs] = y_obs;
{
  int id = 1;
  for (i in 1:N) {
    if (y[i] == -1) {
      y[i] = y_mis[id];
      id += 1;
    }
  }
}
