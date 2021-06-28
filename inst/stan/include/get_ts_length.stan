int[] get_ts_length(int[] k, int[] t) {
  // From vector of patient ID (k) and timepoints (t)
  // ... get the length of the time-series for each patient

  int N = size(k);
  int N_pt = max(k);
  int t_max[N_pt];
  if (size(t) != N) {
    reject("k and t should have the same length");
  }

  for (i in 1:N) {
    t_max[k[i]] = max(t[i], t_max[k[i]]);
  }

  return(t_max);
}
