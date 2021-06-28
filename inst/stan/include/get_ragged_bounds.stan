int[, ] get_ragged_bounds(int[] group_size) {
  // Get indices corresponding to the first and last observation of groups
  // Args: - group_size: array of length N_group indicating the size of each group
  // Return array of size N_group * 2 : first column correspond to first index, second column to last index

  int N_group = size(group_size);
  int id[N_group, 2];

  if (N_group != size(group_size)) {
    reject("group_size should be an array of length N_group");
  }

  for (i in 1:N_group) {
    if (i == 1) {
      id[i, 1] = 1;
    } else {
      id[i, 1] = id[i - 1, 2] + 1;
    }
    id[i, 2] = id[i, 1] - 1 + group_size[i];
  }

  return(id);

}
