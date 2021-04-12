int bin_search(real x, int min_val, int max_val) {
  // Find the integer equivalent to x between min_val and max_val

  int range = (max_val - min_val + 1) / 2;
  int mid_pt = min_val + range;
  real y = round(x);

  while(range > 0) {
    if (y == mid_pt) {
      range = 0;
    } else {
      range =  (range + 1) / 2;
      mid_pt += y > mid_pt ? range: -range;
    }
  }
  return(mid_pt);
}
