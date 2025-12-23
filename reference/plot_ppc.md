# Plot posterior predictive trajectory

Plot as a probability mass function (suffix `_pmf`) or as a fanchart
(stacked confidence interval, suffix `_fanchart`).

- `plot_post_traj_*` plots the posterior predictive trajectory only

- `plot_ppc_traj_*` overlays the observed trajectory to the posterior
  predictive trajectory

## Usage

``` r
plot_post_traj_pmf(
  obj,
  id,
  patient_id,
  max_score = NA,
  support = NULL,
  max_scale = NA
)

plot_post_traj_fanchart(
  obj,
  id,
  patient_id,
  max_score = NA,
  interval = c("eti", "hdi"),
  CI_level = seq(0.1, 0.9, 0.1),
  ...
)

plot_ppc_traj_pmf(obj, train, test, patient_id, max_score = NA, ...)

plot_ppc_traj_fanchart(obj, train, test, patient_id, max_score = NA, ...)
```

## Arguments

- obj:

  Stanfit object or matrix of replications, with rows corresponding to
  samples and columns corresponding to variables (there should be
  `nrow(id)` columns).

- id:

  Dataframe linking index in `obj` to (`Patient`, `Time`) pairs, cf.
  output from
  [`get_index()`](https://ghurault.github.io/EczemaPred/reference/get_index.md).

- patient_id:

  Patient ID.

- max_score:

  (Optional) Maximum value that the score can take. In `plot_*_traj_pmf`
  this will set `support` if it is not supplied. In
  `plot_*_traj_fanchart` this will set the y axis range. In
  `plot_ppc_traj_*` this will check the content of `train` and `test`.

- support:

  Values that the discrete distribution can take. Can be NULL, in that
  case the support of the pmf is estimated from the data (cf.
  [`HuraultMisc::extract_distribution()`](https://ghurault.github.io/HuraultMisc/reference/extract_distribution.html)).

- max_scale:

  Maximum value that the legend display. If NA, this chosen
  automatically.

- interval:

  Type of confidence of interval to display, one of "eti" for
  equal-tailed intervals and "hdi" for highest density interval.

- CI_level:

  Vector of confidence level to plot for the fanchart.

- ...:

  arguments to pass to `plot_post_traj_*`. For
  `plot_post_traj_fanchart()`, arguments to pass to
  [`add_fanchart()`](https://ghurault.github.io/EczemaPred/reference/add_fanchart.md).

- train:

  Training dataset used to obtain the fit.

- test:

  Testing dataset used to obtain the fit (can be `NULL`).

## Value

Ggplot
