# Plot the evolution of the expected latent score of the OrderedRW model

Plot the evolution of the expected latent score of the OrderedRW model

## Usage

``` r
plot_latent_OrderedRW(fit, id, patient_id, ...)
```

## Arguments

- fit:

  Stanfit object

- id:

  Dataframe linking index in fit to (Patient, Time) pairs, cf. output
  from
  [`get_index()`](https://ghurault.github.io/EczemaPred/reference/get_index.md)

- patient_id:

  Patient ID

- ...:

  Arguments to pass to
  [`add_fanchart()`](https://ghurault.github.io/EczemaPred/reference/add_fanchart.md)

## Value

Ggplot

- Horizontal lines correspond to the expected cut-offs

- Ribbons correspond to the CI of a logistic distribution
