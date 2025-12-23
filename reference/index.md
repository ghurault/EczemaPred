# Package index

## Available models

### Main models

- [`BinRW`](https://ghurault.github.io/EczemaPred/reference/BinRW.md) :
  Binomial random walk model
- [`OrderedRW`](https://ghurault.github.io/EczemaPred/reference/OrderedRW.md)
  : Ordered Logistic random walk model
- [`BinMC`](https://ghurault.github.io/EczemaPred/reference/BinMC.md) :
  Binomial Markov Chain model

### Reference models

- [`MC`](https://ghurault.github.io/EczemaPred/reference/MC.md) : Markov
  Chain model
- [`RW`](https://ghurault.github.io/EczemaPred/reference/RW.md) : Random
  walk model
- [`Smoothing`](https://ghurault.github.io/EczemaPred/reference/Smoothing.md)
  : Exponential smoothing model
- [`AR1`](https://ghurault.github.io/EczemaPred/reference/AR1.md) :
  Autoregressive model (order 1)
- [`MixedAR1`](https://ghurault.github.io/EczemaPred/reference/MixedAR1.md)
  : Mixed effect autoregressive model (order 1)

## Fitting models

- [`EczemaModel()`](https://ghurault.github.io/EczemaPred/reference/EczemaModel.md)
  : EczemaModel constructor
- [`EczemaFit()`](https://ghurault.github.io/EczemaPred/reference/EczemaFit.md)
  : Fit an EczemaModel
- [`sample_prior()`](https://ghurault.github.io/EczemaPred/reference/sample_prior.md)
  : Prior predictive distribution

## Manipulating models

- [`list_parameters()`](https://ghurault.github.io/EczemaPred/reference/list_parameters.md)
  : List available parameters

- [`default_prior()`](https://ghurault.github.io/EczemaPred/reference/default_prior.md)
  : Default prior

- [`validate_prior()`](https://ghurault.github.io/EczemaPred/reference/validate_prior.md)
  : Check the prior of an EczemaModel is correct

- [`print_prior()`](https://ghurault.github.io/EczemaPred/reference/print_prior.md)
  [`print_distribution()`](https://ghurault.github.io/EczemaPred/reference/print_prior.md)
  : Print prior distribution

- [`print(`*`<EczemaModel>`*`)`](https://ghurault.github.io/EczemaPred/reference/print.EczemaModel.md)
  : Print model

- [`prepare_standata()`](https://ghurault.github.io/EczemaPred/reference/prepare_standata.md)
  [`prepare_data_lgtd()`](https://ghurault.github.io/EczemaPred/reference/prepare_standata.md)
  : Prepare the data list to pass to the Stan sampler

- [`add_prior()`](https://ghurault.github.io/EczemaPred/reference/add_prior.md)
  : Add prior to the list serving as input to the Stan sampler

- [`replace_prior()`](https://ghurault.github.io/EczemaPred/reference/replace_prior.md)
  : Replace prior

- [`get_index()`](https://ghurault.github.io/EczemaPred/reference/get_index.md)
  [`get_index2()`](https://ghurault.github.io/EczemaPred/reference/get_index.md)
  : Associate (Patient, Time) pairs to corresponding index in the model

- [`make_empty_data()`](https://ghurault.github.io/EczemaPred/reference/make_empty_data.md)
  :

  Create "empty" train and test set to pass to `prepare_standata` for
  `sample_prior`

## Plotting models

- [`plot_post_traj_pmf()`](https://ghurault.github.io/EczemaPred/reference/plot_ppc.md)
  [`plot_post_traj_fanchart()`](https://ghurault.github.io/EczemaPred/reference/plot_ppc.md)
  [`plot_ppc_traj_pmf()`](https://ghurault.github.io/EczemaPred/reference/plot_ppc.md)
  [`plot_ppc_traj_fanchart()`](https://ghurault.github.io/EczemaPred/reference/plot_ppc.md)
  : Plot posterior predictive trajectory
- [`plot_latent_OrderedRW()`](https://ghurault.github.io/EczemaPred/reference/plot_latent_OrderedRW.md)
  : Plot the evolution of the expected latent score of the OrderedRW
  model
- [`plot_transition_MC()`](https://ghurault.github.io/EczemaPred/reference/plot_transition_MC.md)
  : Markov Chain expected transition matrix

## Validation

Utility functions for validating models

- [`get_fc_iteration()`](https://ghurault.github.io/EczemaPred/reference/forward_chaining.md)
  [`split_fc_dataset()`](https://ghurault.github.io/EczemaPred/reference/forward_chaining.md)
  [`detail_fc_training()`](https://ghurault.github.io/EczemaPred/reference/forward_chaining.md)
  [`get_fc_training_iteration()`](https://ghurault.github.io/EczemaPred/reference/forward_chaining.md)
  : Forward chaining
- [`extract_loglikelihood()`](https://ghurault.github.io/EczemaPred/reference/extract_metric.md)
  [`extract_lpd()`](https://ghurault.github.io/EczemaPred/reference/extract_metric.md)
  [`extract_RPS()`](https://ghurault.github.io/EczemaPred/reference/extract_metric.md)
  : Extract lpd (predictive log-likelihood) and RPS from stanfit object
- [`add_metrics1_d()`](https://ghurault.github.io/EczemaPred/reference/add_metrics.md)
  [`add_metrics1_c()`](https://ghurault.github.io/EczemaPred/reference/add_metrics.md)
  [`add_metrics2_d()`](https://ghurault.github.io/EczemaPred/reference/add_metrics.md)
  [`add_metrics2_c()`](https://ghurault.github.io/EczemaPred/reference/add_metrics.md)
  : Append lpd and (C)RPS to (test) dataframe
- [`add_predictions()`](https://ghurault.github.io/EczemaPred/reference/add_predictions.md)
  : Append lpd, (C)RPS and predictive samples to (test) dataframe
- [`add_uniform_pred()`](https://ghurault.github.io/EczemaPred/reference/add_uniform_pred.md)
  : Performance of a uniform forecast
- [`add_historical_pred()`](https://ghurault.github.io/EczemaPred/reference/add_historical_pred.md)
  : Performance of (population) historical forecast

## Miscellaneous

- [`get_compiled_model()`](https://ghurault.github.io/EczemaPred/reference/get_compiled_model.md)
  : Get compiled model
- [`generate_missing()`](https://ghurault.github.io/EczemaPred/reference/generate_missing.md)
  : Generate missing values in a times-series
- [`extract_parameters()`](https://ghurault.github.io/EczemaPred/reference/extract_parameters.md)
  : Extract parameters posterior summary statistics
- [`extract_simulations()`](https://ghurault.github.io/EczemaPred/reference/extract_simulations.md)
  : Extract simulations
- [`samples_to_list()`](https://ghurault.github.io/EczemaPred/reference/samples_to_list.md)
  : Process samples to a list that can be included to a dataframe
- [`add_fanchart()`](https://ghurault.github.io/EczemaPred/reference/add_fanchart.md)
  : Add fanchart to ggplot
- [`add_broken_pointline()`](https://ghurault.github.io/EczemaPred/reference/add_broken_pointline.md)
  : Add broken pointline to ggplot
- [`EczemaPred-package`](https://ghurault.github.io/EczemaPred/reference/EczemaPred-package.md)
  [`EczemaPred`](https://ghurault.github.io/EczemaPred/reference/EczemaPred-package.md)
  : The 'EczemaPred' package.
