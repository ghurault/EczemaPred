# Forward chaining

In a forward chaining setting, we retrain/update the model every
`horizon` day/week/months/etc. and test the model on the next `horizon`
day/week/months/etc., i.e. the data that is included at the next
training step. We call this training/testing steps iterations and start
at iteration 0 where only the first timepoint is only included. At
iteration 1, timepoints 1 to `1 + horizon` are included.

## Usage

``` r
get_fc_iteration(t, horizon)

split_fc_dataset(df, it)

detail_fc_training(df, horizon)

get_fc_training_iteration(it_test)
```

## Arguments

- t:

  Vector of timepoints

- horizon:

  Updating horizon

- df:

  Dataframe with columns `Patient`, `Time`, `Horizon`, `Iteration.` Only
  the columns Time is required for `detail_fc_training`.

- it:

  Iteration number

- it_test:

  Vector of testing iteration numbers

## Value

- `get_fc_iteration` returns a vector corresponding to iteration numbers

- `split_fc_dataset` returns a named list of dataframes (`Training` and
  `Testing`)

- `get_fc_training_iteration` returns a vector of unique training
  iteration number

- `detail_fc_training` returns a dataframe with columns: `Iteration`,
  `N`, `Proportion`, `LastTime`

## Details

- `get_fc_iteration` associate a vector of timepoints `t` to the
  corresponding iteration.

- `split_fc_dataset` split dataset `df` into a training and testing set,
  and computes prediction horizon and last available score in the test
  set.

- `get_fc_training_iteration` identify training iterations such as test
  set is not empty

- `detail_fc_training` derive training data characteristics for each
  iteration in `df`, including the number of training observations, the
  proportion of training observations on the total number of
  observations, and the maximum timepoint in the training sets.

Time=t is in Iteration=i means that:

- Time=t is in the new training data of iteration i

- Time=t is not in the training data of iterations \< i

- Time=t is in the testing data of iteration i-1

- Time=t is in the training data of iterations \>= i

## Examples

``` r
h <- 2
df <- get_index2(t_max = rpois(10, 10))
df$Score <- rnorm(nrow(df))
df$Iteration <- get_fc_iteration(df$Time, h)
sp <- split_fc_dataset(df, 1)
train_it <- get_fc_training_iteration(df$Iteration)
fc_char <- detail_fc_training(df, h)
```
