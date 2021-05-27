# Helper ------------------------------------------------------------------

#' Stop if the dataframe is not a correct input for the Markov Chain model.
#'
#' NB: the function assumes that K is an integer greater than 2.
#'
#' @param df Dataframe to test
#' @param K Number of states of the Markov Chain
#'
#' @return NULL if all statements are TRUE, otherwise an error message
#' @noRd
#'
#' @seealso [base::stopifnot()]
stopifnot_MC_dataframe <- function(df, K) {

  stopifnot(is.data.frame(df),
            all(c("y0", "y1", "dt") %in% colnames(df)),
            all(df[["y0"]] %in% 1:K),
            all(df[["y1"]] %in% 1:K),
            all(is_wholenumber(df[["dt"]])),
            all(df[["dt"]] > 0))

}

# Documentation -----------------------------------------------------------

#' Markov Chain model
#'
#' - `default_prior_MC` returns the default prior
#' - `prepare_data_MC` prepares the data list to pass to the Stan sampler
#' - `fit_MC` fits the Markov Chain model
#' - `sample_prior_MC` samples the prior predictive distribution
#'
#' @param train Training dataframe
#' @param test Testing dataframe
#' @param K Number of states of the Markov Chain
#' @param prior A named list of corresponding to parameters' prior.
#' The transition probabilities from state i `p[i, ]` is assumed to follow a Dirichlet distribution.
#' The prior should be a matrix where each line correspond to the parameters of the Dirichlet distribution for `p[i, ]`.
#' @param ... Arguments to be pass to [rstan::sampling()]
#'
#' @return
#' - `default_prior_MC`: list corresponding to a uniform Dirichlet prior of concentration for each distribution `p[i, ]`
#' - `prepare_data_MC`: list to serve as input to the Stan sampler.
#' - `fit_MC`: Stanfit object
#' - `sample_prior_MC`: Stanfit object
#'
#' @section Dataframe format:
#'
#' - `train` and `test` should have columns `y0` (for the current state), `y1` (for the next state) and
#' `dt` (for the time delay between states).
#' - `y0` and `y1` should take integer values between 1 and K.
#' - `dt` should take integer values greater than or equal to 1.
#'
#' Missing values are not allowed.
#'
#' @section Model's parameters:
#'
#' - `p`: matrix of size K * K where `p[i, j]` represents the transition probabilities from state i to state j.
#'
#' See `list_parameters(model = "MC")`.
#'
#' @details
#' For more details see the [Markov Chain vignette](https://ghurault.github.io/EczemaPred/articles/MC.html).
#'
#' @name MC
NULL

# Functions ---------------------------------------------------------------

#' Stop if the prior input is not valid for the Markov Chain model
#'
#' @param prior A named list of corresponding to parameters' prior. See [default_prior_MC()].
#' @param K Number of states of the Markov Chain
#'
#' @return NULL if all statements are TRUE, otherwise an error message
#'
#' @seealso [base::stopifnot()]
#'
#' @noRd
stopifnot_prior_MC <- function(prior, K) {
  stopifnot(is.list(prior),
            "p" %in% names(prior),
            is.matrix(prior$p),
            dim(prior$p) == c(K, K),
            all(prior$p > 0))
}

#' @rdname MC
#' @export
default_prior_MC <- function(K) {
  stopifnot(is_scalar_wholenumber(K),
            K > 1)
  list(p = matrix(1, nrow = K, ncol = K))
}


#' @rdname MC
#' @export
prepare_data_MC <- function(train, test = NULL, K, prior = default_prior_MC(K)) {

  stopifnot(is_scalar_wholenumber(K),
            K > 1)
  stopifnot_MC_dataframe(train, K)
  stopifnot_prior_MC(prior, K)

  data_stan <- list(
    K = K,

    N = nrow(train),
    y0 = array(train[["y0"]]),
    y1 = array(train[["y1"]]),
    dt =  array(train[["dt"]]),

    run = 1,

    N_test = 0,
    y0_test = vector(),
    y1_test = vector(),
    dt_test = vector()
  )

  data_stan <- add_prior(data_stan, prior)

  if (!is.null(test)) {
    stopifnot_MC_dataframe(test, K)

    data_stan$N_test <- nrow(test)
    data_stan$y0_test <- array(test[["y0"]])
    data_stan$y1_test <- array(test[["y1"]])
    data_stan$dt_test <- array(test[["dt"]])
  }

  return(data_stan)
}

#' @rdname MC
#' @export
fit_MC <- function(train, test = NULL, K, prior = default_prior_MC(K), ...) {

  data_stan <- prepare_data_MC(train = train, test = test, K = K, prior = prior)

  fit <- rstan::sampling(stanmodels$MC, data = data_stan, ...)

  return(fit)

}

#' @rdname MC
#' @export
sample_prior_MC <- function(train = data.frame(y0 = integer(), y1 = integer(), dt = integer()),
                            K,
                            prior = default_prior_MC(K),
                            ...) {

  data_stan <- prepare_data_MC(train = train, test = NULL, K = K, prior = prior)
  data_stan$run <- 0

  fit <- rstan::sampling(stanmodels$MC, data = data_stan, ...)

  return(fit)

}

# Plot --------------------------------------------------------------------

#' Markov Chain expected transition matrix
#'
#' @param fit Stanfit object corresponding to the Markov Chain model
#'
#' @return Ggplot
#' @export
#' @import dplyr tidyr ggplot2
plot_transition_MC <- function(fit) {

  p <- rstan::extract(fit, pars = "p")[[1]]
  p_mean <- apply(p, c(2, 3), mean)

  palette <- c("#FFFFFF", "#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD", "#08519C") # cf. RColorBrewer::brewer.pal(n = 6, "Blues")

  as.data.frame(p_mean) %>%
    mutate(Initial = 1:nrow(p_mean)) %>%
    pivot_longer(-.data$Initial, names_to = "Final", values_to = "Probability") %>%
    mutate(Final = as.numeric(gsub("V", "", .data$Final))) %>%
    ggplot(aes_string(x = "Final", y = "Initial", fill = "Probability")) +
    geom_tile() +
    scale_fill_gradientn(colours = palette, limits = c(0, NA)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_classic(base_size = 15)

}
