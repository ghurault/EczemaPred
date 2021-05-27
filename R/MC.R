# Functions ---------------------------------------------------------------

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

#' Prepare data to pass to the Stan sampler for MC model
#'
#' @param train Training dataframe
#' @param test Testing dataframe
#' @param K Number of categories
#'
#' @details
#' - `train` and `test` should have columns `y0` (for the current state), `y1` (for the next state) and
#' `dt` (for the time delay between states).
#' - `y0` and `y1` should take integer values between 1 and K.
#' - `dt` should take integer values greater than or equal to 1.
#'
#' Missing values are not allowed.
#'
#' @export
prepare_data_MC <- function(train, test = NULL, K) {

  stopifnot(is_scalar_wholenumber(K),
            K > 1)
  stopifnot_MC_dataframe(train, K)

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

  if (!is.null(test)) {
    stopifnot_MC_dataframe(test, K)

    data_stan$N_test <- nrow(test)
    data_stan$y0_test <- array(test[["y0"]])
    data_stan$y1_test <- array(test[["y1"]])
    data_stan$dt_test <- array(test[["dt"]])
  }

  return(data_stan)
}
