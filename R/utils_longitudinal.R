# get_index ---------------------------------------------------------------

#' Associate (Patient, Time) pairs to corresponding index in the model
#'
#' @param train Training dataframe
#' @param test Testing dataframe
#' @param t_max Vector indicating the length of each patient time-series
#'
#' @return Dataframe with columns Patient, Time, Index
#'
#' @details These functions are not designed for use with the Markov Chain model (MC).
#'
#' @export
#' @import dplyr
#'
#' @name get_index
#'
#' @examples
#' library(dplyr)
#' id <- get_index2(t_max = rpois(10, 20))
#' df <- id %>% select(-Index) %>% slice_sample(prop = 0.9) %>% arrange(Patient, Time)
#' get_index(train = df)
NULL

#' @rdname get_index
#' @import dplyr
#' @export
get_index <- function(train, test = NULL) {

  stopifnot_lgtd_id(train)

  if (!is.null(test)) {
    stopifnot_lgtd_id(test)
    train <- bind_rows(train, test)
  }

  full_df <- train %>%
    select(.data$Patient, .data$Time)

  stopifnot(all(!is.na(full_df)))

  tmp <- full_df %>%
    group_by(.data$Patient) %>%
    summarise(t_max = max(.data$Time)) %>%
    arrange(.data$Patient)

  out <- lapply(1:nrow(tmp),
                function(i) {
                  tibble(Patient = tmp[[i, "Patient"]],
                         Time = 1:tmp[[i, "t_max"]])
                }) %>%
    bind_rows() %>%
    mutate(Index = 1:n())

  return(out)
}

#' @rdname get_index
#' @export
get_index2 <- function(t_max) {
  get_index(train = tibble(Patient = seq_along(t_max), Time = t_max), test = NULL)
}

# Extract simulations -----------------------------------------------------

#' Extract simulations
#'
#' @param fit Stanfit object
#' @param id Dataframe linking index in Stan model to (Patient, Time) pairs, cf. output from [get_index()]
#' @param draw Draw ID
#' @param pars Vector of parameters to extract. Default to all parameters except `y_rep`.
#'
#' @return Named list:
#' - Data: dataframe with columns Patient, Time, Index, and Score corresponding to simulations
#' - Parameters: dataframe containing parameters used to generate the data (cf. [HuraultMisc::extract_draws()])
#'
#' @details This function is not designed to use with the Markov Chain model (MC).
#'
#' @import dplyr
#' @export
extract_simulations <- function(fit, id, draw, pars = NULL) {

  stopifnot(is_stanfit(fit),
            is.data.frame(id),
            all(c("Patient", "Time", "Index") %in% colnames(id)),
            is_scalar_wholenumber(draw),
            draw > 0)

  if (is.null(pars)) {
    pars <- setdiff(fit@model_pars, "y_rep")
  } else {
    stopifnot(is.vector(pars, mode = "character"))
  }
  param <- rstan::extract(fit, pars = pars) %>%
    HuraultMisc::extract_draws(draws = draw)

  yrep <- rstan::extract(fit, pars = "y_rep")[[1]]
  df <- mutate(id, Score = yrep[draw, ])

  return(list(Data = df,
              Parameters = param))
}
