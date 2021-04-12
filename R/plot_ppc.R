# Plot posterior predictive trajectory for longitudinal models
# NB: tests are located in test-BinRW.R

# Documentation -----------------------------------------------------------

#' Plot posterior predictive trajectory
#'
#' Plot as a probability mass function (suffix `_pmf`) or
#' as a fanchart (stacked confidence interval, suffix `_fanchart`).
#'
#' - plot_post_traj_* plots the posterior predictive trajectory only
#' - plot_ppc_traj_* overlays the observed trajectory to the posterior predictive trajectory
#'
#' @param obj Stanfit object or matrix of replications, with rows corresponding to samples and
#' columns corresponding to variables (there should be `nrow(id)` columns)
#' @param id Dataframe linking index in obj to (Patient, Time) pairs, cf. output from [get_index()]
#' @param train Training dataset used to obtain the fit
#' @param test Testing dataset used to obtain the fit (can be NULL)
#' @param patient_id Patient ID
#' @param max_score Maximum value that the score can take.
#' Can be NA, in that case the support of the pmf is estimated from the data (cf. [HuraultMisc::extract_distribution()]).
#' @param max_scale Maximum value that the legend display.
#' If NA, this chosen automatically
#' @param interval Type of confidence of interval to display, one of "eti" for equal-tailed intervals and
#' "hdi" for highest density interval.
#' @param CI_level Vector of confidence level to plot for the fanchart
#'
#' @return Ggplot
#'
#' @name plot_ppc
NULL

# Master function plot_post_traj ----------------------------------------------

#' Master functions to plot posterior predictive trajectory as a probability mass function or a fanchart
#'
#' Not exported
#'
#' @rdname plot_ppc
#' @noRd
plot_post_traj <- function(obj,
                           id,
                           patient_id,
                           type = c("pmf", "fanchart"),
                           max_score = NA,
                           max_scale = NA,
                           interval = c("eti", "hdi"),
                           CI_level = seq(0.1, 0.9, 0.1)) {

  type <- match.arg(type)

  stopifnot(is.data.frame(id),
            all(c("Patient", "Time", "Index") %in% colnames(id)),
            patient_id %in% unique(id[["Patient"]]))

  stopifnot(is_scalar(max_score))
  if (!is.na(max_score)) {
    stopifnot(is.numeric(max_score),
              max_score > 0)
  }

  # Extract replications
  id1 <- filter(id, .data$Patient == patient_id)
  if (is_stanfit(obj)) {
    stopifnot("y_rep" %in% obj@model_pars,
              nrow(id) == obj@par_dims[["y_rep"]])
    yrep <- rstan::extract(obj, pars = paste0("y_rep[", id1$Index, "]"))
    yrep <- do.call(cbind, yrep)
  } else {
    stopifnot(is.matrix(obj),
              ncol(obj) == nrow(id))
    yrep <- obj[, id1$Index]
  }

  if (type == "pmf") {

    if (!is.na(max_score)) {
      stopifnot(is_wholenumber(max_score))
      support <- 0:max_score
    } else {
      support <- NULL
    }

    ssd <- full_join(HuraultMisc::extract_distribution(yrep, "y_rep", type = "discrete", support = support),
                     mutate(id1, Index = 1:nrow(id1)),
                     by = "Index") %>%
      select(-.data$Index, -.data$Variable)

    p <- plot_pmf(ssd, max_scale)

  }

  if (type == "fanchart") {

    interval <- match.arg(interval)
    stopifnot(is.numeric(CI_level),
              length(CI_level) > 1,
              all(CI_level > 0 & CI_level < 1))

    ssi <- full_join(HuraultMisc::extract_distribution(yrep, "y_rep", type = interval, CI_level = CI_level),
                     mutate(id1, Index = 1:nrow(id1)),
                     by = "Index") %>%
      select(-.data$Index, -.data$Variable)

    p <- plot_fanchart(ssi) +
      coord_cartesian(ylim = c(0, max_score), expand = FALSE)

  }

  return(p)

}

#' Plot posterior predictive trajectory as a pmf
#'
#' Not exported
#'
#' @param ssd Dataframe summarising the distribution as a probability mass function
#' (output from [HuraultMisc::extract_distribution()] with type discrete).
#' @param max_scale See [plot_ppc]
#' @param palette Colour palette (character vector).
#' Default is a single-hue blue palette: `c("#FFFFFF", RColorBrewer::brewer.pal(n = 6, "Blues"))`.
#'
#' @return Ggplot
#' @noRd
plot_pmf <- function(ssd,
                     max_scale = NA,
                     palette = c("#FFFFFF", "#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD", "#08519C")) {

  stopifnot(is_scalar(max_scale))
  if (!is.na(max_scale)) {
    stopifnot(is.numeric(max_scale),
              dplyr::between(max_scale, max(ssd[["Probability"]]), 1))
  }

  # Plot
  p <- ggplot() +
    geom_tile(data = ssd,
              aes_string(x = "Time", y = "Value", fill = "Probability")) +
    scale_fill_gradientn(colours = palette, limits = c(0, max_scale)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))

  p <- p +
    labs(y = "Score") +
    theme_classic(base_size = 15)

  return(p)

}

#' Plot posterior predictive trajectory as a fanchart
#'
#' Not exported
#'
#' @param ssi Dataframe summarising the distribution as a confidence interval
#' (output from [HuraultMisc::extract_distribution()] with type eti or hdi).
#' @param palette Colour palette (character vector).
#' Default is a single-hue blue palette: `rev(c("#FFFFFF", RColorBrewer::brewer.pal(n = 6, "Blues")))`.
#'
#' @return Ggplot
#' @noRd
plot_fanchart <- function(ssi,
                          ylim = NULL,
                          palette = rev(c("#FFFFFF", "#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD", "#08519C"))) {

  lvl <- sort(unique(ssi[["Level"]]), decreasing = TRUE)

  p <- ggplot()
  # Prediction intervals (cf. fill cannot be an aesthetic with a ribbon)
  for (i in 1:length(lvl)) {
    p <- p + geom_ribbon(data = filter(ssi, .data$Level == lvl[i]),
                         aes_string(x = "Time", ymin = "Lower", ymax = "Upper", fill = "Level"))
  }

  # Formatting
  p <- p +
    scale_fill_gradientn(colours = palette, limits = c(0, 1), breaks = c(.1, .5, .9)) +
    labs(fill = "Confidence level") +
    theme_classic(base_size = 15)

  return(p)

}

# Master function plot_ppc_traj -------------------------------------------

#' @rdname plot_ppc
#' @import dplyr
#' @noRd
plot_ppc_traj <- function(obj,
                          train,
                          test,
                          patient_id,
                          type = c("pmf", "fanchart"),
                          max_score = NA,
                          max_scale = NA,
                          interval = c("eti", "hdi"),
                          CI_level = seq(0.1, 0.9, 0.1)) {

  # Posterior trajectory (and check inputs)
  id <- get_index(train = train, test = test)
  p <- plot_post_traj(obj = obj,
                      id = id,
                      patient_id = patient_id,
                      type = type,
                      max_score = max_score,
                      max_scale = max_scale,
                      interval = interval,
                      CI_level = CI_level)

  # Process dataframe
  if (!is.na(max_score)) {
    ms <- max_score
  } else {
    ms <- 1e3 # just to check train and test
  }
  stopifnot_lgtd_train(train, ms, discrete = (type == "pmf"))
  train <- mutate(train, Label = "Training")
  if (!is.null(test)) {
    stopifnot_lgtd_test(test, train, ms, discrete = (type == "pmf"))
    test <- mutate(test, Label = "Testing")
    df <- bind_rows(train, test)
  } else {
    df <- train
  }
  df <- df[, c("Patient", "Time", "Score", "Label")]
  stopifnot(patient_id %in% unique(df[["Patient"]]))

  # Overlay trajectory
  p <- add_trajectory(p,
                      filter(df, .data$Patient == patient_id))

  return(p)

}

#' Add trajectory to existing ggplot
#'
#' Not exported
#'
#' @param p Ggplot
#' @param df Dataframe
#'
#' @return Ggplot
#' @import ggplot2 dplyr
#' @noRd
add_trajectory <- function(p = ggplot(), df) {

  last_train_time <- df %>%
    filter(.data$Label == "Training") %>%
    summarise(max(.data$Time)) %>%
    pull()
  # Add missing values to have a broken line
  t_mis <- setdiff(1:max(df[["Time"]], na.rm = TRUE), df[["Time"]])
  if (length(t_mis) > 0) {
    df <- data.frame(Time = sort(t_mis)) %>%
      mutate(Label = case_when(.data$Time <= last_train_time ~ "Training",
                               TRUE ~ "Testing")) %>%
      bind_rows(df)
  }
  df <- df %>%
    mutate(Label = factor(.data$Label, levels = c("Training", "Testing"))) %>%
    arrange(.data$Time)

  p <- p +
    geom_path(data = df, aes_string(x = "Time", y = "Score", colour = "Label"), size = 1) +
    geom_point(data = df, aes_string(x = "Time", y = "Score", colour = "Label"), size = 1) + # cf. isolated missing values
    scale_colour_manual(values = c("#000000", "#E69F00"))

  p <- p +
    labs(y = "Score", colour = "") +
    theme_classic(base_size = 15)

  return(p)

}

# Exported functions ------------------------------------------------------

#' @rdname plot_ppc
#' @export
plot_post_traj_pmf <- function(obj, id, patient_id, max_score = NA, max_scale = NA) {
  plot_post_traj(obj = obj, id = id, patient_id = patient_id, type = "pmf", max_score = max_score, max_scale = max_scale)
}

#' @rdname plot_ppc
#' @export
plot_post_traj_fanchart <- function(obj,
                                    id,
                                    patient_id,
                                    max_score = NA,
                                    interval = c("eti", "hdi"),
                                    CI_level = seq(0.1, 0.9, 0.1)) {
  plot_post_traj(obj = obj,
                 id = id,
                 patient_id = patient_id,
                 type = "fanchart",
                 max_score = max_score,
                 interval = interval,
                 CI_level = CI_level)
}

#' @rdname plot_ppc
#' @export
plot_ppc_traj_pmf <- function(obj, train, test, patient_id, max_score = NA, max_scale = NA) {
  plot_ppc_traj(obj = obj,
                train = train,
                test = test,
                patient_id = patient_id,
                type = "pmf",
                max_score = max_score,
                max_scale = max_scale)
}

#' @rdname plot_ppc
#' @export
plot_ppc_traj_fanchart <- function(obj,
                                   train,
                                   test,
                                   patient_id,
                                   max_score = NA,
                                   interval = c("eti", "hdi"),
                                   CI_level = seq(0.1, 0.9, 0.1)) {
  plot_ppc_traj(obj = obj,
                train = train,
                test = test,
                patient_id = patient_id,
                type = "fanchart",
                max_score = max_score,
                interval = interval,
                CI_level = CI_level)
}
