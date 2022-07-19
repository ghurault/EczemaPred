# Plot OrderedRW model latent score -------------------------------------------------------

#' Plot the evolution of the expected latent score of the OrderedRW model
#'
#' @param fit Stanfit object
#' @param id Dataframe linking index in fit to (Patient, Time) pairs, cf. output from [get_index()]
#' @param patient_id Patient ID
#' @param ... Arguments to pass to [add_fanchart()]
#'
#' @return Ggplot
#' - Horizontal lines correspond to the expected cut-offs
#' - Ribbons correspond to the CI of a logistic distribution
#'
#' @import ggplot2 dplyr
#'
#' @export
plot_latent_OrderedRW <- function(fit, id, patient_id, ...) {

  stopifnot(is_stanfit(fit),
            all(c("y_lat", "ct", "sigma_meas") %in% fit@model_pars),
            is.data.frame(id),
            nrow(id) == fit@par_dims[["y_lat"]],
            all(c("Patient", "Time", "Index") %in% colnames(id)),
            patient_id %in% unique(id[["Patient"]]))

  # Extract mean latent score and cutpoints
  id1 <- filter(id, .data$Patient == patient_id)
  df <- id1 %>%
    mutate(Mean = rstan::extract(fit, pars = paste0("y_lat[", id1[["Index"]], "]")) %>%
             vapply(mean, numeric(1)))
  ct <- rstan::extract(fit, pars = "ct")[[1]] %>%
    apply(2, mean)
  sigma_meas <- rstan::extract(fit, pars = "sigma_meas")[[1]] %>%
    mean()
  s <- sigma_meas * sqrt(3) / pi

  max_score <- length(ct)
  lvl <- seq(0.1, 0.9, 0.1)

  # Label location
  midpoint <- (ct - lag(ct))[-1] / 2
  midpoint <- ct[1:length(midpoint)] + midpoint
  midpoint <- c(ct[1] - 1, midpoint, ct[length(ct)] + 1)

  # Dataset containing CI of different levels
  ssi <- lapply(lvl,
                function(CI) {
                  z <- stats::qlogis(0.5 + CI / 2, scale = s)
                  out <- mutate(df, Lower = .data$Mean - z, Upper = .data$Mean + z, Level = CI)
                  return(out)
                }) %>%
    bind_rows()

  p <- ggplot() +
    add_fanchart(df = ssi, ...) +
    geom_hline(yintercept = ct) +
    geom_label(data = data.frame(Label = paste0("y = ", 0:max_score), x = 1, y = midpoint),
               aes_string(x = "x", y = "y", label = "Label"), hjust = 0) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(y = "Expected latent score")

  return(p)
}

# Plot MC transition matrix -----------------------------------------------

#' Markov Chain expected transition matrix
#'
#' @param fit Stanfit object corresponding to the Markov Chain model
#' @param max_scale Maximum value that the legend display.
#' If NA, this chosen automatically.
#' @param show_text Whether to display the probability as text in the heatmap.
#'
#' @return Ggplot
#' @export
#' @import dplyr tidyr ggplot2
plot_transition_MC <- function(fit, max_scale = NA, show_text = FALSE) {

  stopifnot(is_scalar(max_scale),
            is.na(max_scale) || dplyr::between(max_scale, 0, 1),
            is_scalar(show_text),
            is.logical(show_text))

  p <- rstan::extract(fit, pars = "p")[[1]]
  dp <- dim(p)
  stopifnot(length(dp) == 3,
            dp[2] == dp[3])
  K <- dp[2]
  p_mean <- apply(p, c(2, 3), mean)
  dimnames(p_mean) <- list(NULL, paste0("F", 1:K))

  palette <- c("#FFFFFF", "#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD", "#08519C") # cf. RColorBrewer::brewer.pal(n = 6, "Blues")

  out <- as_tibble(p_mean) %>%
    mutate(Initial = 1:nrow(p_mean)) %>%
    pivot_longer(-.data$Initial, names_to = "Final", values_to = "Probability") %>%
    mutate(Final = as.numeric(gsub("F", "", .data$Final)),
           Label = signif(Probability, 2)) %>%
    ggplot(aes_string(x = "Final", y = "Initial", fill = "Probability")) +
    geom_tile() +
    scale_fill_gradientn(colours = palette, limits = c(0, max_scale)) +
    scale_x_continuous(expand = c(0, 0), breaks = 1:K) +
    scale_y_continuous(expand = c(0, 0), breaks = 1:K) +
    theme_classic(base_size = 15)

  if (show_text) {
    out <- out + geom_text(aes_string(label = "Label"))
  }

  return(out)

}
