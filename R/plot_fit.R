# Plot OrderedRW model latent score -------------------------------------------------------

#' Plot the evolution of the expected latent score of the OrderedRW model
#'
#' @param fit Stanfit object
#' @param id Dataframe linking index in fit to (Patient, Time) pairs, cf. output from [get_index()]
#' @param patient_id Patient ID
#'
#' @return Ggplot
#' - Horizontal lines correspond to the expected cut-offs
#' - Ribbons correspond to the CI of a logistic distribution
#'
#' @import ggplot2 dplyr
#'
#' @export
plot_latent_OrderedRW <- function(fit, id, patient_id) {

  stopifnot(is_stanfit(fit),
            all(c("y_lat", "ct") %in% fit@model_pars),
            is.data.frame(id),
            nrow(id) == fit@par_dims[["y_lat"]],
            all(c("Patient", "Time", "Index") %in% colnames(id)),
            patient_id %in% unique(id[["Patient"]]))

  # Extract mean latent score and cutpoints
  id1 <- filter(id, .data$Patient == patient_id)
  df <- mutate(id1, Mean = rstan::extract(fit, pars = paste0("y_lat[", id1[["Index"]], "]")) %>%
                 sapply(mean))
  ct <- rstan::extract(fit, pars = "ct")[[1]] %>%
    apply(2, mean)

  max_score <- length(ct)
  lvl <- seq(0.1, 0.9, 0.1)

  # Label location
  midpoint <- (ct - lag(ct))[-1] / 2
  midpoint <- ct[1:length(midpoint)] + midpoint
  midpoint <- c(ct[1] - 3.5, midpoint, ct[length(ct)] + 3.5)

  # Dataset containing CI of different levels
  ssi <- lapply(lvl,
                function(CI) {
                  z <- stats::qlogis(0.5 + CI / 2)
                  out <- mutate(df, Lower = .data$Mean - z, Upper = .data$Mean + z, Level = CI)
                  return(out)
                }) %>%
    bind_rows()

  p <- plot_fanchart(ssi) +
    geom_hline(yintercept = ct) +
    geom_label(data = data.frame(Label = paste0("y = ", 0:max_score), x = 1, y = midpoint),
               aes_string(x = "x", y = "y", label = "Label"), hjust = 0) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(y = "Expected latent score")

  return(p)
}
