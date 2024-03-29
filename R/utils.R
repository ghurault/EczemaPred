# Pipe --------------------------------------------------------------------

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

# Add prior to stan list ---------------------------------------------------------------

#' Add prior to the list serving as input to the Stan sampler
#'
#' Used internally
#'
#' @param data_stan List
#' @param prior A named list of corresponding to parameters' prior
#' @param prefix Prefix to add to the names of prior
#'
#' @return data_stan with additional items corresponding to the prior.
#'
#' @export
add_prior <- function(data_stan, prior, prefix = "prior_") {

  stopifnot(is.list(data_stan),
            is.list(prior),
            is_scalar(prefix),
            is.character(prefix))

  if (length(prior) > 0) {
    stopifnot(!is.null(names(prior)))
    names(prior) <- paste0(prefix, names(prior))
  }

  return(c(data_stan, prior))
}

# Compiled model ----------------------------------------------------------

#' Get compiled model
#'
#' @param stanmodel Stan model name.
#' NB: this may differ from the name of the model
#'
#' @return Compiled model (object to pass to `rstan::sampling`)
#' @export
get_compiled_model <- function(stanmodel) {
  stopifnot(is_scalar(stanmodel),
            is.character(stanmodel))
  if (!(stanmodel %in% names(stanmodels))) {
    stop("stanmodel should be one of ", paste(stanmodels, collapse = ", "))
  }
  return(stanmodels[[stanmodel]])
}

# Samples to list ---------------------------------------------------------

#' Process samples to a list that can be included to a dataframe
#'
#' @param object Stanfit object containing variable `par_name` or
#' matrix with rows representing samples, columns representing variables.
#' @param n_samples How many samples to return.
#' Default (=NULL) to all samples.
#' @param par_name Name of variable to extract when `object` is a stanfit object.
#' Default to `"y_pred"` (variable containing predictions).
#'
#' @return List of vector of samples
#'
#' @export
#'
#' @examples
#' samples_to_list(matrix(rnorm(1e3), nrow = 1e2))
samples_to_list <- function(object, par_name = "y_pred", n_samples = NULL) {

  if (is_stanfit(object)) {
    stopifnot(is_scalar(par_name),
              is.character(par_name),
              par_name %in% object@model_pars)
    pred <- rstan::extract(object, pars = par_name)[[1]]
  } else {
    pred <- object
  }
  stopifnot(is.matrix(pred))

  if (!is.null(n_samples)) {
    stopifnot(is_scalar_wholenumber(n_samples),
              n_samples > 0)
    pred <- pred[sample(1:nrow(pred), size = n_samples, replace = TRUE), ]
  }
  out <- lapply(1:ncol(pred), function(i) {pred[, i]})

  return(out)

}

# Fanchart ----------------------------------------------------------------

#' Add fanchart to ggplot
#'
#' The fanchart is obtained by overlaying [ggplot2::geom_ribbon()] of different widths
#' (corresponding to different levels).
#' This function is not a geom.
#'
#' @section Alternative:
#' A similar result can be obtained using [`ggdist::geom_lineribbon()`](https://mjskay.github.io/ggdist/),
#' with the difference that the `ggdist` function also plots a point estimate (and is a proper geom).
#' To avoid plotting the point estimate, `size` can be set to 0 and `y = .lower` for example.
#'
#' @param df Data with columns `aes_x`, `aes_ymin`, `aes_ymax` and `aes_fill`
#' @param aes_x Name of the `x` aesthetic
#' @param aes_ymin Name of the `ymin` aesthetic
#' @param aes_ymax Name of the `ymax` aesthetic
#' @param aes_fill Name of the `fill` aesthetic
#' @param legend_fill Whether the legend should be displayed as `continuous` or as `discrete` categories
#' @param labs_fill Name to give to the legend
#' @param palette Colour palette to use.
#' The default is the single-hue blue palette from `RColorBrewer::brewer.pal(n = 6, "Blues")`.
#'
#' @return List to be added to a ggplot
#'
#' @import ggplot2 dplyr
#' @export
#'
#' @examples
#'
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#'
#' tmp <- tibble(Time = 0:10,
#'               y = Time^1.5) %>%
#'   expand_grid(Level = seq(0.1, 0.9, 0.2)) %>%
#'   mutate(Width = qnorm(0.5 + Level / 2, sd = 2),
#'          Lower = y - Width,
#'          Upper = y + Width)
#' ggplot() + add_fanchart(tmp)
#'
add_fanchart <- function(df,
                         aes_x = "Time",
                         aes_ymin = "Lower",
                         aes_ymax = "Upper",
                         aes_fill = "Level",
                         legend_fill = c("continuous", "discrete"),
                         labs_fill = ifelse(legend_fill == "continuous", "Confidence level", "Probability"),
                         palette = c("#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD", "#08519C")) {

  legend_fill <- match.arg(legend_fill)
  stopifnot(is.data.frame(df),
            all(c(aes_x, aes_ymin, aes_ymax, aes_fill) %in% colnames(df)),
            is_scalar(labs_fill),
            is.character(labs_fill),
            is.vector(palette, mode = "character"))

  if (legend_fill == "continuous") {
    palette <- rev(c("#FFFFFF", palette)) # add white for gradient
  }

  lvl <- sort(unique(df[[aes_fill]]), decreasing = TRUE)

  stopifnot(legend_fill == "continuous" || length(lvl) <= length(palette))

  # Overlaying ribbons (cf. fill cannot be an aesthetic with a ribbon)
  out <- lapply(seq_along(lvl),
                function(i) {
                  tmp <- filter(df, .data$Level == lvl[i])
                  if (legend_fill == "continuous") {
                    geom_ribbon(data = tmp,
                                aes_(x = as.name(aes_x), ymin = as.name(aes_ymin), ymax = as.name(aes_ymax), fill = as.name(aes_fill)))
                  } else {
                    geom_ribbon(data = tmp,
                                aes_(x = as.name(aes_x), ymin = as.name(aes_ymin), ymax = as.name(aes_ymax), fill = as.character(lvl[i])))
                  }

                })

  out <- c(out,
           ifelse(legend_fill == "continuous",
                  list(scale_fill_gradientn(colours = palette, limits = c(0, 1), breaks = c(.1, .5, .9))),
                  list(scale_fill_manual(values = stats::setNames(palette[seq_along(lvl)], lvl))))
  )

  out <- c(out,
           list(
             labs(fill = labs_fill),
             theme_classic(base_size = 15)
           ))

  return(out)

}

# Plot broken "pointline" -------------------------------------------------

#' Add broken pointline to ggplot
#'
#' @param df Data with columns `aes_x` and `aes_y`.
#' @param aes_x Name of the `x` aesthetic.
#' @param aes_y Name of the `y` aesthetic.
#' @param size Size of the line and the points.
#' @param ... Other aesthetics to pass to [ggplot2::aes_()].
#' Valid aesthetics are the aesthetics for [ggplot2::geom_path()] and [ggplot2::geom_point()] (except `size` that is fixed).
#' NB: if we want the colour to change with `Group` we would need to add `colour = as.name(Group)`.
#'
#' @return List to add to ggplot.
#'
#' @import ggplot2 dplyr
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' df1 <- tibble(x = 1:100, y = cumsum(rnorm(100))) %>%
#'   slice_sample(prop = .8) %>%
#'   arrange(x)
#'
#' ggplot() +
#'   add_broken_pointline(df1) +
#'   theme_bw(base_size = 15)
#'
#' df2 <- mutate(df1, Group = case_when(x < 60 ~ "A", TRUE ~ "B"))
#'
#' ggplot() +
#'   add_broken_pointline(df2, colour = as.name("Group")) +
#'   scale_colour_discrete(na.translate = FALSE) +
#'   theme_bw(base_size = 15)
#'
add_broken_pointline <- function(df, aes_x = "x", aes_y = "y", size = 1, ...) {

  stopifnot(is.data.frame(df),
            all(c(aes_x, aes_y) %in% colnames(df)))

  # Add missing values to have a broken line
  x_mis <- setdiff(1:max(df[[aes_x]], na.rm = TRUE), df[[aes_x]])
  if (length(x_mis) > 0) {
    df <- tibble(x = x_mis) %>%
      rename(stats::setNames("x", aes_x)) %>%
      bind_rows(df) %>%
      arrange(across(all_of(aes_x)))
  }

  out <- list(
    geom_path(data = df, aes_(x = as.name(aes_x), y = as.name(aes_y), ...), size = size),
    geom_point(data = df, aes_(x = as.name(aes_x), y = as.name(aes_y), ...), size = size) # cf. isolated missing values
  )

  return(out)

}
