# Initialisation ----------------------------------------------------------

max_score <- RW_setup$max_score

id <- get_index(RW_split$Training, RW_split$Testing)

RW_split2 <- lapply(RW_split, function(x) {mutate(x, Score = round(Score))}) # discretised

wrong_max_score <- list(
  as.character(max_score),
  rep(max_score, 2)
)

# Test plot_post_traj* ----------------------------------------------------

for (f in c(plot_post_traj_pmf, plot_post_traj_fanchart)) {

  test_that("plot_post_traj_* returns a ggplot object", {
    expect_s3_class(f(RW_fit, id = id, patient_id = 1, max_score = max_score), "ggplot")
    expect_s3_class(f(rstan::extract(RW_fit, pars = "y_rep")[[1]], id = id, patient_id = 1, max_score = max_score), "ggplot")
  })

  test_that("plot_post_traj_* catches errors in main inputs", {
    expect_error(f("RW_fit", id = id, patient_id = 1, max_score = max_score))
    expect_error(f(rstan::extract(RW_fit, pars = "y_rep")[[1]][, 1:RW_setup$t_max[1]], id = id, patient_id = 1, max_score = max_score))
    expect_error(f(RW_fit, id = get_index(train = RW_split2$Training, test = NULL), patient_id = 1, max_score = max_score))
    expect_error(f(RW_fit, id = id, patient_id = -1, max_score = max_score))
  })

}

test_that("plot_*_traj_pmf works when max_score is NA", {
  expect_s3_class(plot_ppc_traj_pmf(RW_fit, train = RW_split2$Training, test = RW_split2$Testing, patient_id = 1, max_score = NA), "ggplot")
})

test_that("plot_*_traj_pmf works when test is NULL", {
  expect_s3_class(plot_ppc_traj_pmf(RW_fit, train = bind_rows(RW_split2$Training, RW_split2$Testing), test = NULL, patient_id = 1, max_score = max_score), "ggplot")
})

test_that("add_trajectory works when missing values in df", {
  tmp <- RW_split2$Training %>%
    mutate(Label = "Training") %>%
    slice_sample(prop = 0.5)
  expect_s3_class(ggplot() + add_trajectory(tmp), "ggplot")
})

# Test plot_ppc_traj_* ----------------------------------------------------

for (g in c(plot_ppc_traj_pmf, plot_ppc_traj_fanchart)) {

  test_that("plot_ppc_traj_* returns a ggplot object", {
    expect_s3_class(g(RW_fit, train = RW_split2$Training, test = RW_split2$Testing, patient_id = 1, max_score = max_score), "ggplot")
    expect_s3_class(g(rstan::extract(RW_fit, pars = "y_rep")[[1]], train = RW_split2$Training, test = RW_split2$Testing, patient_id = 1, max_score = max_score), "ggplot")
  })

  test_that("plot_ppc_traj_* catches errors in main inputs", {
    expect_error(g("RW_fit", train = RW_split2$Training, test = RW_split2$Testing, patient_id = 1, max_score = max_score))
    expect_error(g(rstan::extract(RW_fit, pars = "y_rep")[[1]][, 1:RW_setup$t_max[1]], train = RW_split2$Training, test = RW_split2$Testing, patient_id = 1, max_score = max_score))
    expect_error(g(RW_fit, train = RW_split2$Training, test = RW_split2$Testing, patient_id = -1, max_score = max_score))
    expect_error(g(RW_fit, train = RW_split2$Training, test = NULL, patient_id = 1, max_score = max_score))
  })

}

test_that("plot_*_traj_pmf warns catches errors and warnings in max_score", {
  for (wms in wrong_max_score) {
    expect_warning(plot_post_traj_pmf(RW_fit, id = id, patient_id = 1, max_score = wms))
    expect_error(plot_ppc_traj_pmf(RW_fit, train = RW_split2$Training, test = RW_split2$Testing, patient_id = 1, max_score = wms))
  }
})

test_that("plot_*_traj_fanchart catches errors in max_score", {
  for (wms in wrong_max_score) {
    expect_error(plot_post_traj_fanchart(RW_fit, id = id, patient_id = 1, max_score = wms))
    expect_error(plot_ppc_traj_fanchart(RW_fit, train = RW_split2$Training, test = RW_split2$Testing, patient_id = 1, max_score = wms))
  }
})

test_that("plot_*_traj_pmf catches errors with max_scale", {

  wrong_max_scale <- list(
    100,
    c(0, 1),
    0.01 / (max_score + 1)
  )

  for (wms in wrong_max_scale) {
    expect_error(plot_post_traj_pmf(RW_fit, id = id, patient_id = 1, max_score = max_score, max_scale = wms))
    expect_error(plot_ppc_traj_pmf(RW_fit, id = id, patient_id = 1, max_score = max_score, max_scale = wms))
  }

})

test_that("plot_*_traj_pmf warns when max_score is not a wholenumber and support is not supplied", {
  expect_warning(plot_post_traj_pmf(RW_fit, id = id, patient_id = 1, max_score = max_score + 0.1))
  expect_warning(plot_ppc_traj_pmf(RW_fit, train = RW_split2$Training, test = RW_split2$Testing, patient_id = 1, max_score = max_score + 0.1))
})

test_that("plot_*_traj_fanchart returns a ggplot object for both eti and hdi intervals", {
  for (itv in c("eti", "hdi")) {
    expect_s3_class(plot_post_traj_fanchart(RW_fit, id = id, patient_id = 1, max_score = max_score, interval = itv),
                    "ggplot")
    expect_s3_class(plot_ppc_traj_fanchart(RW_fit, train = RW_split2$Training, test = RW_split2$Testing, patient_id = 1, max_score = max_score, interval = itv),
                    "ggplot")
  }
})

test_that("plot_*_traj_fanchart catches errors with intervals", {
  expect_error(plot_post_traj_fanchart(RW_fit, id = id, patient_id = 1, max_score = max_score, interval = "a"))
  expect_error(plot_ppc_traj_fanchart(RW_fit, train = RW_split2$Training, test = RW_split2$Testing, patient_id = 1, max_score = max_score, interval = "a"))
})

test_that("plot_*_traj_fanchart returns a ggplot object for a correct CI_level other than the default", {
  ci <- seq(0.2, 0.8, 0.2)
  expect_s3_class(plot_post_traj_fanchart(RW_fit, id = id, patient_id = 1, max_score = max_score, CI_level = ci),
                  "ggplot")
  expect_s3_class(plot_ppc_traj_fanchart(RW_fit, train = RW_split2$Training, test = RW_split2$Testing, patient_id = 1, max_score = max_score, CI_level = ci),
                  "ggplot")
})

test_that("plot_*_traj_fanchart catches errors with CI_level", {
  wrong_ci <- list(
    c("0.2", "0.4"),
    0.9,
    seq(5, 95, 5)
  )
  for (wci in wrong_ci) {
    expect_error(plot_ppc_traj_fanchart(RW_fit, train = RW_split2$Training, test = RW_split2$Testing, patient_id = 1, max_score = max_score, CI_level = wci))
  }
})
