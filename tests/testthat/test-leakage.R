library(testthat)

# Leakage tests. A forecast issued at origin `c` may use only releases reported
# on or before `c`. Each test builds a revision archive, then alters every
# release reported after `c` and checks that nothing decided at `c` changes:
# the prepared features, the training rows and weights, the fitted models and
# the forecasts issued at `c`. Model files are named by a hash of their exact
# training data, weights and settings, so identical file names mean identical
# fits. A control alters releases reported on `c` to show the tests can fail.

# One location with daily (or weekly) reference dates, each released at
# several lags; later releases revise earlier ones upward.
make_archive <- function(weekly = FALSE, n_reference = 120, max_lag = 20) {
  set.seed(11)
  step <- if (weekly) 7 else 1
  references <- as.Date("2023-01-07") + step * (0:(n_reference - 1))
  lags <- step * (0:max_lag)
  archive <- expand.grid(ref = references, lag = lags)
  level <- 200 + 50 * sin(seq_along(references) / 9)
  completeness <- 1 - 0.5 * exp(-archive$lag / (4 * step))
  archive$value <- round(level[match(archive$ref, references)] * completeness +
                           stats::rnorm(nrow(archive), sd = 3))
  archive[order(archive$ref, archive$lag), ]
}

# Change every release reported after `cutoff` (or, for the control, on it).
alter_releases <- function(archive, cutoff, on_cutoff = FALSE) {
  report_date <- archive$ref + archive$lag
  changed <- if (on_cutoff) report_date == cutoff else report_date > cutoff
  archive$value[changed] <- archive$value[changed] * 3 + 7
  archive
}

prepare <- function(archive, ref_lag, weekly = FALSE) {
  data_preprocessing(archive, "value", "ref", "lag", ref_lag,
                     temporal_resol = if (weekly) "weekly" else "daily",
                     smoothed = weekly)
}

run_delphirf <- function(df, origin, cache, weekly = FALSE) {
  result <- suppressMessages(DelphiRF(
    df, testing_start_date = origin, working_scale = "log_value",
    temporal_resol = if (weekly) "weekly" else "daily",
    training_days = if (weekly) 364 else 60, taus = c(0.1, 0.5, 0.9),
    model_save_dir = cache
  ))
  list(
    models = sort(basename(list.files(cache, pattern = "[.]rds$", recursive = TRUE))),
    issued = result %>%
      dplyr::filter(report_date == origin) %>%
      dplyr::arrange(lag, reference_date) %>%
      dplyr::select(reference_date, lag, dplyr::starts_with("predicted_tau")) %>%
      as.data.frame()
  )
}

target_columns <- c("value_target", "value_target_7dav", "log_value_target",
                    "log_value_target_7dav", "target_source_lag", "target_value_source")

test_that("features of releases known at the origin ignore later releases", {
  archive <- make_archive()
  origin <- as.Date("2023-03-28")
  original <- prepare(archive, 10)
  altered <- prepare(alter_releases(archive, origin), 10)
  keep <- function(df) {
    df <- as.data.frame(df)
    df <- df[df$report_date <= origin, setdiff(names(df), target_columns)]
    df[order(df$reference_date, df$report_date), ]
  }
  expect_gt(nrow(keep(original)), 0)
  expect_identical(keep(altered), keep(original))
})

test_that("targets mature only through releases at or before the target lag", {
  archive <- make_archive()
  origin <- as.Date("2023-03-28")
  altered <- alter_releases(archive, origin)
  for (df in list(prepare(archive, 10), prepare(altered, 10))) {
    # A target known at the origin is the same in both archives.
    known <- as.data.frame(df)[df$target_date <= origin, ]
    expect_true(all(known$target_lag == 10))
    expect_true(all(is.na(known$target_source_lag) | known$target_source_lag <= 10))
  }
  known_targets <- function(df) {
    df <- unique(as.data.frame(df)[df$target_date <= origin, c("reference_date", "value_target")])
    df[order(df$reference_date), ]
  }
  expect_identical(known_targets(prepare(altered, 10)), known_targets(prepare(archive, 10)))
})

test_that("DelphiRF fits and forecasts at a daily origin use only data known then", {
  archive <- make_archive()
  origin <- as.Date("2023-03-28")
  cache_a <- tempfile("leak-a-"); cache_b <- tempfile("leak-b-"); cache_c <- tempfile("leak-c-")
  on.exit(unlink(c(cache_a, cache_b, cache_c), recursive = TRUE))

  original <- run_delphirf(prepare(archive, 10), origin, cache_a)
  altered <- run_delphirf(prepare(alter_releases(archive, origin), 10), origin, cache_b)

  expect_gt(length(original$models), 0)
  expect_gt(nrow(original$issued), 0)
  expect_identical(altered$models, original$models)
  expect_identical(altered$issued, original$issued)

  # Control: releases reported on the origin are known then, so altering them
  # must change the fitted models. Otherwise the checks above could not fail.
  control <- run_delphirf(prepare(alter_releases(archive, origin, on_cutoff = TRUE), 10),
                          origin, cache_c)
  expect_false(identical(control$models, original$models))
})

test_that("DelphiRF fits and forecasts at a weekly origin use only data known then", {
  archive <- make_archive(weekly = TRUE, n_reference = 90, max_lag = 12)
  origin <- as.Date("2023-01-07") + 7 * 70
  cache_a <- tempfile("leak-wa-"); cache_b <- tempfile("leak-wb-")
  on.exit(unlink(c(cache_a, cache_b), recursive = TRUE))

  original <- run_delphirf(prepare(archive, 28, weekly = TRUE), origin, cache_a, weekly = TRUE)
  altered <- run_delphirf(prepare(alter_releases(archive, origin), 28, weekly = TRUE),
                          origin, cache_b, weekly = TRUE)
  expect_gt(length(original$models), 0)
  expect_identical(altered$models, original$models)
  expect_identical(altered$issued, original$issued)
})

test_that("cross-validation given the whole archive uses only data known at its end date", {
  archive <- make_archive()
  end <- as.Date("2023-03-28")
  tune <- function(archive, cache) {
    df <- prepare(archive, 10)
    df <- add_weights_related(df, create_weight_anchors(df, end))
    tuned <- suppressMessages(cv_revision_forecast(
      df, test_lag = 3, taus = c(0.1, 0.5, 0.9), working_scale = "log_value",
      lambda_candidates = 0.1, gamma_candidates = c(0.1, 1), lag_pad_candidates = 1,
      n_folds = 2, training_end_date = end, training_days = 60, model_save_dir = cache
    ))
    list(models = sort(basename(list.files(cache, pattern = "[.]rds$", recursive = TRUE))),
         scores = tuned$scores)
  }
  cache_a <- tempfile("leak-cv-a-"); cache_b <- tempfile("leak-cv-b-")
  on.exit(unlink(c(cache_a, cache_b), recursive = TRUE))
  original <- tune(archive, cache_a)
  expect_gt(length(original$models), 0)
  expect_true(all(is.finite(original$scores$mean_wis_working)))
  altered <- tune(alter_releases(archive, end), cache_b)
  # The fold models and the validation scores that choose the hyperparameters.
  expect_identical(altered$models, original$models)
  expect_identical(altered$scores, original$scores)
})

test_that("revision_forecast given the whole archive uses only data known at its end date", {
  archive <- make_archive()
  end <- as.Date("2023-03-28")
  forecast_at_end <- function(archive, cache) {
    df <- prepare(archive, 10)
    df <- add_weights_related(df, create_weight_anchors(df, end))
    train <- df[df$lag == 3, ]   # every release at lag 3, including later ones
    issued <- df[df$lag == 3 & df$report_date == end, ]
    result <- revision_forecast(
      train, issued, taus = c(0.1, 0.5, 0.9), working_scale = "log_value",
      params_list = c("log_value_7dav", "log_value_7dav_lag7"),
      training_end_date = as.character(end), training_days = 60,
      test_lag_group = "3", model_save_dir = cache
    )
    list(models = sort(basename(list.files(cache, pattern = "[.]rds$", recursive = TRUE))),
         issued = as.data.frame(result[c("reference_date", "predicted_tau0.1",
                                         "predicted_tau0.5", "predicted_tau0.9")]))
  }
  cache_a <- tempfile("leak-rf-a-"); cache_b <- tempfile("leak-rf-b-")
  on.exit(unlink(c(cache_a, cache_b), recursive = TRUE))
  original <- forecast_at_end(archive, cache_a)
  expect_gt(nrow(original$issued), 0)
  expect_identical(forecast_at_end(alter_releases(archive, end), cache_b), original)
})

test_that("weights at an origin ignore later releases", {
  archive <- make_archive()
  origin <- as.Date("2023-03-28")
  weights_at <- function(archive) {
    df <- prepare(archive, 10)
    train <- df[df$report_date <= origin & df$target_date <= origin &
                  df$target_date > origin - 60, ]
    weighted <- add_weights_related(train, create_weight_anchors(df, origin))
    weighted <- weighted[order(weighted$reference_date, weighted$lag), ]
    observation_weights(weighted, gamma = 1)
  }
  expect_identical(weights_at(alter_releases(archive, origin)), weights_at(archive))
})
