library(testthat)

test_that("delphirf_nowcast censors releases and restores locations", {
  raw <- data.frame(
    geo_value = rep(c("a", "b"), each = 3L),
    reference_date = rep(as.Date("2024-01-01") + 0:2, 2L),
    report_date = rep(as.Date("2024-01-02") + 0:2, 2L),
    lag = 1L,
    value = 1:6
  )
  seen <- new.env(parent = emptyenv())
  seen$preprocessing <- list()
  seen$engine <- list()

  local_mocked_bindings(
    data_preprocessing = function(df, value_col, refd_col, lag_col, ref_lag,
                                  temporal_resol, target_as_of_date, ...) {
      seen$preprocessing[[length(seen$preprocessing) + 1L]] <- list(
        df = df, lag_col = lag_col, ref_lag = ref_lag,
        temporal_resol = temporal_resol,
        target_as_of_date = target_as_of_date)
      data.frame(
        reference_date = as.Date(df[[refd_col]]),
        report_date = as.Date(df[[refd_col]]) + df[[lag_col]],
        target_date = as.Date(df[[refd_col]]) + ref_lag,
        lag = df[[lag_col]], genuine_event = TRUE)
    },
    delphirf_nowcast_preprocessed = function(df, testing_start_date, geo,
                                             working_scale, genuine_testing, ...) {
      seen$engine[[length(seen$engine) + 1L]] <- list(
        df = df, origin = testing_start_date, geo = geo,
        working_scale = working_scale, genuine_testing = genuine_testing)
      tibble::as_tibble(df[df$report_date >= testing_start_date, , drop = FALSE])
    },
    .package = "DelphiRF"
  )

  result <- delphirf_nowcast(
    raw, as_of = as.Date("2024-01-03"), ref_lag = 14,
    model_save_dir = tempfile("models-"))

  expect_length(seen$preprocessing, 2L)
  expect_true(all(vapply(seen$preprocessing, function(x) nrow(x$df) == 2L,
                         logical(1))))
  expect_true(all(vapply(seen$preprocessing, function(x) x$lag_col == "lag",
                         logical(1))))
  expect_true(all(vapply(seen$preprocessing, function(x) x$ref_lag == 14,
                         logical(1))))
  expect_true(all(vapply(seen$preprocessing,
                         function(x) identical(x$target_as_of_date, as.Date("2024-01-03")),
                         logical(1))))
  expect_setequal(vapply(seen$engine, `[[`, character(1), "geo"), c("a", "b"))
  expect_true(all(vapply(seen$engine, `[[`, character(1), "working_scale") ==
                    "log_value"))
  expect_true(all(!vapply(seen$engine, `[[`, logical(1), "genuine_testing")))
  expect_true(all(result$forecast_date == as.Date("2024-01-03")))
  expect_setequal(result$geo_value, c("a", "b"))
})

test_that("delphirf_nowcast requires a valid lag column", {
  raw <- data.frame(
    reference_date = as.Date("2024-01-01"),
    value = 10
  )
  expect_error(delphirf_nowcast(raw), "missing required column.*lag")
  raw$lag <- 1.5
  expect_error(delphirf_nowcast(raw), "whole numbers")
})

test_that("delphirf_nowcast infers fraction working scale and weekly resolution", {
  raw <- data.frame(
    reference_date = as.Date("2024-01-06") + 7 * 0:2,
    lag = rep(7L, 3L), numerator = 1:3, denominator = 4:6
  )
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    data_preprocessing = function(df, refd_col, lag_col, ref_lag,
                                  temporal_resol, ...) {
      seen$preprocess_resolution <- temporal_resol
      data.frame(reference_date = df[[refd_col]],
                 report_date = df[[refd_col]] + df[[lag_col]],
                 target_date = df[[refd_col]] + ref_lag,
                 lag = df[[lag_col]], genuine_event = TRUE)
    },
    delphirf_nowcast_preprocessed = function(df, testing_start_date,
                                             working_scale, temporal_resol, ...) {
      seen$working_scale <- working_scale
      seen$engine_resolution <- temporal_resol
      tibble::as_tibble(df[df$report_date == testing_start_date, , drop = FALSE])
    },
    .package = "DelphiRF"
  )
  delphirf_nowcast(
    raw, value_col = c("numerator", "denominator"), value_type = "fraction",
    as_of = as.Date("2024-01-27"), model_save_dir = tempfile("models-"))
  expect_identical(seen$preprocess_resolution, "weekly")
  expect_identical(seen$engine_resolution, "weekly")
  expect_identical(seen$working_scale, "log_ratio")
})

test_that("delphirf_backtest preprocesses once and reuses the full archive", {
  raw <- data.frame(
    reference_date = as.Date("2024-01-01") + 0:4,
    report_date = as.Date("2024-01-02") + 0:4,
    lag = 1L,
    value = 1:5
  )
  seen <- as.Date(character())
  preprocessing_calls <- 0L
  prepared_rows <- integer()
  local_mocked_bindings(
    data_preprocessing = function(df, refd_col, lag_col, ref_lag, ...) {
      preprocessing_calls <<- preprocessing_calls + 1L
      data.frame(reference_date = df[[refd_col]],
                 report_date = df[[refd_col]] + df[[lag_col]],
                 target_date = df[[refd_col]] + ref_lag,
                 lag = df[[lag_col]], genuine_event = TRUE)
    },
    delphirf_nowcast_preprocessed = function(df, testing_start_date, ...) {
      seen <<- c(seen, testing_start_date)
      prepared_rows <<- c(prepared_rows, nrow(df))
      out <- tibble::as_tibble(
        df[df$report_date >= testing_start_date, , drop = FALSE])
      attr(out, "dropped_test_rows") <- data.frame()
      out
    },
    .package = "DelphiRF"
  )
  dates <- as.Date(c("2024-01-04", "2024-01-02", "2024-01-04"))
  result <- delphirf_backtest(raw, dates,
                              model_save_dir = tempfile("models-"))
  expect_identical(preprocessing_calls, 1L)
  expect_identical(prepared_rows, c(5L, 5L))
  expect_identical(seen, as.Date(c("2024-01-02", "2024-01-04")))
  expect_identical(result$forecast_date, seen)
})

test_that("high-level wrappers reject invalid dates and reserved dots", {
  raw <- data.frame(reference_date = as.Date("2024-01-01"), lag = 0, value = 1)
  expect_error(delphirf_nowcast(raw, as_of = "not-a-date"), "valid")
  expect_error(delphirf_nowcast(raw, working_scale = "log_value"),
               "wrapper-controlled")
  expect_error(delphirf_backtest(raw, as.Date(character())), "at least one")
})
