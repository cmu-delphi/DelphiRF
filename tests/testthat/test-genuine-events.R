library(testthat)

test_that("genuine-event filtering defaults are explicit", {
  defaults <- formals(DelphiRF)
  expect_identical(defaults$genuine_training, TRUE)
  expect_identical(defaults$genuine_testing, TRUE)

  argument_names <- names(defaults)
  expect_identical(
    tail(argument_names, 5L),
    c("onehot_weekdays", "genuine_training", "genuine_testing",
      "model_backend", "time_limit")
  )
  expect_identical(defaults$model_backend, quote(c("quantreg", "quantgen")))
})

test_that("DelphiRF requires genuine-event metadata by default", {
  df <- data.frame(
    report_date = as.Date("2024-01-01"),
    target_date = as.Date("2024-01-02"),
    lag = 1L
  )
  expect_error(
    DelphiRF(df, testing_start_date = as.Date("2024-01-02")),
    "genuine_event column is required"
  )
})

test_that("DelphiRF validates genuine-event metadata", {
  df <- data.frame(
    report_date = as.Date("2024-01-01"),
    target_date = as.Date("2024-01-02"),
    lag = 1L,
    genuine_event = 1L
  )
  expect_error(
    DelphiRF(df, testing_start_date = as.Date("2024-01-02")),
    "genuine_event must be a logical column"
  )
})

test_that("DelphiRF genuine-event switches independently filter training and testing", {
  cutoff <- as.Date("2024-02-01")
  n_train <- 20L
  n_test <- 4L
  df <- data.frame(
    reference_date = seq(cutoff - n_train, by = "day", length.out = n_train + n_test),
    report_date = c(seq(cutoff - n_train, cutoff - 1L, by = "day"),
                    seq(cutoff, by = "day", length.out = n_test)),
    target_date = rep(cutoff, n_train + n_test),
    lag = rep(8L, n_train + n_test),
    genuine_event = rep(c(TRUE, FALSE), length.out = n_train + n_test),
    value_7dav = seq_len(n_train + n_test),
    value_7dav_lag7 = seq_len(n_train + n_test),
    log_value_7dav = log1p(seq_len(n_train + n_test)),
    log_value_7dav_lag7 = log1p(seq_len(n_train + n_test))
  )

  local_mocked_bindings(
    revision_forecast = function(train_data, test_data, ...) {
      test_data$n_training_rows <- nrow(train_data)
      test_data$all_training_rows_genuine <- all(train_data$genuine_event)
      test_data
    },
    .package = "DelphiRF"
  )

  genuine <- DelphiRF(
    df, testing_start_date = cutoff, test_lag_groups = 8,
    lag_pad = 0, training_days = 30
  )
  expect_equal(nrow(genuine), 2L)
  expect_true(all(genuine$genuine_event))
  expect_equal(unique(genuine$n_training_rows), 10L)
  expect_true(all(genuine$all_training_rows_genuine))

  all_training <- DelphiRF(
    df, testing_start_date = cutoff, test_lag_groups = 8,
    lag_pad = 0, training_days = 30,
    genuine_training = FALSE, genuine_testing = TRUE
  )
  expect_equal(nrow(all_training), 2L)
  expect_equal(unique(all_training$n_training_rows), 20L)

  all_testing <- DelphiRF(
    df, testing_start_date = cutoff, test_lag_groups = 8,
    lag_pad = 0, training_days = 30,
    genuine_training = TRUE, genuine_testing = FALSE
  )
  expect_equal(nrow(all_testing), 4L)
  expect_equal(unique(all_testing$n_training_rows), 10L)

  all_rows <- DelphiRF(
    df, testing_start_date = cutoff, test_lag_groups = 8,
    lag_pad = 0, training_days = 30,
    genuine_training = FALSE, genuine_testing = FALSE
  )
  expect_equal(nrow(all_rows), 4L)
  expect_equal(unique(all_rows$n_training_rows), 20L)
})

test_that("preprocessing metadata controls high-level training and testing", {
  raw <- do.call(rbind, lapply(
    seq(as.Date("2024-01-01"), as.Date("2024-01-20"), by = "day"),
    function(reference_date) {
      level <- as.numeric(reference_date - as.Date("2023-12-01"))
      data.frame(
        ref_date = reference_date,
        lag = c(0L, 2L),
        value = c(level, level + 1)
      )
    }
  ))
  processed <- data_preprocessing(
    raw,
    value_col = "value", refd_col = "ref_date", lag_col = "lag",
    ref_lag = 3L, lagged_term_list = c(1L, 7L), smoothed = TRUE
  )
  cutoff <- as.Date("2024-01-18")

  local_mocked_bindings(
    revision_forecast = function(train_data, test_data, ...) {
      test_data$n_training_rows <- nrow(train_data)
      test_data$all_training_rows_genuine <- all(train_data$genuine_event)
      test_data
    },
    .package = "DelphiRF"
  )

  genuine <- DelphiRF(
    processed, testing_start_date = cutoff, test_lag_groups = "0-2",
    lag_pad = 0, training_days = 30
  )
  unfiltered <- DelphiRF(
    processed, testing_start_date = cutoff, test_lag_groups = "0-2",
    lag_pad = 0, training_days = 30,
    genuine_training = FALSE, genuine_testing = FALSE
  )

  expect_gt(nrow(genuine), 0L)
  expect_true(all(genuine$genuine_event))
  expect_true(all(genuine$all_training_rows_genuine))
  expect_gt(nrow(unfiltered), nrow(genuine))
  expect_gt(unique(unfiltered$n_training_rows),
            unique(genuine$n_training_rows))
})

test_that("cross-validation applies genuine-event switches before fitting", {
  df <- data.frame(
    reference_date = as.Date("2024-01-01") + 0:39,
    report_date = as.Date("2024-01-02") + 0:39,
    target_date = as.Date("2024-02-01"),
    lag = 8L,
    genuine_event = rep(c(TRUE, FALSE), 20),
    value_7dav = 1:40,
    value_7dav_lag7 = 1:40,
    log_value_7dav = log1p(1:40),
    log_value_7dav_lag7 = log1p(1:40)
  )
  seen <- new.env(parent = emptyenv())
  seen$training <- list()
  seen$testing <- list()

  local_mocked_bindings(
    revision_forecast = function(train_data, test_data, ...) {
      seen$training[[length(seen$training) + 1L]] <- train_data$genuine_event
      seen$testing[[length(seen$testing) + 1L]] <- test_data$genuine_event
      data.frame(wis = 0)
    },
    .package = "DelphiRF"
  )

  cv_revision_forecast(
    df, test_lag = 8L, taus = 0.5,
    lambda_candidates = 0.1, gamma_candidates = 0.1,
    lag_pad_candidates = 0, n_folds = 2,
    genuine_training = FALSE, genuine_testing = TRUE
  )

  expect_true(any(vapply(seen$training, function(x) any(!x), logical(1))))
  expect_true(all(vapply(seen$testing, all, logical(1))))
})
