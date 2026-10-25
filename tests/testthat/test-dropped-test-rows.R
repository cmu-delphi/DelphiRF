library(testthat)

# Test rows without every predictor are not forecast; they must be reported,
# with the reason, rather than lost silently.

make_prepared <- function() {
  set.seed(5)
  references <- as.Date("2023-01-07") + 0:99
  archive <- expand.grid(ref = references, lag = 0:15)
  level <- 200 + 50 * sin(seq_along(references) / 9)
  archive$value <- round(level[match(archive$ref, references)] *
                           (1 - 0.5 * exp(-archive$lag / 4)) + stats::rnorm(nrow(archive), sd = 3))
  data_preprocessing(archive[order(archive$ref, archive$lag), ], "value", "ref", "lag", 10,
                     temporal_resol = "daily", smoothed = FALSE)
}

test_that("incomplete_test_rows lists each left-out row with its missing features", {
  test <- data.frame(reference_date = as.Date("2023-03-01") + 0:3, report_date = as.Date("2023-03-05"),
                     lag = 4:1, a_lag1 = c(1, NA, 3, NA), b = c(1, 2, NA, NA))
  out <- incomplete_test_rows(test, c("a_lag1", "b"))
  expect_equal(out$reference_date, as.Date("2023-03-01") + 1:3)
  expect_equal(out$missing_features, c("a_lag1", "b", "a_lag1;b"))
  expect_equal(out$reason, c("missing lagged feature", "missing predictor", "missing predictor"))
  expect_equal(nrow(incomplete_test_rows(test[1, ], c("a_lag1", "b"))), 0)
})

test_that("revision_forecast reports the test rows it cannot forecast", {
  df <- make_prepared()
  end <- as.Date("2023-03-28")
  df <- add_weights_related(df, create_weight_anchors(df, end))
  train <- df[df$lag == 3, ]
  test <- df[df$lag == 3 & df$report_date >= end, ]
  test$log_value_7dav_lag7[1:2] <- NA
  cache <- tempfile("dropped-"); on.exit(unlink(cache, recursive = TRUE))
  expect_message(
    result <- revision_forecast(
      train, test, taus = c(0.1, 0.5, 0.9), working_scale = "log_value",
      params_list = c("log_value_7dav", "log_value_7dav_lag7"),
      training_end_date = as.character(end), training_days = 60,
      test_lag_group = "3", model_save_dir = cache),
    "2 test row\\(s\\) not forecast")
  dropped <- attr(result, "dropped_test_rows")
  expect_equal(nrow(dropped), 2)
  expect_equal(dropped$reference_date, test$reference_date[1:2])
  expect_equal(unique(dropped$reason), "missing lagged feature")
  # Every test row is either forecast or reported.
  expect_equal(nrow(result) + nrow(dropped), nrow(test))
})

test_that("preprocessed nowcasting gathers dropped rows across lag groups", {
  df <- make_prepared()
  origin <- as.Date("2023-03-28")
  df$log_value_7dav_lag7[df$report_date == origin & df$lag %in% c(1, 5)] <- NA
  cache <- tempfile("dropped-d-"); on.exit(unlink(cache, recursive = TRUE))
  result <- suppressMessages(delphirf_nowcast_preprocessed(
    df, testing_start_date = origin, working_scale = "log_value", temporal_resol = "daily",
    test_lag_groups = c("1", "5"), training_days = 60, taus = c(0.1, 0.5, 0.9),
    params_list = c("log_value_7dav", "log_value_7dav_lag7"), model_save_dir = cache))
  dropped <- attr(result, "dropped_test_rows")
  expect_setequal(dropped$test_lag_group, c("1", "5"))
  expect_true(all(dropped$report_date == origin))
  expect_false(any(paste(result$reference_date, result$report_date) %in%
                     paste(dropped$reference_date, dropped$report_date)))
})
