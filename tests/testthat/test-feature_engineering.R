library(testthat)

library(tidyr)
library(dplyr)
library(stringr)

test_that("add_dayofweek correctly adds one-hot encoding for weekdays", {
  # Sample data frame
  df <- data.frame(
    date = as.Date(c("2024-02-26", "2024-02-27", "2024-02-28", "2024-02-29", "2024-03-01", "2024-03-02", "2024-03-03"))  # Mon-Sun
  )

  # Apply the function
  result <- add_dayofweek(df, time_col = "date", suffix = "_test")

  # Check that new columns exist
  expect_true(all(paste0(c("Mon", "Tue", "Wed", "Thurs", "Fri", "Sat", "Sun"), "_test") %in% colnames(result)))

  # Verify one-hot encoding correctness
  expect_equal(result$Mon_test, c(1, 0, 0, 0, 0, 0, 0))
  expect_equal(result$Tue_test, c(0, 1, 0, 0, 0, 0, 0))
  expect_equal(result$Wed_test, c(0, 0, 1, 0, 0, 0, 0))
  expect_equal(result$Thurs_test, c(0, 0, 0, 1, 0, 0, 0))
  expect_equal(result$Fri_test, c(0, 0, 0, 0, 1, 0, 0))
  expect_equal(result$Sat_test, c(0, 0, 0, 0, 0, 1, 0))
  expect_equal(result$Sun_test, c(0, 0, 0, 0, 0, 0, 1))
})



test_that("testing adding columns for each week of a month", {

  # Create a test data frame
  test_df <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-08", "2024-01-15", "2024-01-22", "2024-01-29", "2024-02-05"))
  )

  # Expected week labels
  week_labels <- c("W1", "W2", "W3", "W4")

  # Apply the function
  result_df <- add_weekofmonth(test_df, "date", week_labels)

  # Check that new columns are added
  expect_equal(ncol(result_df), ncol(test_df) + length(week_labels))

  # Check that each row has exactly one '1' in the week columns
  expect_true(all(rowSums(result_df[, week_labels]) == 1))

  # Check that known dates fall into the expected weeks
  expect_equal(result_df[result_df$date == as.Date("2024-01-01"), "W1"], 1)
  expect_equal(result_df[result_df$date == as.Date("2024-01-08"), "W2"], 1)
  expect_equal(result_df[result_df$date == as.Date("2024-01-15"), "W3"], 1)
  expect_equal(result_df[result_df$date == as.Date("2024-01-22"), "W4"], 1)

  # Check that 5th and 6th week dates correctly map to W1
  expect_equal(result_df[result_df$date == as.Date("2024-01-29"), "W1"], 1)
  expect_equal(result_df[result_df$date == as.Date("2024-02-05"), "W2"], 1)
})

test_that("add_7davs correctly computes report_date and 7dav", {
  # Create a sample dataset
  test_df <- data.frame(ref_date = c(as.Date("2022-01-03"), as.Date("2022-01-03"),
                                     as.Date("2022-01-03"), as.Date("2022-01-03"),
                                     as.Date("2022-01-04"), as.Date("2022-01-04"),
                                     as.Date("2022-01-04"), as.Date("2022-01-07"),
                                     as.Date("2022-01-07"), as.Date("2022-01-09"),
                                     as.Date("2022-01-09"),as.Date("2022-01-10"),
                                     as.Date("2022-01-10"),as.Date("2022-01-11")
  ),
  lag = c(0, 3, 5, 8, 0, 2, 7, 1, 3, 0, 2, 0, 1, 0),
  value_raw=c(100, 200, 500, 1000, 0, 200, 220, 50, 300, 20, 50, 60, 120, 30))
  test_df <- fill_missing_updates(test_df, value_col = "value_raw", refd_col = "ref_date",
                                  lag_col = "lag", temporal_resol = "daily")

  # Apply function
  result_df <- add_7davs(test_df, value_col = "value_raw", refd_col = "reference_date", lag_col = "lag")

  # Check if 'value_7dav' is computed correctly
  computed <- result_df$value_7dav[(result_df$report_date == as.Date("2022-01-11"))
                                   & (result_df$ref_date == as.Date("2022-01-11"))]
  expected <- (30 + 120 + 50 + 300 ) / 7
  expect_equal(computed, expected)

  computed <- result_df$value_7dav[(result_df$report_date == as.Date("2022-01-11"))
                                   & (result_df$ref_date == as.Date("2022-01-10"))]
  expected <- (120 + 50 + 300 + 220) / 7
  expect_equal(computed, expected)

  computed <- result_df$value_7dav[(result_df$report_date == as.Date("2022-01-11"))
                                   & (result_df$ref_date == as.Date("2022-01-09"))]
  expected <- (50 + 300 + 220 + 1000) / 7
  expect_equal(computed, expected)

  # Check if the output retains all original columns + new ones
  expect_true(all(c("report_date", "value_7dav") %in% names(result_df)))

  # Expect NA values for first 6 rows
  expect_true(all(is.na(result_df$value_7dav[1:6])))

})



test_that("add_lagged_terms correctly adds lagged columns for daily data", {
  # Create sample data
  df <- data.frame(
    ref_date = c(as.Date("2024-01-01"), as.Date("2024-01-01"), as.Date("2024-01-01"),
                 as.Date("2024-01-02"), as.Date("2024-01-02"),
                 as.Date("2024-01-03")
    ),  # 10 consecutive days
    lag = c(0, 1, 2, 0, 1, 0),
    value = 10:15
  )

  # Run function with daily resolution
  df_lagged <- add_lagged_terms(df, value_col = "value", refd_col = "ref_date",
                                lag_col = "lag", lagged_term_list = c(1, 2), temporal_resol = "daily")

  # Check that new lagged columns exist
  expect_true(all(c("value_lag1", "value_lag2") %in% colnames(df_lagged)))
  expect_true(all(c("log_delta_value_lag1", "log_delta_value_lag2") %in% colnames(df_lagged)))

  # Check if lagged values are correctly assigned
  expect_equal(df_lagged$value_lag1[5], df_lagged$value_lag2[6])
})



test_that("create_target_lookup fixes maturity at the requested target lag", {
  raw <- data.frame(
    ref_date = as.Date("2024-01-01"),
    lag = c(50, 59, 61, 62, 70),
    value = c(10, 10, 12, 15, 20)
  )
  target <- create_target_lookup(raw, "value", "ref_date", "lag", 60)
  expect_equal(target$target_lag, 60)
  expect_equal(target$target_date, as.Date("2024-03-01"))
  expect_equal(target$target_source_lag, 59)
  expect_equal(target$target_value_source, "carried_forward")
})

test_that("create_target_lookup records an exact release separately from maturity", {
  raw <- data.frame(
    ref_date = as.Date("2024-01-01"),
    lag = c(50, 59, 60, 62),
    value = c(10, 10, 12, 15)
  )
  target <- create_target_lookup(raw, "value", "ref_date", "lag", 60)
  expect_equal(target$target_lag, 60)
  expect_equal(target$target_source_lag, 60)
  expect_equal(target$target_value_source, "exact_release")
})

test_that("create_target_lookup does not use reports after the target lag", {
  raw <- data.frame(
    ref_date = as.Date("2024-01-01"),
    lag = c(50, 59, 61, 62, 70),
    value = c(10, 10, 12, 15, 20)
  )
  target <- create_target_lookup(raw, "value", "ref_date", "lag", 60)
  expect_equal(target$target_source_lag, 59)
  expect_equal(target$target_value_source, "carried_forward")
})

test_that("omitting target_as_of_date preserves the original target behavior", {
  raw <- data.frame(
    ref_date = rep(as.Date("2024-01-01"), 4),
    lag = c(0, 5, 7, 9),
    value = c(10, 11, 12, 13)
  )

  legacy_call <- create_target_lookup(
    raw, "value", "ref_date", "lag", 7, temporal_resol = "daily"
  )
  explicit_default <- create_target_lookup(
    raw, "value", "ref_date", "lag", 7, temporal_resol = "daily",
    target_as_of_date = NULL
  )

  expect_identical(legacy_call, explicit_default)
})

test_that("as_of_date withholds targets until requested maturity", {
  raw <- data.frame(
    ref_date = as.Date("2024-01-01"),
    lag = c(40, 50, 59, 70),
    value = c(8, 10, 10, 20)
  )
  immature <- create_target_lookup(
    raw, "value", "ref_date", "lag", 60, temporal_resol = "daily",
    target_as_of_date = as.Date("2024-02-20")
  )
  expect_equal(nrow(immature), 1)
  expect_equal(immature$target_date, as.Date("2024-03-01"))
  expect_equal(immature$target_lag, 60)
  expect_true(is.na(immature$target_source_lag))
  expect_true(is.na(immature$target_value_source))

  mature <- create_target_lookup(
    raw, "value", "ref_date", "lag", 60, temporal_resol = "daily",
    target_as_of_date = as.Date("2024-03-01")
  )
  expect_equal(mature$target_lag, 60)
  expect_equal(mature$target_source_lag, 59)
  expect_equal(mature$target_value_source, "carried_forward")
})

test_that("target cutoff retains maturity dates but withholds source metadata", {
  raw <- data.frame(
    ref_date = as.Date("2024-01-10"),
    lag = c(0, 1, 2),
    value = c(10, 12, 15)
  )
  target <- create_target_lookup(
    raw, "value", "ref_date", "lag", 1, temporal_resol = "daily",
    target_as_of_date = as.Date("2024-01-09")
  )
  expect_equal(nrow(target), 1)
  expect_equal(target$target_date, as.Date("2024-01-11"))
  expect_equal(target$target_lag, 1)
  expect_true(is.na(target$target_source_lag))
  expect_true(is.na(target$target_value_source))
})

test_that("data_preprocessing never attaches a target after its as-of date", {
  raw <- data.frame(
    ref_date = rep(as.Date(c("2024-01-01", "2024-01-02")), each = 6),
    lag = rep(0:5, 2),
    value = c(10, 10, 11, 12, 13, 14, 20, 20, 21, 22, 23, 24)
  )
  cutoff <- as.Date("2024-01-04")
  result <- data_preprocessing(
    raw, "value", "ref_date", "lag", 3,
    lagged_term_list = c(1), smoothed = FALSE,
    target_as_of_date = cutoff
  )
  targets <- result %>%
    distinct(reference_date, target_date, target_lag,
             target_source_lag, target_value_source) %>%
    filter(!is.na(target_date))

  expect_equal(nrow(targets), 2)
  expect_equal(targets$target_date,
               as.Date(c("2024-01-04", "2024-01-05")))
  expect_equal(targets$target_lag, c(3, 3))
  expect_equal(targets$target_source_lag, c(3, NA))
  expect_equal(targets$target_value_source, c("exact_release", NA))
  immature <- result$target_date > cutoff
  expect_true(all(is.na(result$value_target[immature])))
})

test_that("create_target_lookup never selects negative reporting lags", {
  raw <- data.frame(
    ref_date = as.Date("2024-01-10"),
    lag = c(-7, 5),
    value = c(5, 8)
  )
  target <- create_target_lookup(
    raw, "value", "ref_date", "lag", 7,
    target_as_of_date = as.Date("2024-01-17")
  )
  expect_equal(target$target_lag, 7)
  expect_equal(target$target_source_lag, 5)
  expect_equal(target$target_value_source, "carried_forward")
})

test_that("create_target_lookup quietly returns a typed empty lookup for invalid lags", {
  expected <- data.frame(
    reference_date = as.Date(character()),
    target_date = as.Date(character()),
    target_lag = numeric(),
    target_source_lag = numeric(),
    target_value_source = character()
  )
  negative_only <- data.frame(
    ref_date = as.Date(c("2024-01-10", "2024-01-11")),
    lag = c(-7, -1),
    value = c(5, 8)
  )
  all_missing <- data.frame(
    ref_date = as.Date(c("2024-01-10", "2024-01-11")),
    lag = c(NA_real_, NA_real_),
    value = c(5, 8)
  )

  expect_no_warning(
    negative_result <- create_target_lookup(
      negative_only, "value", "ref_date", "lag", 7
    )
  )
  expect_no_warning(
    missing_result <- create_target_lookup(
      all_missing, "value", "ref_date", "lag", 7
    )
  )
  expect_identical(negative_result, expected)
  expect_identical(missing_result, expected)
})

test_that("create_target_lookup ignores reports without usable values", {
  raw <- data.frame(
    ref_date = as.Date("2024-01-01"),
    lag = c(0, 1, 2),
    value = c(10, NA, 12)
  )
  target <- create_target_lookup(raw, "value", "ref_date", "lag", 1,
                                 target_as_of_date = as.Date("2024-01-02"))
  expect_equal(target$target_source_lag, 0)
  expect_equal(target$target_value_source, "carried_forward")

  raw$value[] <- NA_real_
  expect_identical(
    create_target_lookup(raw, "value", "ref_date", "lag", 1),
    data.frame(
      reference_date = as.Date(character()),
      target_date = as.Date(character()),
      target_lag = numeric(),
      target_source_lag = numeric(),
      target_value_source = character()
    )
  )
})

test_that("preprocessing excludes empty count episodes and labels missing updates false", {
  raw <- data.frame(
    ref_date = rep(as.Date(c("2024-01-01", "2024-01-02")), each = 3),
    lag = rep(0:2, 2),
    value = c(10, NA, 10, NA, NA, NA)
  )
  result <- data_preprocessing(
    raw, "value", "ref_date", "lag", ref_lag = 3,
    lagged_term_list = c(1, 7), smoothed = TRUE,
    target_as_of_date = as.Date("2024-01-04")
  )

  expect_setequal(unique(result$reference_date), as.Date("2024-01-01"))
  expect_equal(result$genuine_event[match(0:2, result$lag)], c(TRUE, FALSE, TRUE))
  expect_true(all(result$value_target == 10))
  expect_true(all(result$target_source_lag == 2))
})

test_that("fraction observations require both numerator and denominator", {
  raw <- data.frame(
    ref_date = rep(as.Date(c("2024-01-01", "2024-01-02")), each = 3),
    lag = rep(0:2, 2),
    numerator = c(1, 2, 2, 1, 2, 3),
    denominator = c(10, NA, 20, NA, NA, NA)
  )
  result <- data_preprocessing(
    raw, c("numerator", "denominator"), "ref_date", "lag", ref_lag = 3,
    suffixes = c("_num", "_denom"), lagged_term_list = c(1, 7),
    value_type = "fraction", smoothed = TRUE,
    target_as_of_date = as.Date("2024-01-04")
  )

  expect_setequal(unique(result$reference_date), as.Date("2024-01-01"))
  expect_equal(result$genuine_event[match(0:2, result$lag)], c(TRUE, FALSE, TRUE))
  expect_true(all(result$target_source_lag == 2))
  expect_equal(unique(result$value_target_num), 2)
  expect_equal(unique(result$value_target_denom), 20)
  expect_equal(unique(result$value_target), 3 / 21)
})

test_that("weekly raw targets retain Friday and keep day-based lags", {
  raw <- data.frame(
    ref_date = as.Date("2024-01-06"),
    lag = c(60, 62), # Wednesday and Friday in the same epiweek
    value = c(10, 14)
  )
  target <- create_target_lookup(
    raw, "value", "ref_date", "lag", 63, temporal_resol = "weekly"
  )
  expect_equal(target$target_lag, 63)
  expect_equal(target$target_date, as.Date("2024-03-09"))
  expect_equal(target$target_source_lag, 63)
  expect_equal(target$target_value_source, "exact_release")
})

test_that("create_target_lookup dates targets even when no source lag is eligible", {
  # Simulate bulk-loaded historical data: every reference_date has only one
  # observation with lag >> ref_lag (e.g. loaded months after the fact).
  # The requested maturity date remains authoritative even when there was no
  # raw report at or before that lag. Source audit metadata is unavailable.
  raw <- data.frame(
    ref_date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    lag = c(250, 249, 248),  # all >> ref_lag=60; no fallback <= 60 exists
    value = c(10, 12, 11)
  )
  result <- create_target_lookup(raw, "value", "ref_date", "lag", 60)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3L)
  expect_true("reference_date" %in% colnames(result))
  expect_true("target_date"    %in% colnames(result))
  expect_true("target_lag"     %in% colnames(result))
  expect_true("target_source_lag" %in% colnames(result))
  expect_true("target_value_source" %in% colnames(result))
  expect_true(all(result$target_lag == 60))
  expect_true(all(is.na(result$target_source_lag)))
  expect_true(all(is.na(result$target_value_source)))
})

test_that("data_preprocessing attaches raw-aware target after filling", {
  raw <- data.frame(
    ref_date = rep(as.Date("2024-01-01"), 4),
    lag = c(50, 59, 61, 62),
    value = c(10, 10, 12, 15)
  )
  result <- data_preprocessing(
    raw, "value", "ref_date", "lag", 60,
    lagged_term_list = c(1, 7), temporal_resol = "daily", smoothed = TRUE
  )
  expect_true(all(result$target_lag == 60))
  expect_true(all(result$target_source_lag == 59))
  expect_true(all(result$target_value_source == "carried_forward"))
  expect_true(all(result$value_target == 10))
})

test_that("an explicit as_of_date carries a quiet episode through target maturity", {
  raw <- data.frame(
    ref_date = rep(as.Date("2024-01-01"), 2),
    lag = c(2, 5),
    value = c(10, 12)
  )
  result <- data_preprocessing(
    raw, "value", "ref_date", "lag", 7,
    lagged_term_list = c(1, 7), temporal_resol = "daily", smoothed = TRUE,
    target_as_of_date = as.Date("2024-01-10")
  )

  expect_true(all(result$target_lag == 7))
  expect_true(all(result$target_date == as.Date("2024-01-08")))
  expect_true(all(result$target_source_lag == 5))
  expect_true(all(result$target_value_source == "carried_forward"))
  expect_true(all(result$value_target == 12))
})

test_that("data_preprocessing uses denominator-only revisions for fraction targets", {
  raw <- data.frame(
    ref_date = as.Date("2024-01-01"),
    lag = c(50, 59, 61, 62),
    numerator = c(10, 10, 10, 10),
    denominator = c(100, 100, 200, 200)
  )

  result <- data_preprocessing(
    raw, c("numerator", "denominator"), "ref_date", "lag", 60,
    suffixes = c("_num", "_denom"), lagged_term_list = c(1, 7),
    value_type = "fraction", temporal_resol = "daily", smoothed = TRUE
  )

  expect_true(all(result$target_lag == 60))
  expect_true(all(result$target_date == as.Date("2024-03-01")))
  expect_true(all(result$target_source_lag == 59))
  expect_true(all(result$target_value_source == "carried_forward"))
  expect_equal(unique(result$log_value_target), log(11) - log(101))
})


test_that("add_log_transformed correctly applies log transformation", {
  # Create a test dataframe
  test_df <- data.frame(
    value_raw = c(0, 5, 10, 20),
    value_7dav = c(1, 3, 6, 12),
    value_target = c(2, 4, 8, 16),
    value_target_7dav = c(1, 2, 4, 8),
    value_7dav_lag1 = c(0, 2, 4, 6),
    value_7dav_lag2 = c(1, 1, 3, 5)
  )

  # Apply function
  transformed_df <- add_log_transformed(test_df, lagged_term_list = c(1, 2))

  # Check if new log-transformed columns are created
  expected_cols <- c("log_value_raw", "log_value_7dav", "log_value_target",
                     "log_value_target_7dav", "log_value_7dav_lag1", "log_value_7dav_lag2")

  expect_true(all(expected_cols %in% colnames(transformed_df)))

  # Check log transformation correctness
  expect_equal(transformed_df$log_value_raw, log(test_df$value_raw + 1))
  expect_equal(transformed_df$log_value_7dav, log(test_df$value_7dav + 1))
  expect_equal(transformed_df$log_value_target, log(test_df$value_target + 1))
  expect_equal(transformed_df$log_value_target_7dav, log(test_df$value_target_7dav + 1))
  expect_equal(transformed_df$log_value_7dav_lag1, log(test_df$value_7dav_lag1 + 1))
  expect_equal(transformed_df$log_value_7dav_lag2, log(test_df$value_7dav_lag2 + 1))
})


test_that("add_params_for_dates keeps all weekday indicators in daily mode", {
  test_df <- data.frame(
    ref_date = seq(as.Date("2024-02-26"), by = "day", length.out = 7L),
    lag = rep(0L, 7L)
  )
  df_with_params <- add_params_for_dates(test_df, "ref_date", "lag", "daily")

  ref_columns <- paste0(WEEKDAYS_ABBR, "_ref")
  issue_columns <- paste0(WEEKDAYS_ABBR, "_issue")
  expect_true(all(ref_columns %in% names(df_with_params)))
  expect_true(all(issue_columns %in% names(df_with_params)))
  expect_true(all(rowSums(df_with_params[, ref_columns]) == 1L))
  expect_true(all(rowSums(df_with_params[, issue_columns]) == 1L))
  expect_true(all(WEEK_ISSUES %in% colnames(df_with_params)))
  expect_true(all(rowSums(df_with_params[, WEEK_ISSUES]) <= 1))
})

test_that("create_params_list uses six weekday indicators and omits Sunday", {
  test_df <- data.frame(
    ref_date = seq(as.Date("2024-02-26"), by = "day", length.out = 14L),
    lag = rep(0:1, 7L)
  )
  result <- add_params_for_dates(test_df, "ref_date", "lag", "daily")
  params <- create_params_list(
    result, lagged_term_list = c(1L, 7L), temporal_resol = "daily"
  )

  modeled_weekdays <- setdiff(WEEKDAYS_ABBR, "Sun")
  expect_true(all(paste0(modeled_weekdays, "_ref") %in% params))
  expect_true(all(paste0(modeled_weekdays, "_issue") %in% params))
  expect_false("Sun_ref" %in% params)
  expect_false("Sun_issue" %in% params)
  expect_true(all(c("Sun_ref", "Sun_issue") %in% names(result)))
})

test_that("add_params_for_dates correctly handles weekly resolution", {
  test_df <- data.frame(
    ref_date = as.Date(c("2022-03-01", "2022-03-08", "2022-03-15")),
    lag = c(0, 7, 14)
  )
  df_with_params <- add_params_for_dates(test_df, "ref_date", "lag", "weekly")

  expect_true("report_date" %in% colnames(df_with_params))
  expect_false(any(paste0(WEEKDAYS_ABBR, "_ref") %in% names(df_with_params)))
  expect_false(any(paste0(WEEKDAYS_ABBR, "_issue") %in% names(df_with_params)))
  expect_true(all(WEEK_ISSUES %in% colnames(df_with_params)))
  expect_true(all(rowSums(df_with_params[, WEEK_ISSUES]) <= 1))
})

test_that("data_preprocessing handles multiple value columns correctly", {
  df <- data.frame(
    ref_date = as.Date("2023-01-01") + 0:9,  # 10 days of reference dates
    cases = c(10, 20, 15, 30, 25, 5, 40, 50, 35, 45),
    deaths = c(1, 2, 2, 1, 2, 0, 0, 0, 1, 2),
    extra = 0:9,
    lag = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  )

  expect_warning(
    data_preprocessing(df, value_col = c("cases", "deaths"), refd_col = "ref_date",
                       lag_col = "lag", ref_lag = 7, value_type = "count"),
    "Multiple value column names provided; only the first one will be used."
    )
  expect_warning(
    data_preprocessing(df, value_col = c("cases", "deaths", "extra"),
                       suffixes=c("_1", "_2"), refd_col = "ref_date", lag_col = "lag", ref_lag = 7, value_type = "fraction"),
    "More than two value column names provided; only the first two will be used as numerator and denominator."
    )
  expect_warning(
    data_preprocessing(df, value_col = c("cases", "deaths"),  suffixes=c("_1", "_1"),
                       refd_col = "ref_date", lag_col = "lag", ref_lag = 7, value_type = "fraction"),
    "Insufficient suffixes provided for numerator and denominator; default suffixes '_num' and '_denom' will be used."
  )
  # One column with value_type = "fraction" is a directly supplied rate.
  expect_no_warning(
    data_preprocessing(df, value_col = c("cases"), refd_col = "ref_date",
                       lag_col = "lag", ref_lag = 7, value_type = "fraction")
  )
  expect_warning(
    data_preprocessing(df, value_col = c("cases", "deaths"), refd_col = "ref_date",
                       lag_col = "lag", ref_lag = 7, value_type = "count"),
    "Multiple value column names provided; only the first one will be used."
  )

  # For count data
  result_df <- data_preprocessing(df, value_col = c("cases"), refd_col = "ref_date", lag_col = "lag",
                                  ref_lag = 7, value_type = "count")
  expect_true("value_7dav" %in% colnames(result_df))
  expect_true("value_target" %in% colnames(result_df))
  expect_true("value_target_7dav" %in% colnames(result_df))
  expect_true("log_value_7dav" %in% colnames(result_df))

  # For fraction data
  result_df <- data_preprocessing(df, value_col = c("cases", "deaths"), suffixes=c("_num", "_denom"),
                                  refd_col = "ref_date", lag_col = "lag", ref_lag = 7, value_type = "fraction")
  expect_s3_class(result_df, "tbl_df")
  expect_true("value_7dav" %in% colnames(result_df))
  expect_true("value_7dav_num" %in% colnames(result_df))
  expect_true("value_7dav_denom" %in% colnames(result_df))
  expect_true("value_target" %in% colnames(result_df))
  expect_true("value_target_7dav" %in% colnames(result_df))
  expect_true("log_value_7dav" %in% colnames(result_df))

  expect_true(all(paste0(WEEKDAYS_ABBR, "_issue") %in% names(result_df)))
  expect_true(all(paste0(WEEKDAYS_ABBR, "_ref") %in% names(result_df)))

  expect_true("log_value_7dav_lag1" %in% colnames(result_df))
  expect_true("log_value_7dav_lag7" %in% colnames(result_df))

  expect_true("log_delta_value_7dav_lag7" %in% colnames(result_df))
  expect_true("log_delta_value_7dav_lag1" %in% colnames(result_df))

  weekly_result <- data_preprocessing(
    df, value_col = c("cases", "deaths"), suffixes = c("_num", "_denom"),
    refd_col = "ref_date", lag_col = "lag", ref_lag = 7,
    value_type = "fraction", temporal_resol = "weekly"
  )
  expect_true(all(weekdays(weekly_result$reference_date) == "Saturday"))
  expect_true(all(weekly_result$lag %% 7 == 0))

  expect_true(max(result_df$lag) < 7)
  expect_true("reference_date" %in% colnames(result_df))
  expect_true("lag" %in% colnames(result_df))

})

test_that("Testing add weighted related features", {
  test_df <- data.frame(
    report_date = as.Date(c("2024-12-25", "2024-12-26")),
    value_7dav = c(100, 200),
    value_7dav_lag7 = c(90, 190),
    log_value_7dav = log(c(100, 200) + 1),
    log_value_7dav_lag7 = log(c(90, 190) + 1),
    lag = c(2, 2)
  )

  anchors <- create_weight_anchors(test_df, as.Date("2024-12-26"))

  # Function runs without errors
  expect_silent(result <- add_weights_related(test_df, anchors))

  # Missing required columns throws error
  incomplete_df <- test_df %>% select(-log_value_7dav_lag7)
  expect_error(add_weights_related(incomplete_df, anchors),
               "Missing required columns when adding weighted related features")

  # Check that correct columns are present in output
  expected_columns <- c("report_date", "value_7dav", "value_7dav_lag7", "log_value_7dav", "log_value_7dav_lag7", "lag",
                        "log_7dav_slope", "ref_slope_value", "ref_7dav_value",
                        "value_slope_diff", "value_7dav_diff")
  expect_true(all(expected_columns %in% colnames(result)))

})


test_that("data_preprocessing labels genuine events before forward filling", {
  raw <- data.frame(
    ref_date = rep(as.Date("2024-01-01"), 3),
    lag = c(0L, 2L, 3L),
    value = c(10, 10, 12)
  )

  result <- data_preprocessing(
    raw,
    value_col = "value", refd_col = "ref_date", lag_col = "lag",
    ref_lag = 4L, lagged_term_list = c(1L, 7L), smoothed = TRUE
  )
  result <- result[order(result$lag), ]

  expect_type(result$genuine_event, "logical")
  expect_equal(result$lag, 0:3)
  expect_equal(result$genuine_event, c(TRUE, FALSE, TRUE, TRUE))
})

test_that("add_lagged_terms measures revisions from an explicit start lag", {
  df <- expand.grid(ref_date = as.Date("2022-01-01") + 0:20, lag = 0:5)
  df$value <- ifelse(df$lag == 0, 0, 10 * as.numeric(df$ref_date - as.Date("2022-01-01")) + df$lag)
  from_grid <- add_lagged_terms(df, "value", "ref_date", "lag", 1, "daily")
  from_first_report <- add_lagged_terms(df, "value", "ref_date", "lag", 1, "daily", start_lag = 1)
  row <- from_first_report[from_first_report$ref_date == as.Date("2022-01-10") &
                             from_first_report$lag == 3, ]
  # Reference date 01-09 at lag 4 (same report date), measured from its lag-1 report.
  expect_equal(row$value_start_lag1, 10 * 8 + 1)
  expect_equal(row$log_delta_value_lag1, log(10 * 8 + 4 + 1) - log(10 * 8 + 1 + 1))
  # Without the start lag the lag-0 zero is the reference, as before.
  old <- from_grid[from_grid$ref_date == as.Date("2022-01-10") & from_grid$lag == 3, ]
  expect_equal(old$value_start_lag1, 0)
})

test_that("data_preprocessing measures revision magnitude from the first report (l_min)", {
  refs <- as.Date("2022-01-01") + 0:59
  raw <- do.call(rbind, lapply(seq_along(refs), function(k) {
    data.frame(reference_date = refs[k], lag = 1:20,       # first released at lag 1
               value = 100 + k + 3 * pmin(1:20, 8))
  }))
  out <- data_preprocessing(raw, value_col = "value", refd_col = "reference_date",
                            lag_col = "lag", ref_lag = 14, lagged_term_list = c(1, 7),
                            value_type = "count", temporal_resol = "daily", smoothed = TRUE)
  row <- out[out$reference_date == as.Date("2022-02-15") & out$lag == 3, ]
  # Reference date 02-08 (k = 39) at lag 10, measured from its lag-1 report.
  expect_equal(row$value_7dav_start_lag7, 100 + 39 + 3)
  expect_equal(row$log_delta_value_7dav_lag7, log(100 + 39 + 24 + 1) - log(100 + 39 + 3 + 1))
  # It is no longer a copy of the level feature.
  expect_false(isTRUE(all.equal(out$log_delta_value_7dav_lag7, out$log_value_7dav_lag7)))
  # An explicit start lag is honoured.
  explicit <- data_preprocessing(raw, value_col = "value", refd_col = "reference_date",
                                 lag_col = "lag", ref_lag = 14, lagged_term_list = c(1, 7),
                                 value_type = "count", temporal_resol = "daily", smoothed = TRUE,
                                 revision_start_lag = 0)
  expect_true(all(explicit$value_7dav_start_lag7[!is.na(explicit$value_7dav_start_lag7)] == 0))
})

test_that("revision magnitudes use each episode's own first genuine report", {
  refs <- as.Date("2022-01-01") + 0:59
  raw <- do.call(rbind, lapply(seq_along(refs), function(k) {
    first_lag <- if (k %% 2L) 1L else 4L
    lags <- first_lag:20L
    data.frame(reference_date = refs[k], lag = lags,
               value = 100 + k + 3 * pmin(lags - first_lag, 8L))
  }))
  out <- data_preprocessing(raw, value_col = "value", refd_col = "reference_date",
                            lag_col = "lag", ref_lag = 14,
                            lagged_term_list = c(1, 7), value_type = "count",
                            temporal_resol = "daily", smoothed = TRUE)

  # The lag-7 source episode for this row starts at lag 4. Its baseline must be
  # that genuine lag-4 value, not the grid-filled zero at the global minimum 1.
  row <- out[out$reference_date == refs[20] & out$lag == 8, ]
  source_k <- 13L
  expected_start <- 100 + source_k
  expect_equal(row$value_7dav_start_lag7, expected_start)
  expect_equal(row$log_delta_value_7dav_lag7,
               log(row$value_7dav_lag7 + 1) - log(expected_start + 1))
  expect_lt(row$log_delta_value_7dav_lag7, 1)
})

test_that("episode-specific revision starts support a mixture of lag zero and later starts", {
  refs <- as.Date("2023-01-01") + 0:39
  raw <- do.call(rbind, lapply(seq_along(refs), function(k) {
    first_lag <- if (k %% 2L) 0L else 3L
    lags <- first_lag:14L
    data.frame(reference_date = refs[k], lag = lags,
               value = 1000 + 100 * k + 2 * (lags - first_lag))
  }))
  out <- data_preprocessing(raw, value_col = "value", refd_col = "reference_date",
                            lag_col = "lag", ref_lag = 14,
                            lagged_term_list = c(1, 7), value_type = "count",
                            temporal_resol = "daily", smoothed = TRUE)

  # The lag-1 source episode is reference date k=19, whose first report is lag 0.
  row <- out[out$reference_date == refs[20] & out$lag == 5, ]
  expected_start <- 1000 + 100 * 19
  expect_equal(row$value_7dav_start_lag1, expected_start)
  expect_equal(row$log_delta_value_7dav_lag1,
               log(row$value_7dav_lag1 + 1) - log(expected_start + 1))

  # The next source episode is k=20, whose first genuine report is lag 3.
  later_row <- out[out$reference_date == refs[21] & out$lag == 5, ]
  expect_equal(later_row$value_7dav_start_lag1, 1000 + 100 * 20)
})

test_that("weekly revision magnitudes use each episode's normalized first report", {
  refs <- as.Date("2023-01-07") + 7 * (0:29)
  raw <- do.call(rbind, lapply(seq_along(refs), function(k) {
    first_lag <- if (k %% 2L) 0L else 7L
    lags <- seq(first_lag, 35L, by = 7L)
    data.frame(reference_date = refs[k], lag = lags,
               value = 2000 + 100 * k + 3 * ((lags - first_lag) / 7))
  }))
  out <- data_preprocessing(raw, value_col = "value", refd_col = "reference_date",
                            lag_col = "lag", ref_lag = 28,
                            lagged_term_list = c(7, 14), value_type = "count",
                            temporal_resol = "weekly", smoothed = TRUE)

  # The lag-7 source episode is reference date k=19, whose first report is lag 0.
  row <- out[out$reference_date == refs[20] & out$lag == 14, ]
  expected_start <- 2000 + 100 * 19
  expect_equal(row$value_7dav_start_lag7, expected_start)
  expect_equal(row$log_delta_value_7dav_lag7,
               log(row$value_7dav_lag7 + 1) - log(expected_start + 1))

  # The next source episode begins at lag 7 rather than the dataset minimum 0.
  later_row <- out[out$reference_date == refs[21] & out$lag == 14, ]
  expect_equal(later_row$value_7dav_start_lag7, 2000 + 100 * 20)
})

test_that("fraction revision magnitudes share the episode-specific start", {
  refs <- as.Date("2023-01-01") + 0:39
  raw <- do.call(rbind, lapply(seq_along(refs), function(k) {
    first_lag <- if (k %% 2L) 0L else 3L
    lags <- first_lag:14L
    data.frame(reference_date = refs[k], lag = lags,
               numerator = 1000 + 100 * k + 2 * (lags - first_lag),
               denominator = 10000 + 50 * k + lags)
  }))
  out <- data_preprocessing(
    raw, value_col = c("numerator", "denominator"),
    refd_col = "reference_date", lag_col = "lag", ref_lag = 14,
    suffixes = c("_num", "_den"), lagged_term_list = c(1, 7),
    value_type = "fraction", temporal_resol = "daily", smoothed = TRUE
  )

  row <- out[out$reference_date == refs[20] & out$lag == 5, ]
  expect_equal(row$value_7dav_start_lag1_num, 1000 + 100 * 19)
  expect_equal(row$value_7dav_start_lag1_den, 10000 + 50 * 19)
  expect_equal(row$log_delta_value_7dav_lag1,
               row$log_delta_value_7dav_lag1_num -
                 row$log_delta_value_7dav_lag1_den)
})

test_that("lag terms are validated before any work", {
  raw <- data.frame(ref = rep(as.Date("2024-01-06") + 7 * (0:5), each = 3),
                    lag = rep(c(0, 7, 14), 6), value = 1:18)
  expect_error(data_preprocessing(raw, "value", "ref", "lag", 28, temporal_resol = "weekly",
                                  smoothed = TRUE, lagged_term_list = c(1, 7)),
               "multiples of 7")
  expect_error(data_preprocessing(raw, "value", "ref", "lag", 28, temporal_resol = "daily",
                                  lagged_term_list = c(0.5, 7)),
               "positive whole numbers")
  expect_error(add_lagged_terms(raw, "value", "ref", "lag", lagged_term_list = -1,
                                temporal_resol = "daily"),
               "positive whole numbers")
})

test_that("a weekly target lag that is not whole weeks raises an alarm", {
  raw <- expand.grid(ref = as.Date("2024-01-06") + 7 * (0:20), k = 0:10)
  raw$lag <- raw$k * 7
  raw$value <- 100 + raw$k
  expect_warning(
    data_preprocessing(raw[c("ref", "lag", "value")], "value", "ref", "lag", 30,
                       temporal_resol = "weekly", smoothed = TRUE),
    "30 days is not a whole number of weeks")
  expect_no_warning(
    data_preprocessing(raw[c("ref", "lag", "value")], "value", "ref", "lag", 28,
                       temporal_resol = "weekly", smoothed = TRUE))
})
