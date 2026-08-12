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



test_that("add_lagged_terms correctly adds lagged columns for weekly data", {
  # Create sample data
  df <- data.frame(
    ref_date = c(as.Date("2024-01-01"), as.Date("2024-01-01"), as.Date("2024-01-01"),
                 as.Date("2024-01-02"), as.Date("2024-01-02"),
                 as.Date("2024-01-03")
    ),  # 10 consecutive days
    lag = c(0, 1, 2, 0, 1, 0),
    value = 10:15
  )

  # Run function with weekly resolution
  df_lagged <- add_lagged_terms(df, value_col = "value", refd_col = "ref_date",
                                lag_col = "lag", lagged_term_list = c(1, 2), temporal_resol = "daily")

  # Check that new lagged columns exist
  expect_true(all(c("value_lag1", "value_lag2") %in% colnames(df_lagged)))
  expect_true(all(c("log_delta_value_lag1", "log_delta_value_lag2") %in% colnames(df_lagged)))

  # Check if lagged values are correctly assigned
  expect_equal(df_lagged$value_lag1[5], df_lagged$value_lag2[6])  # 7-week lag should match value from first row
})



test_that("add_targets correctly adds target columns", {
  # Create a sample dataframe
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
  df <- fill_missing_updates(test_df, value_col = "value_raw", refd_col = "ref_date",
                             lag_col = "lag", temporal_resol = "daily")
  df$value_7dav <- df$value_raw

  # Apply function with a reference lag of 3
  df_new <- add_targets(df, value_col = "value_raw", refd_col = "ref_date",
                        lag_col = "lag", ref_lag = 3, temporal_resol = "daily")

  # Check if target columns are added
  expect_true("value_target" %in% colnames(df_new))
  expect_true("value_target_7dav" %in% colnames(df_new))
  expect_true("target_date" %in% colnames(df_new))

  # Verify correct target values
  expect_equal(unique(df_new[df_new$ref_date == as.Date("2022-01-03"), "value_target"]), 200)
  expect_equal(unique(df_new[df_new$ref_date == as.Date("2022-01-04"), "value_target_7dav"]), 200)
  expect_equal(unique(df_new[df_new$ref_date == as.Date("2022-01-10"), "value_target"]), NA_real_)
})

test_that("create_target_lookup selects latest genuine revision in window", {
  raw <- data.frame(
    ref_date = as.Date("2024-01-01"),
    lag = c(50, 59, 61, 62, 70),
    value = c(10, 10, 12, 15, 20)
  )
  target <- create_target_lookup(raw, "value", "ref_date", "lag", 60, 1, 2)
  expect_equal(target$target_lag, 62)
  expect_equal(target$target_date, as.Date("2024-03-03"))
  expect_equal(target$target_type, "revision")
})

test_that("create_target_lookup falls back to latest raw value at lower bound", {
  raw <- data.frame(
    ref_date = as.Date("2024-01-01"),
    lag = c(40, 55, 59, 70),
    value = c(8, 10, 10, 20)
  )
  target <- create_target_lookup(raw, "value", "ref_date", "lag", 60, 1, 2)
  expect_equal(target$target_lag, 59)
  expect_equal(target$target_type, "fallback")
})

test_that("create_target_lookup never selects negative reporting lags", {
  raw <- data.frame(
    ref_date = as.Date("2024-01-10"),
    lag = c(-7, 5),
    value = c(5, 8)
  )
  target <- create_target_lookup(raw, "value", "ref_date", "lag", 7)
  expect_equal(target$target_lag, 5)
  expect_equal(target$target_type, "fallback")
})

test_that("weekly raw targets retain Friday and keep day-based lags", {
  raw <- data.frame(
    ref_date = as.Date("2024-01-06"),
    lag = c(60, 62), # Wednesday and Friday in the same epiweek
    value = c(10, 14)
  )
  target <- create_target_lookup(
    raw, "value", "ref_date", "lag", 63, 0, 0, "weekly"
  )
  expect_equal(target$target_lag, 63)
  expect_equal(target$target_date, as.Date("2024-03-09"))
  expect_equal(target$target_type, "revision")
})

test_that("data_preprocessing attaches raw-aware target after filling", {
  raw <- data.frame(
    ref_date = rep(as.Date("2024-01-01"), 4),
    lag = c(50, 59, 61, 62),
    value = c(10, 10, 12, 15)
  )
  result <- data_preprocessing(
    raw, "value", "ref_date", "lag", 60,
    lagged_term_list = c(1, 7), temporal_resol = "daily", smoothed = TRUE,
    target_lag_lower_tolerance = 1, target_lag_upper_tolerance = 2
  )
  expect_true(all(result$target_lag == 62))
  expect_true(all(result$target_type == "revision"))
  expect_true(all(result$value_target == 15))
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


test_that("add_grouped_dayofweek creates one column per group with correct values", {
  # Mon=1, Tue=2, ..., Sun=7 via %u format
  df <- data.frame(
    date = as.Date(c("2024-02-26", "2024-02-27", "2024-02-28", "2024-03-01", "2024-03-02", "2024-03-03"))
    # Mon,        Tue,           Wed,           Fri,           Sat,           Sun
  )
  groups <- list(Mon = c("Mon"), Weekends = c("Sat", "Sun"), Other = c("Tue", "Wed", "Thurs", "Fri"))
  result <- add_grouped_dayofweek(df, "date", "_ref", groups)

  expect_true(all(c("Mon_ref", "Weekends_ref", "Other_ref") %in% colnames(result)))
  expect_equal(result$Mon_ref,      c(1L, 0L, 0L, 0L, 0L, 0L))
  expect_equal(result$Weekends_ref, c(0L, 0L, 0L, 0L, 1L, 1L))
  expect_equal(result$Other_ref,    c(0L, 1L, 1L, 1L, 0L, 0L))
  # every row sums to exactly 1 (exhaustive, non-overlapping groups)
  expect_true(all(rowSums(result[, c("Mon_ref", "Weekends_ref", "Other_ref")]) == 1L))
})

test_that("add_grouped_dayofweek derives column names from day abbreviations when unnamed", {
  df <- data.frame(date = as.Date(c("2024-02-26", "2024-03-02")))  # Mon, Sat
  result <- add_grouped_dayofweek(df, "date", "_ref", list(c("Mon"), c("Sat", "Sun")))
  expect_true("Mon_ref" %in% colnames(result))
  expect_true("SatSun_ref" %in% colnames(result))
})

test_that("add_params_for_dates uses default Mon/Weekends groups in daily mode", {
  test_df <- data.frame(
    ref_date = as.Date(c("2022-01-01", "2022-01-05", "2022-01-10", "2022-02-01", "2022-02-15")),
    lag = c(0, 2, 5, 7, 10)
  )
  df_with_params <- add_params_for_dates(test_df, "ref_date", "lag", "daily")

  expect_true("report_date" %in% colnames(df_with_params))
  expect_true(all(c("Mon_ref", "Weekends_ref", "Mon_issue", "Weekends_issue") %in% colnames(df_with_params)))
  # old per-day columns should not be present
  expect_false(any(c("Tue_ref", "Wed_ref", "Thurs_ref", "Fri_ref", "Sat_ref", "Sun_ref") %in% colnames(df_with_params)))
  # each row is either Mon (1,0), Weekends (0,1), or Other (0,0) — never (1,1)
  expect_true(all(df_with_params$Mon_ref + df_with_params$Weekends_ref <= 1L))
  expect_true(all(df_with_params$Mon_issue + df_with_params$Weekends_issue <= 1L))
  expect_true(all(WEEK_ISSUES %in% colnames(df_with_params)))
  expect_true(all(rowSums(df_with_params[, WEEK_ISSUES]) <= 1))
})

test_that("add_params_for_dates respects custom onehot_weekdays", {
  # Dates: 2024-02-26=Mon, 2024-02-28=Wed, 2024-03-01=Fri, 2024-03-02=Sat
  test_df <- data.frame(
    ref_date = as.Date(c("2024-02-26", "2024-02-28", "2024-03-01", "2024-03-02")),
    lag = c(0, 0, 0, 0)
  )
  groups <- list(WedFri = c("Wed", "Fri"), Other = c("Mon", "Tue", "Thurs", "Sat", "Sun"))
  result <- add_params_for_dates(test_df, "ref_date", "lag", "daily", onehot_weekdays = groups)

  expect_true(all(c("WedFri_ref", "Other_ref") %in% colnames(result)))
  expect_equal(result$WedFri_ref, c(0L, 1L, 1L, 0L))
  expect_equal(result$Other_ref,  c(1L, 0L, 0L, 1L))
})

test_that("add_params_for_dates correctly handles weekly resolution", {
  test_df <- data.frame(
    ref_date = as.Date(c("2022-03-01", "2022-03-08", "2022-03-15")),
    lag = c(0, 7, 14)
  )
  df_with_params <- add_params_for_dates(test_df, "ref_date", "lag", "weekly")

  expect_true("report_date" %in% colnames(df_with_params))
  # no day-of-week columns in weekly mode
  expect_false(any(c("Mon_ref", "Weekends_ref", "Mon_issue", "Weekends_issue") %in% colnames(df_with_params)))
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
  expect_warning(
    data_preprocessing(df, value_col = c("cases"), refd_col = "ref_date",
                       lag_col = "lag", ref_lag = 7, value_type = "fraction"),
    "Only one value column provided; it will be treated as a fraction."
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
  expect_true("value_7dav" %in% colnames(result_df))
  expect_true("value_7dav_num" %in% colnames(result_df))
  expect_true("value_7dav_denom" %in% colnames(result_df))
  expect_true("value_target" %in% colnames(result_df))
  expect_true("value_target_7dav" %in% colnames(result_df))
  expect_true("log_value_7dav" %in% colnames(result_df))

  expect_true("Weekends_issue" %in% colnames(result_df))
  expect_true("Weekends_ref" %in% colnames(result_df))

  expect_true("log_value_7dav_lag1" %in% colnames(result_df))
  expect_true("log_value_7dav_lag7" %in% colnames(result_df))

  expect_true("log_delta_value_7dav_lag7" %in% colnames(result_df))
  expect_true("log_delta_value_7dav_lag7" %in% colnames(result_df))

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

  # Function runs without errors
  expect_silent(result <- add_weights_related(test_df))

  # Missing required columns throws error
  incomplete_df <- test_df %>% select(-value_7dav)
  expect_error(add_weights_related(incomplete_df),
               "Missing required columns when adding weighted related features")

  # Check that correct columns are present in output
  expected_columns <- c("report_date", "value_7dav", "value_7dav_lag7", "log_value_7dav", "log_value_7dav_lag7", "lag",
                        "log_7dav_slope", "ref_slope_value", "ref_7dav_value",
                        "value_slope_diff", "value_7dav_diff")
  expect_true(all(expected_columns %in% colnames(result)))

})


make_daily_tri <- function(ref_dates, lags) {
  do.call(rbind, lapply(ref_dates, function(rd) {
    data.frame(
      reference_date = rd,
      lag = lags,
      value = as.numeric(as.Date(rd) - as.Date("2022-12-31"))
    )
  }))
}

test_that("data_preprocessing drops rows where aux has no coverage (earlier min)", {
  ref_all  <- seq(as.Date("2023-01-01"), as.Date("2023-01-15"), by = "day")
  ref_late <- seq(as.Date("2023-01-08"), as.Date("2023-01-15"), by = "day")

  primary <- make_daily_tri(ref_all, 1:3)
  aux     <- make_daily_tri(ref_late, 1:3)

  result <- data_preprocessing(
    primary,
    value_col = "value", refd_col = "reference_date", lag_col = "lag",
    ref_lag = 5L, temporal_resol = "daily",
    aux_triangles = list(beds = aux)
  )

  expect_true(all(result$reference_date >= as.Date("2023-01-08")))
  expect_true("beds_value_raw" %in% colnames(result))
  expect_false(anyNA(result[["beds_value_raw"]]))
})

test_that("data_preprocessing forward-fills aux to primary max report_date (later max)", {
  ref_dates    <- seq(as.Date("2023-01-01"), as.Date("2023-01-10"), by = "day")
  primary <- make_daily_tri(ref_dates, 1:5)
  aux     <- make_daily_tri(ref_dates, 1:3)
  # primary max report_date = 2023-01-10 + 5 = 2023-01-15
  # aux    max report_date  = 2023-01-10 + 3 = 2023-01-13

  result <- data_preprocessing(
    primary,
    value_col = "value", refd_col = "reference_date", lag_col = "lag",
    ref_lag = 6L, temporal_resol = "daily",
    aux_triangles = list(beds = aux)
  )

  # rows with lag 4 and 5 (report_dates past aux max) should be present
  expect_true(any(result$lag == 4L))
  expect_true(any(result$lag == 5L))
  expect_true("beds_value_raw" %in% colnames(result))
  expect_false(anyNA(result[["beds_value_raw"]]))
})

test_that("data_preprocessing returns 0 rows when aux has no data for the geo", {
  ref_dates <- seq(as.Date("2023-01-01"), as.Date("2023-01-15"), by = "day")
  primary   <- make_daily_tri(ref_dates, 1:4)
  empty_aux <- data.frame(
    reference_date = as.Date(character()),
    report_date    = as.Date(character()),
    lag            = integer(),
    value          = numeric()
  )

  result <- data_preprocessing(
    primary,
    value_col = "value", refd_col = "reference_date", lag_col = "lag",
    ref_lag = 5L, temporal_resol = "daily",
    aux_triangles = list(beds = empty_aux)
  )

  expect_equal(nrow(result), 0L)
})

test_that("process_aux_triangle returns zero-row df with correct columns when aux is empty", {
  empty_aux <- data.frame(
    reference_date = as.Date(character()),
    report_date    = as.Date(character()),
    lag            = integer(),
    value          = numeric()
  )
  lagged_term_list <- c(1L, 7L)

  result <- process_aux_triangle(empty_aux, "beds", lagged_term_list, "daily", TRUE)

  expect_equal(nrow(result), 0L)
  expect_true(all(aux_feature_names("beds", lagged_term_list) %in% colnames(result)))
  expect_true(all(c("reference_date", "report_date", "lag") %in% colnames(result)))
})
