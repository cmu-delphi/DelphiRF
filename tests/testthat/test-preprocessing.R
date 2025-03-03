library(testthat)

library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)

refd_col <- "reference_date"
lag_col <- "lag"
value_col <- "Counts_Products_Denom"
min_refd <- as.Date("2022-01-01")
max_refd <- as.Date("2022-01-07")
ref_lag <- 7
fake_df <- data.frame(reference_date = c(as.Date("2022-01-03"), as.Date("2022-01-03"),
                                     as.Date("2022-01-03"), as.Date("2022-01-03"),
                                     as.Date("2022-01-04"), as.Date("2022-01-04"),
                                     as.Date("2022-01-04"), as.Date("2022-01-05"),
                                     as.Date("2022-01-05")),
                      lag = c(0, 1, 3, 7, 0, 6, 7, 0, 7),
                      Counts_Products_Denom=c(100, 200, 500, 1000, 0, 200, 220, 50, 300))
wd <- c("Mon", "Tue", "Wed", "Thurs", "Fri", "Sat")
wm <- c("W1_issue", "W2_issue", "W3_issue", "W4_issue")


test_that("testing rows filling for missing lags", {
  # Make sure all reference date have enough rows for updates
  df_new <- fill_rows(fake_df, refd_col, lag_col, min_refd, max_refd, ref_lag)
  n_refds <- as.numeric(max_refd - min_refd)+1

  expect_equal(nrow(df_new), n_refds*(ref_lag+31))
  expect_equal(df_new %>% drop_na(), fake_df)
})



test_that("fill_missing_updates correctly fills missing values for daily data", {
  df <- data.frame(
    ref_date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-04")),
    lag = c(0, 1, 2),
    value = c(10, NA, 30)
  )

  filled_df <- fill_missing_updates(df, value_col = "value", refd_col = "ref_date",
                                    lag_col = "lag", temporal_resol = "daily")

  expect_equal(nrow(filled_df), 3 + 4 + 5 + 6)  # 4 reference dates, 6 report dates
  expect_true(all(!is.na(filled_df$value_raw)))  # No NA values should remain
  expect_equal(unique(filled_df$value_raw[filled_df$ref_date == as.Date("2024-01-03")]), 0)  # Forward fill check
  expect_equal(unique(filled_df$value_raw[(filled_df$ref_date == as.Date("2024-01-04"))
                                          & (filled_df$lag < 2)]), 0)  # Forward fill check
})

test_that("fill_missing_updates correctly processes weekly data", {
  df <- data.frame(
    ref_date = as.Date(c("2023-01-01", "2023-01-08")),
    lag = c(0, 7),
    value = c(5, NA)
  )

  filled_df <- fill_missing_updates(df, "value", "ref_date", "lag", "weekly")

  expect_true(all(!is.na(filled_df$value_raw)))  # Should be filled correctly
  expect_equal(nrow(filled_df), 2 + 3)  # Two rows expected since it's weekly data
})

test_that("fill_missing_updates raises an error for irregular gaps", {
  df <- data.frame(
    ref_date = as.Date(c("2023-01-01", "2023-01-05")),
    lag = c(0, 4),
    value = c(10, 20)
  )

  expect_error(
    fill_missing_updates(df, "value", "ref_date", "lag", "weekly"),
    "The reference dates do not regularly have a gap of 7 days. Some reference dates will be ignored. Please check your input data."
  )  # Function should stop due to irregular gap

  df <- data.frame(
    ref_date = as.Date(c("2023-01-01", "2023-01-08")),
    lag = c(0, 4),
    value = c(10, 20)
  )

  expect_error(
    fill_missing_updates(df, "value", "ref_date", "lag", "weekly"),
    "The report dates do not regularly have a gap of 7 days. Some report dates will be ignored. Please check your input data."
  )  # Function should stop due to irregular gap
})


test_that("fill_missing_updates raises an error for invalid temporal resolution", {
  df <- data.frame(
    ref_date = as.Date(c("2023-01-01", "2023-01-02")),
    lag = c(0, 1),
    value = c(10, 20)
  )

  expect_error(
    fill_missing_updates(df, "value", "ref_date", "lag", "hourly"),
    "Invalid temporal_resol"
  )  # Should only accept "daily" or "weekly"
})


test_that("fill_missing_updates correctly fills missing intermediate values", {
  df <- data.frame(
    ref_date = as.Date(c("2023-01-01", "2023-01-03", "2023-01-05")),
    lag = c(0, 2, 4),
    value = c(5, NA, 15)
  )

  filled_df <- fill_missing_updates(df, "value", "ref_date", "lag", "daily")

  expect_true(all(!is.na(filled_df$value_raw)))  # No missing values after filling
  expect_equal(nrow(filled_df), 5 + 6 + 7 + 8 + 9)  # Expansion should be correct
})

test_that("fill_missing_updates correctly handles empty input", {
  df <- data.frame(
    ref_date = as.Date(character(0)),
    lag = integer(0),
    value = numeric(0)
  )

  filled_df <- fill_missing_updates(df, "value", "ref_date", "lag", "daily")

  expect_equal(nrow(filled_df), 0)  # Output should be empty
})

test_that("add_shift correctly shifts reference dates", {
  # Sample data
  df <- data.frame(
    ref_date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    value = c(10, 20, 30)
  )

  # Test with positive shift
  shifted_df <- add_shift(df, 3, "ref_date")
  expect_equal(shifted_df$ref_date, as.Date(c("2024-01-04", "2024-01-05", "2024-01-06")))

  # Test with negative shift
  shifted_df_neg <- add_shift(df, -2, "ref_date")
  expect_equal(shifted_df_neg$ref_date, as.Date(c("2023-12-30", "2023-12-31", "2024-01-01")))

  # Test with zero shift (should remain unchanged)
  shifted_df_zero <- add_shift(df, 0, "ref_date")
  expect_equal(shifted_df_zero$ref_date, df$ref_date)

  # Check if the value column remains unchanged
  expect_equal(shifted_df$value, df$value)
  expect_equal(shifted_df_neg$value, df$value)
  expect_equal(shifted_df_zero$value, df$value)
})


test_that("testing the calculation of week of a month", {
  expect_equal(get_weekofmonth(as.Date("2021-12-31")), 1)
  expect_equal(get_weekofmonth(as.Date("2022-01-01")), 1)
  expect_equal(get_weekofmonth(as.Date("2022-01-02")), 2)
  expect_equal(get_weekofmonth(as.Date("2022-01-09")), 3)

  expect_equal(get_weekofmonth(as.Date("2022-09-01")), 1)
  expect_equal(get_weekofmonth(as.Date("2022-09-04")), 2)
  expect_equal(get_weekofmonth(as.Date("2022-09-24")), 4)
  expect_equal(get_weekofmonth(as.Date("2022-09-25")), 1)

  expect_equal(get_weekofmonth(as.Date("2022-10-01")), 1)
  expect_equal(get_weekofmonth(as.Date("2022-10-02")), 2)
  expect_equal(get_weekofmonth(as.Date("2022-10-09")), 3)
  expect_equal(get_weekofmonth(as.Date("2022-10-16")), 4)
  expect_equal(get_weekofmonth(as.Date("2022-10-23")), 5)
  expect_equal(get_weekofmonth(as.Date("2022-10-30")), 1)

})



