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

test_that("weekly as-of dates between grid dates do not extend a partial week", {
  df <- data.frame(
    ref_date = as.Date(c("2024-01-06", "2024-01-13")),
    lag = c(0L, 0L),
    value = c(10, 20)
  )

  expect_no_error(
    result <- fill_missing_updates(
      df, "value", "ref_date", "lag", "weekly",
      max_report_override = as.Date("2024-01-14")
    )
  )
  expect_equal(max(result$report_date), as.Date("2024-01-13"))
})

test_that("weekly data are normalized to epiweek-ending Saturdays", {
  df <- data.frame(
    ref_date = as.Date(c("2023-01-01", "2023-01-05")),
    lag = c(0, 4),
    value = c(10, 20)
  )

  filled_df <- fill_missing_updates(df, "value", "ref_date", "lag", "weekly")
  expect_true(all(weekdays(filled_df$reference_date) == "Saturday"))
  expect_true(all(weekdays(filled_df$report_date) == "Saturday"))
  expect_true(all(filled_df$lag %% 7 == 0))
})

test_that("weekly data keep the latest revision in each epiweek", {
  df <- data.frame(
    ref_date = as.Date(c("2023-01-07", "2023-01-07")),
    lag = c(4, 6), # Wednesday and Friday of the following epiweek
    value = c(10, 20)
  )

  filled_df <- fill_missing_updates(df, "value", "ref_date", "lag", "weekly")
  expect_equal(nrow(filled_df), 1)
  expect_equal(filled_df$reference_date, as.Date("2023-01-07"))
  expect_equal(filled_df$report_date, as.Date("2023-01-14"))
  expect_equal(filled_df$lag, 7)
  expect_equal(filled_df$value_raw, 20)
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

test_that("conflicting releases for one reference and report date are an error", {
  dup <- data.frame(ref = as.Date("2024-01-01") + c(0, 0, 0, 1),
                    lag = c(0, 1, 1, 0), value = c(10, 11, 99, 20))
  expect_error(fill_missing_updates(dup, "value", "ref", "lag"),
               "1 reference date/report date pair\\(s\\) have more than one value.*reference 2024-01-01 reported 2024-01-02")
  # An exact repeat of the same release is not a conflict.
  same <- rbind(dup[c(1, 2, 4), ], dup[2, ])
  expect_silent(fill_missing_updates(same, "value", "ref", "lag"))
})

test_that("fill_missing_updates keeps rows up to max_lag unchanged", {
  set.seed(4)
  raw <- expand.grid(ref = as.Date("2024-01-01") + 0:59, lag = c(0, 1, 3, 8, 20, 45))
  raw$value <- round(runif(nrow(raw), 10, 100))
  full <- fill_missing_updates(raw, "value", "ref", "lag")
  trimmed <- fill_missing_updates(raw, "value", "ref", "lag", max_lag = 10)
  expect_equal(max(trimmed$lag), 10)
  key <- function(df) df[order(df$reference_date, df$report_date), c("reference_date", "report_date", "lag", "value_raw")]
  expect_equal(key(trimmed), key(full[full$lag <= 10, ]), ignore_attr = TRUE)
})

test_that("data_preprocessing output is unchanged by trimming the grid at ref_lag + 100", {
  set.seed(8)
  # Releases far beyond the target lag, so the trim removes most of the grid.
  raw <- expand.grid(ref = as.Date("2023-01-01") + 0:299, lag = c(0:12, 30, 150, 260))
  raw$value <- round(200 + 20 * sin(as.numeric(raw$ref) / 10) - 40 * exp(-raw$lag / 3) +
                       rnorm(nrow(raw), sd = 2))
  trimmed <- data_preprocessing(raw, "value", "ref", "lag", 10)

  original_fill <- fill_missing_updates
  calls <- new.env(parent = emptyenv())
  local_mocked_bindings(
    fill_missing_updates = function(..., max_lag = NULL) {
      calls$max_lag <- max_lag
      calls$untrimmed_rows <- nrow(original_fill(...))
      original_fill(...)
    },
    .package = "DelphiRF"
  )
  untrimmed <- data_preprocessing(raw, "value", "ref", "lag", 10)

  expect_equal(calls$max_lag, 110)
  expect_identical(as.data.frame(trimmed), as.data.frame(untrimmed))
  # The trimmed grid is much smaller than the full one.
  expect_lt(nrow(original_fill(raw, "value", "ref", "lag", max_lag = 110)),
            calls$untrimmed_rows / 2)
})
