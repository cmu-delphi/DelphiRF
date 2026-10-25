library(testthat)
library(stringr)

test_that("testing create directory if not exist", {
  # If not exists
  path = "test.test"
  create_dir_not_exist(path)
  expect_true(file.exists(path))

  # If already exists
  create_dir_not_exist(path)
  expect_true(file.exists(path))

  # Remove
  unlink(path, recursive = TRUE)
  expect_true(!file.exists(path))
})

test_that("create_dir_not_exist creates missing parent directories", {
  root <- tempfile("delphirf-dir-")
  path <- file.path(root, "models", "nested")
  on.exit(unlink(root, recursive = TRUE))

  expect_invisible(create_dir_not_exist(path))
  expect_true(dir.exists(path))
})


test_that("testing number of available issue dates for training", {
  start_date <- as.Date("2022-01-01")
  end_date <- as.Date("2022-01-09")
  training_days <- 10
  report_date <- seq(start_date, end_date, by = "days")
  expect_warning(training_days_check(report_date, training_days = training_days),
                 "Only 9 days are available at most for training.")

  end_date <- as.Date("2022-01-10")
  training_days <- 10
  report_date <- seq(start_date, end_date, by = "days")
  expect_silent(training_days_check(report_date, training_days = training_days))
})

test_that("testing key creation behavior", {
  value_type <- "covid"
  signal_suffix <- "52"

  expect_equal(
    make_key(value_type = value_type, signal_suffix = signal_suffix),
    str_interp("${value_type} ${signal_suffix}")
  )
  expect_equal(
    make_key(value_type = value_type, signal_suffix = ""), value_type
  )
  expect_equal(
    make_key(value_type = value_type, signal_suffix = NA), value_type
  )
})

test_that("testing assert command", {
  expect_equal(assert(TRUE), NULL)
  expect_error(assert(FALSE))
  expect_error(assert(FALSE, "Don't do that"), "Don't do that")
})

test_that("testing timestamp message command", {
  expect_message(msg_ts("Hello"), paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " --- Hello"))
})


test_that("Returns numeric value when param is a number", {
  expect_equal(handle_hyperparam(0.5, "1"), 0.5)
  expect_equal(handle_hyperparam(10, "2"), 10)
  expect_equal(handle_hyperparam(-3, "3"), -3)
})

test_that("Returns correct value from named list when test_lag_group exists", {
  param_list <- list("1" = 0.1, "2" = 0.2, "others" = 0.3)

  expect_equal(handle_hyperparam(param_list, "1"), 0.1)
  expect_equal(handle_hyperparam(param_list, "2"), 0.2)
})

test_that("Returns 'others' value when test_lag_group is not found", {
  param_list <- list("1" = 0.1, "2" = 0.2, "others" = 0.3)

  expect_equal(handle_hyperparam(param_list, "3"), 0.3)
  expect_equal(handle_hyperparam(param_list, "99"), 0.3)
})

test_that("Throws error when param is a list but has no names", {
  unnamed_list <- list(0.1, 0.2, 0.3)

  expect_error(handle_hyperparam(unnamed_list, "1"),
               "Error: Parameter is not provided for test lag group 1.")
})

test_that("Handles numeric test_lag_group properly", {
  param_list <- list("1" = 0.1, "2" = 0.2, "others" = 0.3)

  expect_equal(handle_hyperparam(param_list, 1), 0.1) # Numeric should still work
  expect_equal(handle_hyperparam(param_list, 2), 0.2)
  expect_equal(handle_hyperparam(param_list, 3), 0.3) # Falls back to "others"
})

test_that("a hyperparameter list without the group or an 'others' entry is an error", {
  expect_error(handle_hyperparam(list("1" = 0.1), "5"),
               "No value for test lag group 5 and no 'others' entry")
})
