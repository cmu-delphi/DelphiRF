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

test_that("testing get the top200 populous counties", {
  counties <- get_populous_counties()

  expect_true(length(counties) == 200)
  expect_true("06037" %in% counties)
})

test_that("testing read parameters", {
  # No input file
  expect_error(read_params(path = "params-test.json", template_path = "params-test.json.template",
                           train_models = TRUE, make_predictions = TRUE),
               "input_dir must be set in `params` and exist")

  # Check parameters
  params <- read_json("params-test.json", simplifyVector = TRUE)
  # Check initialization
  expect_true(!("export_dir" %in% names(params)))
  expect_true(!("cache_dir" %in% names(params)))

  expect_true(!("parallel" %in% names(params)))
  expect_true(!("parallel_max_cores" %in% names(params)))


  expect_true(!("taus" %in% names(params)))
  expect_true(!("lambda" %in% names(params)))
  expect_true(!("lp_solver" %in% names(params)))
  expect_true(!("lag_pad" %in% names(params)))

  expect_true(!("num_col" %in% names(params)))
  expect_true(!("denom_col" %in% names(params)))
  expect_true(!("geo_levels" %in% names(params)))
  expect_true(!("value_types" %in% names(params)))

  expect_true(!("training_days" %in% names(params)))
  expect_true(!("ref_lag" %in% names(params)))
  expect_true(!("test_dates" %in% names(params)))

  expect_true(!("make_predictions" %in% names(params)))
  expect_true(!("train_models" %in% names(params)))
  expect_true(!("indicators" %in% names(params)))

  # Create input file
  path = "test.temp"
  create_dir_not_exist(path)
  expect_silent(params <- read_params(path = "params-test.json",
                                      template_path = "params-test.json.template",
                                      train_models = TRUE, make_predictions = TRUE))
  unlink(path, recursive = TRUE)


  expect_true("export_dir" %in% names(params))
  expect_true("cache_dir" %in% names(params))

  expect_true("parallel" %in% names(params))
  expect_true("parallel_max_cores" %in% names(params))

  expect_true("taus" %in% names(params))
  expect_true("lambda" %in% names(params))
  expect_true("lp_solver" %in% names(params))
  expect_true("lag_pad" %in% names(params))

  expect_true("num_col" %in% names(params))
  expect_true("denom_col" %in% names(params))
  expect_true("geo_levels" %in% names(params))
  expect_true("value_types" %in% names(params))

  expect_true("training_days" %in% names(params))
  expect_true("ref_lag" %in% names(params))
  expect_true("test_dates" %in% names(params))

  expect_true("make_predictions" %in% names(params))
  expect_true("train_models" %in% names(params))
  expect_true("indicators" %in% names(params))

  expect_true(params$export_dir == "./receiving")
  expect_true(params$cache_dir == "./cache")

  expect_true(params$parallel == FALSE)
  expect_true(params$parallel_max_cores == .Machine$integer.max)

  expect_true(all(params$taus == TAUS))
  expect_true(params$lambda == LAMBDA)
  expect_true(params$lp_solver == LP_SOLVER)
  expect_equal(params$lag_pad, LAG_PAD)

  expect_true(params$num_col == "num")
  expect_true(params$denom_col == "denom")
  expect_true(all(params$geo_levels == c("state", "county")))
  expect_true(all(params$value_types == c("count", "fraction")))

  expect_true(params$training_days == TRAINING_DAYS)
  expect_true(params$ref_lag == REF_LAG)
  start_date <- TODAY
  end_date <- TODAY
  expect_true(all(params$test_dates == seq(start_date, end_date, by="days")))

  # Check mode settings.
  expect_equal(params$make_predictions, TRUE)
  expect_equal(params$train_models, TRUE)
  expect_equal(params$indicators, "all")

  expect_silent(file.remove("params-test.json"))
})

test_that("validity_checks alerts appropriately", {
  reference_date = as.Date(c("2022-01-01", "2022-01-02", "2022-01-03"))
  report_date = as.Date(c("2022-01-05", "2022-01-05", "2022-01-05"))
  lag = report_date - reference_date
  num = c(10, 15, 2)
  den = c(101, 104, 102)
  state_id = rep("al", 3)
  geo_value = rep("01001", 3)

  check_wrapper <- function(df, value_type, signal_suffixes = "") {
    validity_checks(df, value_type = value_type, num_col = "num",
      denom_col = "den", signal_suffixes = signal_suffixes)
  }

  missing_count_cols_error <- "No valid column name detected for the count values!"
  expect_error(check_wrapper(data.frame(), "count"),
    missing_count_cols_error)
  expect_error(check_wrapper(data.frame(num_1 = num), "count", signal_suffixes = c("1", "2")),
    missing_count_cols_error)

  missing_fraction_cols_error <- "No valid column name detected for the fraction values!"
  expect_error(check_wrapper(data.frame(), "fraction"),
    missing_fraction_cols_error)
  expect_error(check_wrapper(data.frame(num), "fraction"),
    missing_fraction_cols_error)
  expect_error(check_wrapper(data.frame(num_1 = num, den_1 = den), "count", c("1", "2")),
    missing_count_cols_error)


  expect_error(check_wrapper(data.frame(num, den), "count"),
    "No reference date column detected for the reference date!")
  expect_error(check_wrapper(data.frame(num, den, reference_date = as.character(reference_date)), "count"),
    "Reference date column must be of `Date` type")


  issued_lag_error <- "Issue date and lag fields must exist in the input data"
  expect_error(check_wrapper(data.frame(num, den, reference_date), "count"),
    issued_lag_error)
  expect_error(check_wrapper(data.frame(num, den, reference_date, lag), "count"),
    issued_lag_error)
  expect_error(check_wrapper(data.frame(num, den, reference_date, report_date), "count"),
    issued_lag_error)


  missing_val_error <- "Issue date, lag, or reference date fields contain missing values"
  df <- data.frame(num, den, reference_date, report_date, lag)
  new_row <- df[1,]
  new_row$reference_date <- NA
  expect_error(check_wrapper(bind_rows(df, new_row), "count"), missing_val_error)
  new_row <- df[1,]
  new_row$report_date <- NA
  expect_error(check_wrapper(bind_rows(df, new_row), "count"), missing_val_error)
  new_row <- df[1,]
  new_row$lag <- NA
  expect_error(check_wrapper(bind_rows(df, new_row), "count"), missing_val_error)


  expect_error(check_wrapper(data.frame(num, den, reference_date, lag, report_date = as.character(report_date)), "count"),
    "Issue date column must be of `Date` type")


  df <- data.frame(num, den, reference_date, report_date, lag, geo_value, state_id)
  expect_warning(check_wrapper(df[rep(1, 3), ], "count"),
    "Data contains duplicate rows, dropping")

  df <- data.frame(num, den, reference_date, report_date, lag, geo_value, state_id)
  df <- df[rep(1, 3), ]
  df$num <- num
  expect_error(check_wrapper(df, "count"),
    "Data contains multiple entries with differing values")

  df <- data.frame(num, den, reference_date, report_date, lag, geo_value, state_id)
  expect_error(check_wrapper(df, "count"), NA)
  expect_error(check_wrapper(df, "fraction"), NA)
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

test_that("testing params element existence checker", {
  params <- list("setting" = 5)
  expect_true(params_element_exists_and_valid(params, "setting"))
  params <- list("setting" = NA)
  expect_false(params_element_exists_and_valid(params, "setting"))
  params <- list("setting" = NULL)
  expect_false(params_element_exists_and_valid(params, "setting"))
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



