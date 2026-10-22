# Load testthat for unit testing
library(testthat)
library(dplyr)
library(stringr)

set.seed(20261005)

# Constants
indicator <- "chng"
signal <- "outpatient"
geo_level <- "state"
signal_suffix <- ""
lambda <- 0.1
gamma <- 0.1
test_lag_group <- 1
model_save_dir <- tempfile("delphirf-model-cache-")
geo <- "pa"
value_type <- "fraction"
date_format = "%Y%m%d"
training_days <- 7
training_end_date <- as.Date("2022-01-01")

# Generate Test Data
n_train <- 1000
n_test <- 60
main_covariate <- c("log_value_7dav")
weights_related <- c("value_7dav_diff", "value_slope_diff")
dayofweek_covariate <- c("Mon_ref")
response <- "log_value_target"
train_beta_vs <- log(rbeta(n_train, 2, 5))
test_beta_vs <- log(rbeta(n_test, 2, 5))
train_data <- data.frame(log_value_7dav = train_beta_vs,
                         log_value_target = train_beta_vs)
test_data <- data.frame(log_value_7dav = test_beta_vs,
                        log_value_target = test_beta_vs)
for (cov in weights_related){
  train_data[[cov]] <- rnorm(n_train, 0, 1)
  test_data[[cov]] <- rnorm(n_test, 0, 1)
}
for (cov in c(dayofweek_covariate)){
  train_data[[cov]] <- sample(c(0, 1), n_train, replace = TRUE)
  test_data[[cov]] <- sample(c(0, 1), n_test, replace = TRUE)
}
covariates <- c(main_covariate, dayofweek_covariate)

test_that("testing the generation of model filename prefix", {
  model_file_name <- generate_filename(indicator, signal,
                                       geo_level, signal_suffix, lambda, gamma,
                                       training_end_date=training_end_date,
                                       model_save_dir=model_save_dir)
  expected <- str_interp(
    file.path(
      model_save_dir,
      "chng_outpatient_state/${format(training_end_date, date_format)}_tw365_lambda0.1_gamma0.1.rds"
    )
  )
  expect_equal(model_file_name, expected)

  unlink(file.path(model_save_dir, "chng_outpatient_state"), recursive = TRUE)
})

test_that("model APIs default to the persistent user cache, except cross-validation", {
  expected_default <- quote(default_model_save_dir())
  expected_dir <- if (getRversion() >= "4.0.0") {
    tools::R_user_dir("DelphiRF", "cache")
  } else {
    file.path(tempdir(), "DelphiRF", "models")
  }

  expect_identical(formals(generate_filename)$model_save_dir, expected_default)
  expect_identical(formals(revision_forecast)$model_save_dir, expected_default)
  # Fold models are tuning by-products and go to a temporary directory.
  expect_null(formals(cv_revision_forecast)$model_save_dir)
  expect_identical(formals(DelphiRF)$model_save_dir, expected_default)
  expect_identical(default_model_save_dir(), expected_dir)
})

test_that("generating a model filename does not create its cache directory", {
  cache_dir <- tempfile("delphirf-filename-only-")

  path <- generate_filename(
    indicator, signal, geo_level, signal_suffix, lambda, gamma,
    training_end_date = training_end_date,
    model_save_dir = cache_dir
  )

  expect_false(dir.exists(cache_dir))
  expect_true(startsWith(path, cache_dir))
})

test_that("testing prediction column exponentiation", {
  # Basic example
  input <- data.frame(
    reference_date = 5,
    predicted_tau0.1 = c(0, 1, 1),
    predicted_tau0.5 = c(2, 0, 1)
  )
  expected <- data.frame(
    reference_date = 5,
    predicted_tau0.1 = exp(c(0, 1, 1)) - 1,
    predicted_tau0.5 = exp(c(2, 0, 1)) - 1,
    predicted_tau0.1_working = c(0, 1, 1),
    predicted_tau0.5_working = c(2, 0, 1)
  )
  expect_equal(expected, exponentiate_preds(input, c(0.1, 0.5), working_scale = "log_value"))
  expect_error(exponentiate_preds(input, c(0.1, 0.5)), "working_scale")
  expect_error(exponentiate_preds(input, c(0.1, 0.5), working_scale = "log"),
               "working_scale must be")


  # Realistic test df
  pred_cols = paste0("predicted_tau", TAUS)
  test_data <- mutate(test_data, reference_date = as.Date("2022-12-02"))

  for (tau in TAUS){
    test_data[[paste0("predicted_tau", as.character(tau))]] <- log(quantile(exp(train_beta_vs), tau))
  }

  expected <- test_data
  for (col_name in pred_cols){
    expected[[paste0(col_name, "_working")]] <- test_data[[col_name]]
    expected[[col_name]] <- exp(test_data[[col_name]]) - 1
  }

  result <- exponentiate_preds(test_data, TAUS, working_scale = "log_value")
  expect_equal(result, expected)

  ratio_result <- exponentiate_preds(
    input, c(0.1, 0.5), working_scale = "log_ratio"
  )
  expect_equal(
    ratio_result$predicted_tau0.1,
    exp(input$predicted_tau0.1)
  )
  expect_equal(
    ratio_result$predicted_tau0.5,
    exp(input$predicted_tau0.5)
  )
  expect_equal(ratio_result$predicted_tau0.1_working, input$predicted_tau0.1)
  expect_equal(ratio_result$predicted_tau0.5_working, input$predicted_tau0.5)
})

test_that("testing generating or loading the model", {
  # Check the model that does not exist
  tau <- 0.5
  gamma <- 0.1
  lambda <- 0.1
  model_file_name <- generate_filename(indicator, signal,
                                       geo_level, signal_suffix, lambda, gamma,
                                       geo=geo, test_lag_group=test_lag_group, tau=tau,
                                       training_end_date=training_end_date,
                                       training_days=training_days,
                                       model_save_dir=model_save_dir)
  # Generate the model and check again
  obj <- get_model(model_file_name, train_data, covariates, response, TAUS,
                   lambda, gamma, "glpk", train_models=TRUE)
  saved <- attr(obj, "model_path")
  # The file name carries the first eight characters of the contract hash.
  expect_match(basename(saved), "_c[0-9a-f]{8}[.]rds$")
  expect_equal(sub("_c[0-9a-f]{8}[.]rds$", ".rds", saved), model_file_name)
  expect_true(file.exists(saved))
  created <- file.info(saved)$ctime
  expect_equal(attr(obj, "gamma"), gamma)
  expect_equal(attr(obj, "lambda"), lambda)

  # Check that the model was not generated again.
  # Load existed model
  obj <- get_model(model_file_name, train_data, covariates, response, TAUS,
                   lambda, gamma, "glpk", train_models=FALSE)
  expect_equal(attr(obj, "model_path"), saved)
  expect_equal(file.info(saved)$ctime, created)
  # No temporary file is left behind by the atomic write.
  expect_equal(list.files(dirname(saved), all.files = TRUE, no.. = TRUE), basename(saved))

  unlink(file.path(model_save_dir, "chng_outpatient_state"), recursive = TRUE)
})

test_that("each fitting contract has its own cache file and a mismatch is caught", {
  cache <- tempfile("contract-cache-")
  path <- file.path(cache, "model.rds")
  on.exit(unlink(cache, recursive = TRUE))
  contract <- list(training_end_date = "2022-01-01", target_lag = 14)
  fit <- function(train_models, data = train_data, taus = c(0.1, 0.5, 0.9),
                  fit_contract = contract, lam = lambda) {
    get_model(path, data, covariates, response, taus, lam, gamma, "glpk",
              train_models = train_models, fit_contract = fit_contract)
  }

  fitted <- fit(TRUE)
  stored <- attr(fitted, "fit_contract")
  expect_equal(stored$taus, c(0.1, 0.5, 0.9))
  expect_equal(stored$covariates, covariates)
  expect_equal(stored$target_lag, 14)

  expect_message(loaded <- fit(FALSE), "Loading from")
  expect_equal(coef(loaded), coef(fitted))
  expect_equal(attr(loaded, "model_path"), attr(fitted, "model_path"))

  # A different contract maps to a different file: it is never loaded in
  # place of the first model, and both files are kept.
  shifted <- train_data
  shifted$log_value_target <- shifted$log_value_target + 1
  variants <- list(
    taus = function() fit(FALSE, taus = c(0.25, 0.5, 0.75)),
    data = function() fit(FALSE, data = shifted),
    cutoff = function() fit(FALSE, fit_contract = modifyList(contract, list(training_end_date = "2022-02-01"))),
    lambda = function() fit(FALSE, lam = 1)
  )
  for (name in names(variants)) {
    expect_warning(other <- variants[[name]](), "does not exist; training new model")
    expect_false(identical(attr(other, "model_path"), attr(fitted, "model_path")), info = name)
  }
  expect_length(list.files(cache), 1L + length(variants))

  # Safety net: a file whose stored contract differs from its name is rejected.
  wrong <- fitted
  attr(wrong, "fit_contract")$taus <- c(0.25, 0.5, 0.75)
  saveRDS(wrong, attr(fitted, "model_path"))
  expect_error(fit(FALSE), "differing field\\(s\\): taus")
  attr(wrong, "fit_contract") <- NULL
  saveRDS(wrong, attr(fitted, "model_path"))
  expect_error(fit(FALSE), "has no fitting contract")
})

test_that("quantreg backend implements non-negative case weights", {
  x <- matrix(1, nrow = 3, ncol = 1,
              dimnames = list(NULL, "constant_predictor"))
  y <- c(0, 10, 10)
  weighted <- fit_quantreg_lasso(
    x, y, taus = 0.5, lambda = 0.1, case_weights = c(10, 1, 1)
  )
  unweighted <- fit_quantreg_lasso(
    x, y, taus = 0.5, lambda = 0.1, case_weights = rep(1, 3)
  )
  newx <- matrix(1, nrow = 1, ncol = 1)
  expect_lt(abs(predict(weighted, newx)), 1e-5)
  expect_equal(as.numeric(predict(unweighted, newx)), 10, tolerance = 1e-5)
  expect_error(
    fit_quantreg_lasso(x, y, 0.5, case_weights = c(1, -1, 1)),
    "non-negative"
  )
})

test_that("default quantreg backend supports multiple quantiles and records backend", {
  x <- matrix(seq_len(30), ncol = 2,
              dimnames = list(NULL, c("x1", "x2")))
  y <- seq_len(15)
  fit <- fit_quantreg_lasso(x, y, taus = c(0.1, 0.5, 0.9), lambda = 0.1)
  prediction <- predict(fit, x[1:4, , drop = FALSE])
  expect_equal(dim(prediction), c(4L, 3L))

  path <- tempfile(fileext = ".rds")
  obj <- get_model(
    path, train_data, covariates, response, 0.5,
    lambda, gamma, "glpk", TRUE,
    model_backend = "quantreg"
  )
  expect_s3_class(obj, "delphirf_quantreg_lasso")
  expect_equal(attr(obj, "model_backend"), "quantreg")
  unlink(path)
})

test_that("duplicate quantile levels are fitted and predicted only once", {
  x <- cbind(first = seq_len(20), second = (seq_len(20))^2)
  y <- 3 * x[, "first"] - 0.5 * x[, "second"]
  fit <- fit_quantreg_lasso(x, y, taus = c(0.5, 0.1, 0.5, 0.1), lambda = 0)

  expect_identical(fit$taus, c(0.5, 0.1))
  expect_equal(ncol(fit$coefficients), 2L)

  test_data <- data.frame(
    first = x[1:3, "first"],
    second = x[1:3, "second"],
    response = y[1:3]
  )
  result <- get_prediction(
    test_data, taus = c(0.5, 0.1, 0.5, 0.1),
    covariates = c("first", "second"), response = "response", obj = fit
  )
  expect_equal(
    grep("^predicted_tau", names(result), value = TRUE),
    c("predicted_tau0.5", "predicted_tau0.1")
  )
})

test_that("quantreg backend handles rank-deficient predictors", {
  x <- cbind(
    original = seq_len(30),
    duplicate = 2 * seq_len(30),
    constant = 1
  )
  y <- seq_len(30) + rep(c(-1, 0, 1), 10)

  fit <- fit_quantreg_lasso(x, y, taus = c(0.1, 0.5, 0.9))
  predictions <- predict(fit, newx = x[1:3, , drop = FALSE])

  expect_equal(dim(predictions), c(3L, 3L))
  expect_true(all(is.finite(predictions)))
  expect_true(length(fit$dropped_predictors) >= 2L)
  expect_equal(nrow(fit$coefficients), ncol(x) + 1L)
})

test_that("named prediction data are validated and reordered", {
  x <- data.frame(first = seq_len(20), second = (seq_len(20))^2)
  y <- 3 * x$first - 0.5 * x$second
  fit <- fit_quantreg_lasso(x, y, taus = 0.5, lambda = 0)

  expected <- predict(fit, x[1:4, , drop = FALSE])
  reordered <- x[1:4, c("second", "first"), drop = FALSE]
  expect_equal(predict(fit, reordered), expected)

  expect_error(
    predict(fit, x[1:4, "first", drop = FALSE]),
    "missing: second"
  )
  with_extra <- x[1:4, , drop = FALSE]
  with_extra$other <- 1
  expect_error(predict(fit, with_extra), "unexpected: other")

  duplicated <- as.matrix(x[1:4, , drop = FALSE])
  colnames(duplicated) <- c("first", "first")
  expect_error(
    predict(fit, duplicated),
    "missing: second; duplicated: first"
  )
})

test_that("unnamed prediction matrices retain positional behavior", {
  x <- cbind(first = seq_len(20), second = (seq_len(20))^2)
  y <- 3 * x[, "first"] - 0.5 * x[, "second"]
  fit <- fit_quantreg_lasso(x, y, taus = 0.5, lambda = 0)
  newx <- unname(x[1:4, , drop = FALSE])

  expect_equal(predict(fit, newx), predict(fit, x[1:4, , drop = FALSE]))
  expect_error(
    predict(fit, newx[, 1, drop = FALSE]),
    "does not have the fitted number of predictors"
  )
})

test_that("optional quantgen backend records its backend", {
  skip_if_not_installed("quantgen")
  legacy_path <- tempfile(fileext = ".rds")
  legacy <- get_model(
    legacy_path, train_data, covariates, response, 0.5,
    lambda, gamma, "glpk", TRUE,
    model_backend = "quantgen"
  )
  expect_equal(attr(legacy, "model_backend"), "quantgen")
  expect_true(is.finite(as.numeric(predict(
    legacy, newx = as.matrix(test_data[1, covariates, drop = FALSE])
  ))))
  unlink(legacy_path)
})

test_that("testing making predictions", {
  # Mock model object for testing
  tau <- 0.5
  model_file_name <- generate_filename(indicator, signal,
                                       geo_level, signal_suffix, lambda, gamma,
                                       geo=geo, test_lag_group=test_lag_group, tau=tau,
                                       training_end_date=training_end_date,
                                       training_days=training_days,
                                       model_save_dir=model_save_dir)

  obj <- get_model(model_file_name, train_data, covariates, response, tau,
                   lambda, gamma, "glpk", train_models=TRUE)

  expect_error(get_prediction(test_data, tau, covariates, response, NULL),
               "Model not found. Ensure the model is trained or loaded before prediction.")

  result <- get_prediction(test_data, tau, covariates, response, obj)

  expect_s3_class(result, "tbl_df")
  expect_true("predicted_tau0.5" %in% colnames(result))
  expect_equal(nrow(result), nrow(test_data))
  expect_false(any(is.na(result$predicted_tau0.5)))
  expect_true("gamma" %in% colnames(result))
  expect_true("lambda" %in% colnames(result))
  expect_equal(unique(result$gamma), gamma)
  expect_equal(unique(result$lambda), lambda)
  unlink(file.path(model_save_dir, "chng_outpatient_state"), recursive = TRUE)
})


test_that("evaluate uses twice mean pinball loss and preserves missing rows", {
  taus <- c(0.1, 0.5, 0.9)
  test_data <- data.frame(
    reference_date = c(as.Date("2022-01-01"), as.Date("2022-01-02")),
    report_date = c(as.Date("2022-01-02"), as.Date("2022-01-02")),
    location = "test_location",
    predicted_tau0.1 = c(8, NA),
    predicted_tau0.5 = c(9, NA),
    predicted_tau0.9 = c(13, NA),
    response = c(10, NA)
  )

  result <- evaluate(test_data, taus, "response")

  expect_s3_class(result, "data.frame")
  expect_true("wis" %in% colnames(result))
  expect_type(result$wis, "double")
  expected <- mean(c(0.4, 1.0, 0.6))
  expect_equal(result$wis, c(expected, NA_real_), tolerance = 1e-12)
})

test_that("median-only WIS equals absolute error for either error sign", {
  test_data <- data.frame(
    predicted_tau0.5 = c(8, 12, 10),
    response = c(10, 10, 10)
  )

  result <- evaluate(test_data, 0.5, "response")

  expect_equal(result$wis, c(2, 2, 0))
  expect_equal(result$wis, abs(test_data$predicted_tau0.5 - test_data$response))
})

test_that("testing data_filteration", {
  data <- data.frame(lag = -5:5, value = rnorm(11))

  # Test with lag_pad as a single value
  result <- data_filteration(test_lag = 0, data = data, lag_pad = 2)
  expected_lags <- -2:2
  expect_equal(result$lag, expected_lags)

  # Test with lag_pad as a vector (different left and right padding)
  result <- data_filteration(test_lag = 0, data = data, lag_pad = c(3, 1))
  expected_lags <- -3:1
  expect_equal(result$lag, expected_lags)

  # Test with lag_pad larger than two elements (should issue a warning)
  expect_warning(
    data_filteration(test_lag = 0, data = data, lag_pad = c(2, 1, 5)),
    "lag_pad has more than two elements. Only the first two will be used."
  )

  # Test edge case: when no data matches the filtering condition
  result <- data_filteration(test_lag = 10, data = data, lag_pad = 1)
  expect_equal(nrow(result), 0)  # Should return an empty dataframe

  # Test with test_lag as a vector
  result <- data_filteration(test_lag = c(-1, 1), data = data, lag_pad = 2)
  expected_lags <- -3:3
  expect_equal(result$lag, expected_lags)

  # Test with test_lag larger than two elements
  expect_warning(
    data_filteration(test_lag = c(-1, 0, 1), data = data, lag_pad = 2),
    "test_lag has more than two elements. Only the first two will be used."
  )

  # Test with lag_pad as a vector as while test_lag_group as a vector
  result <- data_filteration(test_lag = c(-1, 1), data = data, lag_pad = c(1, 2))
  expected_lags <- -2:3
  expect_equal(result$lag, expected_lags)

})

test_that("revision_forecast with train_models=FALSE loads cached model and produces identical predictions", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE))

  set.seed(42)
  nn_train <- 200
  nn_test  <- 30

  make_rf_data <- function(nn, start_date) {
    dates <- seq(as.Date(start_date), by = "day", length.out = nn)
    data.frame(
      reference_date   = dates,
      report_date      = dates + 3L,
      target_date      = dates + 3L,
      lag              = 3L,
      value_7dav       = runif(nn, 0, 1),
      log_value_7dav   = rnorm(nn),
      log_value_target = rnorm(nn),
      value_7dav_diff  = rnorm(nn),
      value_slope_diff = rnorm(nn),
      Mon_ref          = sample(c(0L, 1L), nn, replace = TRUE)
    )
  }

  train_data <- make_rf_data(nn_train, "2021-01-01")
  test_data  <- make_rf_data(nn_test,  "2021-07-20")

  rf_args <- list(
    taus             = 0.5,
    smoothed_target  = FALSE,
    params_list      = c("log_value_7dav", "Mon_ref"),
    temporal_resol   = "daily",
    lambda           = 0.1,
    gamma            = 0.1,
    lp_solver        = "glpk",
    model_save_dir   = tmpdir,
    indicator        = "test",
    signal           = "sig",
    geo_level        = "state",
    geo              = "pa",
    training_days    = nn_train,
    make_predictions = TRUE,
    working_scale    = "log_value"
  )

  result_fit <- do.call(
    revision_forecast,
    c(list(train_data = train_data, test_data = test_data, train_models = TRUE), rf_args)
  )
  expect_s3_class(result_fit, "tbl_df")
  expect_gt(nrow(result_fit), 0)

  result_cached <- do.call(
    revision_forecast,
    c(list(train_data = train_data, test_data = test_data, train_models = FALSE), rf_args)
  )
  expect_gt(nrow(result_cached), 0)
  expect_equal(result_fit$predicted_tau0.5, result_cached$predicted_tau0.5)
})

test_that("revision_forecast without predictions still fits every lambda-gamma model", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE))

  set.seed(7)
  nn <- 60
  dates <- seq(as.Date("2021-01-01"), by = "day", length.out = nn)
  train_data <- data.frame(
    reference_date   = dates,
    report_date      = dates + 3L,
    target_date      = dates + 3L,
    lag              = 3L,
    log_value_7dav   = rnorm(nn),
    log_value_target = rnorm(nn),
    value_7dav_diff  = rnorm(nn),
    value_slope_diff = rnorm(nn)
  )

  result <- revision_forecast(
    train_data = train_data, test_data = train_data[0, ], taus = 0.5,
    smoothed_target = FALSE, params_list = "log_value_7dav",
    lambda = c(0.1, 1), gamma = c(0.1, 1),
    model_save_dir = tmpdir, indicator = "test", signal = "sig",
    geo = "pa", training_days = nn, working_scale = "log_value",
    train_models = TRUE, make_predictions = FALSE
  )

  expect_equal(nrow(result), 0L)
  saved <- list.files(tmpdir, pattern = "[.]rds$", recursive = TRUE)
  expect_length(saved, 4L)
  for (combo in c("lambda0.1_gamma0.1", "lambda0.1_gamma1",
                  "lambda1_gamma0.1", "lambda1_gamma1")) {
    expect_true(any(grepl(combo, saved, fixed = TRUE)), info = combo)
  }
})

test_that("DelphiRF trains on releases reported up to and including the cutoff", {
  cutoff <- as.Date("2024-01-10")
  n_train <- 12L
  n_test <- 2L
  values <- seq_len(n_train + n_test)
  df <- data.frame(
    reference_date = seq(cutoff - n_train, by = "day", length.out = n_train + n_test),
    report_date = c(seq(cutoff - n_train, cutoff - 1L, by = "day"),
                    seq(cutoff, by = "day", length.out = n_test)),
    target_date = rep(cutoff, n_train + n_test),
    genuine_event = rep(TRUE, n_train + n_test),
    lag = rep(8L, n_train + n_test),
    value_7dav = values,
    value_7dav_lag7 = values,
    log_value_7dav = log1p(values),
    log_value_7dav_lag7 = log1p(values)
  )

  local_mocked_bindings(
    revision_forecast = function(train_data, test_data, ...) {
      test_data$latest_training_report_date <- max(train_data$report_date)
      test_data
    },
    .package = "DelphiRF"
  )

  result <- DelphiRF(
    df,
    testing_start_date = cutoff,
    working_scale = "log_value",
    test_lag_groups = 8,
    lag_pad = 0,
    training_days = 30
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(result$latest_training_report_date <= cutoff))
  expect_equal(unique(result$latest_training_report_date), cutoff)
})

test_that("DelphiRF honors an explicit training end date", {
  cutoff <- as.Date("2024-01-10")
  training_end <- cutoff - 3L
  dates <- seq(cutoff - 12L, cutoff + 1L, by = "day")
  values <- seq_along(dates)
  df <- data.frame(
    reference_date = dates,
    report_date = dates,
    target_date = dates,
    genuine_event = TRUE,
    lag = 8L,
    value_7dav = values,
    value_7dav_lag7 = values,
    log_value_7dav = log1p(values),
    log_value_7dav_lag7 = log1p(values)
  )

  local_mocked_bindings(
    revision_forecast = function(train_data, test_data, training_end_date, ...) {
      test_data$latest_training_report_date <- max(train_data$report_date)
      test_data$received_training_end_date <- as.Date(training_end_date)
      test_data
    },
    .package = "DelphiRF"
  )

  result <- DelphiRF(
    df,
    testing_start_date = cutoff,
    working_scale = "log_value",
    training_end_date = training_end,
    test_lag_groups = 8,
    lag_pad = 0,
    training_days = 30,
    genuine_training = FALSE,
    genuine_testing = FALSE
  )

  expect_true(all(result$latest_training_report_date <= training_end))
  expect_equal(unique(result$received_training_end_date), training_end)
  expect_error(
    DelphiRF(df, testing_start_date = cutoff,
    working_scale = "log_value",
             training_end_date = cutoff + 1L),
    "cannot be after the forecast origin"
  )
})

test_that("forecast entry points validate the training cutoff", {
  minimal <- data.frame(
    report_date = as.Date("2024-01-01"),
    target_date = as.Date("2024-01-01"),
    lag = 1
  )

  expect_error(
    revision_forecast(
      minimal, minimal[0, ], taus = 0.5,
      working_scale = "log_value", training_end_date = "not-a-date"
    ),
    "training_end_date must be empty or one valid date"
  )
  expect_error(
    revision_forecast(
      minimal, minimal[0, ], taus = 0.5,
      working_scale = "log_value",
      training_end_date = c("2024-01-01", "2024-01-02")
    ),
    "training_end_date must be empty or one valid date"
  )
  expect_error(
    cv_revision_forecast(
      minimal, test_lag = 1, taus = 0.5,
      working_scale = "log_value", training_end_date = "not-a-date",
      genuine_training = FALSE, genuine_testing = FALSE
    ),
    "training_end_date must be empty or one valid date"
  )
})

test_that("cross-validation respects its training window", {
  training_end <- as.Date("2024-02-01")
  target_dates <- c(
    rep(as.Date("2024-01-01"), 10),
    rep(seq(training_end - 9L, training_end, by = "day"), each = 3),
    rep(training_end + 1L, 10)
  )
  nn <- length(target_dates)
  df <- data.frame(
    reference_date = as.Date("2023-12-01") + seq_len(nn),
    report_date = rep(training_end, nn),
    target_date = target_dates,
    lag = rep(8L, nn)
  )
  seen <- new.env(parent = emptyenv())
  seen$training_target_dates <- list()
  seen$validation_target_dates <- list()

  local_mocked_bindings(
    revision_forecast = function(train_data, test_data, ...) {
      seen$training_target_dates[[length(seen$training_target_dates) + 1L]] <-
        train_data$target_date
      seen$validation_target_dates[[length(seen$validation_target_dates) + 1L]] <-
        test_data$target_date
      data.frame(wis_working = 0)
    },
    .package = "DelphiRF"
  )

  cv_revision_forecast(
    df, test_lag = 8L, taus = 0.5, working_scale = "log_value",
    lambda_candidates = 0.1, gamma_candidates = 0.1,
    lag_pad_candidates = 0, n_folds = 2,
    training_end_date = training_end, training_days = 10,
    genuine_training = FALSE, genuine_testing = FALSE
  )

  used_dates <- c(
    unlist(seen$training_target_dates),
    unlist(seen$validation_target_dates)
  )
  expect_gt(length(used_dates), 0L)
  expect_true(all(used_dates > training_end - 10L))
  expect_true(all(used_dates <= training_end))
})

test_that("quantile rearrangement sorts each prediction row", {
  predictions <- matrix(c(
    0.4, 0.2, 0.1, 0.9, 0.3,
    1.0, 1.0, 0.5, 2.0, 1.5
  ), nrow = 2, byrow = TRUE)
  rearranged <- rearrange_quantile_predictions(predictions)
  expect_equal(rearranged[1, ], c(0.1, 0.2, 0.3, 0.4, 0.9))
  expect_equal(rearranged[2, ], c(0.5, 1.0, 1.0, 1.5, 2.0))
  expect_true(all(apply(rearranged, 1, function(x) all(diff(x) >= 0))))
})

test_that("get_prediction returns monotone quantiles", {
  test_data <- data.frame(x = c(0, 1), truth = c(0, 0))
  fake_model <- structure(list(), gamma = 0.1, lambda = 0.1)
  local_mocked_bindings(
    predict = function(object, newx) {
      matrix(rep(c(0.4, 0.2, 0.1, 0.9, 0.3), each = nrow(newx)),
             nrow = nrow(newx))
    },
    .package = "DelphiRF"
  )
  result <- get_prediction(test_data, c(.1, .2, .5, .8, .9), "x", "truth",
                           fake_model, make_evaluation = FALSE)
  expect_equal(as.numeric(result[1, paste0("predicted_tau", c(.1,.2,.5,.8,.9))]),
               c(0.1, 0.2, 0.3, 0.4, 0.9))
})

test_that("forecasts carry WIS on both the working and the reporting scale", {
  cache <- tempfile("wis-cache-")
  on.exit(unlink(cache, recursive = TRUE))
  set.seed(3)
  nn <- 60
  dates <- seq(as.Date("2021-01-01"), by = "day", length.out = nn)
  train <- data.frame(reference_date = dates, report_date = dates + 3L, target_date = dates + 3L, lag = 3L,
                      log_value_7dav = rnorm(nn, 5), log_value_target = rnorm(nn, 5),
                      value_7dav_diff = runif(nn), value_slope_diff = runif(nn))
  test <- train[1:5, ]
  taus <- c(0.1, 0.5, 0.9)
  for (scale in c("log_value", "log_ratio")) {
    result <- revision_forecast(train, test, taus = taus, smoothed_target = FALSE,
                                params_list = "log_value_7dav", working_scale = scale,
                                model_save_dir = cache)
    expect_false("wis" %in% names(result))
    working <- as.matrix(result[paste0("predicted_tau", taus, "_working")])
    reporting <- as.matrix(result[paste0("predicted_tau", taus)])
    truth <- result$log_value_target
    truth_reporting <- if (scale == "log_value") exp(truth) - 1 else exp(truth)
    for (i in seq_len(nrow(result))) {
      expect_equal(result$wis_working[i],
                   weighted_interval_score(taus, working[i, ] - truth[i]))
      expect_equal(result$wis_reporting[i],
                   weighted_interval_score(taus, reporting[i, ] - truth_reporting[i]))
    }
  }
})

test_that("a group whose test rows all lack predictors returns no forecasts", {
  cache <- tempfile("empty-cache-")
  on.exit(unlink(cache, recursive = TRUE))
  nn <- 40
  dates <- seq(as.Date("2021-01-01"), by = "day", length.out = nn)
  train <- data.frame(reference_date = dates, report_date = dates + 3L, target_date = dates + 3L, lag = 3L,
                      log_value_7dav = rnorm(nn), log_value_target = rnorm(nn),
                      value_7dav_diff = runif(nn), value_slope_diff = runif(nn))
  test <- train[1:3, ]
  test$log_value_7dav <- NA_real_
  result <- revision_forecast(train, test, taus = c(0.1, 0.5, 0.9), smoothed_target = FALSE,
                              params_list = "log_value_7dav", working_scale = "log_value",
                              model_save_dir = cache)
  expect_equal(nrow(result), 0L)
  # The model is still fitted and saved for this group.
  expect_length(list.files(cache, pattern = "[.]rds$", recursive = TRUE), 1L)
})

test_that("cross-validation writes fold models to a temporary directory", {
  df <- data.frame(
    reference_date = as.Date("2024-01-01") + 0:39,
    report_date = as.Date("2024-01-02") + 0:39,
    target_date = as.Date("2024-02-01"), lag = 8L
  )
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    revision_forecast = function(train_data, test_data, model_save_dir, ...) {
      seen$dir <- model_save_dir
      dir.create(model_save_dir, recursive = TRUE, showWarnings = FALSE)
      data.frame(wis_working = 0)
    },
    .package = "DelphiRF"
  )
  cv_revision_forecast(df, test_lag = 8L, taus = 0.5, working_scale = "log_value",
                       lambda_candidates = 0.1, gamma_candidates = 0.1,
                       lag_pad_candidates = 0, n_folds = 2,
                       genuine_training = FALSE, genuine_testing = FALSE)
  expect_true(startsWith(normalizePath(dirname(seen$dir)), normalizePath(tempdir())))
  expect_false(identical(seen$dir, default_model_save_dir()))
  expect_false(dir.exists(seen$dir))
})

test_that("training rows whose target matures after the training end date are dropped", {
  cache <- tempfile("maturity-cache-")
  on.exit(unlink(cache, recursive = TRUE))
  set.seed(5)
  nn <- 40
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = nn)
  train <- data.frame(reference_date = dates, report_date = dates + 2L, lag = 2L,
                      target_date = dates + 7L, log_value_7dav = rnorm(nn),
                      log_value_target = rnorm(nn), value_7dav_diff = runif(nn),
                      value_slope_diff = runif(nn))
  end <- dates[30]
  fit_once <- function(data) {
    revision_forecast(data, data[1:2, ], taus = 0.5, smoothed_target = FALSE,
                      params_list = "log_value_7dav", working_scale = "log_value",
                      training_end_date = as.character(end), model_save_dir = cache,
                      make_predictions = FALSE)
    path <- list.files(cache, pattern = "[.]rds$", full.names = TRUE, recursive = TRUE)
    model <- readRDS(path)
    unlink(path)
    model
  }
  # Reported by the end date but maturing after it: these rows carry future
  # target values and must not be used.
  expect_true(any(train$report_date <= end & train$target_date > end))
  model <- fit_once(train)
  expect_length(model$training_weights, sum(train$target_date <= end))

  # Changing the targets of rows that are not yet mature changes nothing.
  leaked <- train
  leaked$log_value_target[leaked$target_date > end] <- 1e6
  expect_equal(coef(fit_once(leaked)), coef(model))

  expect_error(revision_forecast(train[setdiff(names(train), "target_date")], train[1:2, ],
                                 taus = 0.5, params_list = "log_value_7dav",
                                 working_scale = "log_value", model_save_dir = cache),
               "target_date")
})

test_that("a constant training response is fitted and saved like any other", {
  cache <- tempfile("constant-cache-")
  on.exit(unlink(cache, recursive = TRUE))
  set.seed(9)
  nn <- 30
  dates <- as.Date("2024-01-01") + 0:(nn - 1)
  train <- data.frame(reference_date = dates, report_date = dates + 2L, target_date = dates + 2L,
                      lag = 2L, log_value_7dav = rnorm(nn), value_7dav_diff = runif(nn),
                      value_slope_diff = runif(nn), log_value_target = log1p(50))
  test <- train[1:3, ]
  test$log_value_7dav[2] <- NA
  result <- revision_forecast(train, test, taus = c(0.1, 0.5, 0.9), smoothed_target = FALSE,
                              params_list = "log_value_7dav", lambda = c(0.1, 1),
                              gamma = c(0.1, 1), working_scale = "log_value",
                              model_save_dir = cache)
  # One forecast set per lambda-gamma pair, for the rows with every predictor.
  expect_setequal(paste(result$lambda, result$gamma),
                  c("0.1 0.1", "0.1 1", "1 0.1", "1 1"))
  expect_equal(nrow(result), 4L * 2L)
  expect_false(as.Date("2024-01-02") %in% result$reference_date)
  expect_equal(result$predicted_tau0.5, rep(50, nrow(result)), tolerance = 1e-5)
  # Every model is saved for later use.
  expect_length(list.files(cache, pattern = "[.]rds$", recursive = TRUE), 4L)
})
