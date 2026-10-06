#' Filter Training and Testing Data Based on Lag
#'
#' This function filters the input data based on the specified test lag and lag padding.
#' The lag padding can be either a single value (applied symmetrically) or a vector of two values,
#' specifying separate left and right padding.
#'
#' @param test_lag Numeric or vector, the reference lag used for filtering.
#' @param data Data frame containing the lagged data to be filtered.
#' @param lag_pad Numeric or vector, specifying the lag padding. If a single value is provided,
#'   it is applied symmetrically. If a vector of length two is provided, the first element is used
#'   for left padding and the second for right padding.
#'
#' @importFrom rlang .data .env
#'
#' @export
data_filteration <- function(test_lag, data, lag_pad) {
  if (length(lag_pad) == 1) {
    lag_pad_l <- lag_pad
    lag_pad_r <- lag_pad
  } else {
    lag_pad_l <- lag_pad[1]
    lag_pad_r <- lag_pad[2]
    if (length(lag_pad) > 2) {
      warning("lag_pad has more than two elements. Only the first two will be used.")
    }
  }
  if (length(test_lag) == 1) {
    test_lag_l <- test_lag
    test_lag_r <- test_lag
  } else {
    test_lag_l <- test_lag[1]
    test_lag_r <- test_lag[2]
    if (length(test_lag) > 2) {
      warning("test_lag has more than two elements. Only the first two will be used.")
    }
  }
  filtered_data <- data %>%
    filter(.data$lag >= test_lag_l - lag_pad_l & .data$lag <= test_lag_r + lag_pad_r)
  return (filtered_data)
}

#' Add Square Root Scale Indicator Columns
#'
#' This function adds new columns to the dataset to indicate
#' the scale of values at the square root level. The function divides the range
#' of values into 4 square root-based bins and assigns binary indicators.
#'
#' @param df Data frame containing the data.
#' @param sqrt_max_raw The maximum value in the dataset, used to determine bin thresholds.
#' @return A data frame with additional binary indicator columns for square root scaling.
#' @export
add_sqrtscale <- function(df, sqrt_max_raw, rare_thresh = 0.05) {
  bin_counts <- c()
  n <- nrow(df)

  for (split in seq(0, 2)) {
    y_col <- paste0("sqrty", split)
    qv_pre <- sqrt_max_raw * split / 4
    qv_next <- sqrt_max_raw * (split + 1) / 4

    df[[y_col]] <- ifelse((df$value_7dav >= (qv_pre)^2) & (df$value_7dav < (qv_next)^2), 1, 0)
    bin_counts[y_col] <- sum(df[[y_col]])
  }

  # Keep only frequent bins
  kept_bins <- names(bin_counts)[bin_counts / n >= rare_thresh]
  df <- df[, c(setdiff(names(df), names(bin_counts)), kept_bins)]

  return(list(data = as.data.frame(df), kept_bins = kept_bins))
}

add_sqrtscale_test <- function(df, sqrt_max_raw, kept_bins) {
  for (split in seq(0, 2)) {
    y_col <- paste0("sqrty", split)
    qv_pre <- sqrt_max_raw * split / 4
    qv_next <- sqrt_max_raw * (split + 1) / 4

    df[[y_col]] <- ifelse((df$value_7dav >= (qv_pre)^2) & (df$value_7dav < (qv_next)^2), 1, 0)
  }

  # Keep only the columns selected from training
  all_sqrty_cols <- paste0("sqrty", 0:2)
  drop_cols <- setdiff(all_sqrty_cols, kept_bins)
  df <- df[, !(names(df) %in% drop_cols)]

  return(as.data.frame(df))
}


#' Rearrange quantile predictions to prevent crossing
#'
#' Sorts each row of a prediction matrix while retaining the same fitted values.
#'
#' @param predictions Numeric matrix with rows as observations and columns as
#'   ordered quantile levels.
#' @return A numeric matrix whose rows are nondecreasing.
#' @export
rearrange_quantile_predictions <- function(predictions) {
  predictions <- as.matrix(predictions)
  if (!is.numeric(predictions)) {
    stop("predictions must be numeric.")
  }
  if (ncol(predictions) <= 1L || nrow(predictions) == 0L) return(predictions)
  # Increasing rearrangement is the row-wise empirical quantile function.  It
  # preserves the multiset of fitted values while enforcing Q(tau_j) <=
  # Q(tau_k) whenever tau_j < tau_k.
  t(apply(predictions, 1L, sort, na.last = TRUE))
}

#' Generate and optionally evaluate predictions from a fitted model
#'
#' @param test_data Data frame containing model predictors and, when evaluated,
#'   the response column.
#' @param taus Numeric vector of quantile levels.
#' @param covariates Character vector naming predictor columns.
#' @param response Character scalar naming the response column.
#' @param obj A fitted model object supporting [stats::predict()].
#' @param make_evaluation Logical; calculate WIS when `TRUE`.
#' @return A tibble containing the input prediction rows, fitted quantiles,
#'   model metadata, and optionally WIS.
#' @importFrom stats predict coef
#' @export
get_prediction <- function(test_data, taus, covariates, response, obj,
                           make_evaluation = TRUE) {
  # Ensure model object exists before prediction
  if (is.null(obj)) {
    stop("Model not found. Ensure the model is trained or loaded before prediction.")
  }

  #start_time <- Sys.time()
  # Generate predictions
  y_hat_all <- predict(obj, newx = as.matrix(test_data[covariates]))
  y_hat_all <- matrix(as.numeric(y_hat_all), nrow= dim(test_data)[1])
  if (length(taus) > 1L) {
    tau_order <- order(taus)
    ordered_predictions <- rearrange_quantile_predictions(y_hat_all[, tau_order, drop = FALSE])
    y_hat_all[, tau_order] <- ordered_predictions
  }
  test_data[paste0("predicted_tau", as.character(taus))] = y_hat_all #+ test_data[["log_value_7dav"]]

  # Perform evaluation if requested
  if (make_evaluation) {
    if (!(response %in% colnames(test_data))) {
      test_data[[response]] <- NULL
    }
    test_data <-evaluate(test_data, taus, response=response)
  }

  #end_time <- Sys.time()
  #elapsed_time <- end_time - start_time

  #coef_list = c(paste(covariates, '_coef', sep=''))
  #coef_matrix = t(as.matrix(coef(obj)))
  #coef_combined_result = data.frame(tau=taus, geo_value=geo, test_lag_group=test_lag_group,
  #                                  training_end_date=training_end_date,
  #                                  training_start_date=training_start_date,
  #                                  lambda=lambda, gamma=gamma)
  #coef_combined_result[c("intercept", coef_list)] = coef_matrix
  #coef_combined_result[coef_list] = coef_matrix

  # Add metadata columns to test data
  test_data$gamma <- attr(obj, "gamma")
  test_data$lambda <- attr(obj, "lambda")
  test_data$model_backend <- attr(obj, "model_backend")

  return(tibble::as_tibble(test_data))
}

#' Weighted interval score for a single observation
#'
#' Inlined from the evalcast package.
#'
#' @param taus Numeric vector of quantile levels.
#' @param residuals Numeric vector of (quantile_prediction - actual) values.
#' @param point_pred Unused; kept for interface compatibility.
#' @keywords internal
weighted_interval_score <- function(taus, residuals, point_pred) {
  # Twice the mean pinball loss.  In particular, when `taus = 0.5`,
  # this reduces exactly to absolute error and is symmetric in the sign of
  # the residual.  This matches evalcast::weighted_interval_score(), which
  # was used to produce the released paper results.
  mean(2 * pmax(
    taus * (-residuals),
    (1 - taus) * residuals
  ), na.rm = TRUE)
}

#' Evaluation of the test results based on WIS score
#'
#' @param test_data dataframe with a column containing the prediction results of
#'    each requested quantile. Each row represents an update with certain
#'    (reference_date, report_date, location) combination.
#' @template taus-template
#'
#' @export
evaluate <- function(test_data, taus, response) {
  n_row <- nrow(test_data)
  taus_list <- as.list(data.frame(matrix(replicate(n_row, taus), ncol=n_row)))
  pred_cols <- paste0("predicted_tau", taus)

  # Calculate WIS
  predicted_all <- as.matrix(test_data[, pred_cols])
  predicted_trans <- as.list(data.frame(t(predicted_all - test_data[[response]])))
  test_data$wis <- mapply(weighted_interval_score, taus_list, predicted_trans, 0)

  return (test_data)
}

#' Un-log predicted values
#'
#' Inverts the response transformation used during preprocessing. Counts and
#' fractions supplied as one value column use `log(value + 1)` and therefore
#' require `exp(prediction) - 1`. Fractions supplied as numerator and
#' denominator columns use `log(numerator + 1) - log(denominator + 1)` and
#' therefore require `exp(prediction)`.
#'
#' @param test_data dataframe with a column containing the prediction results of
#'    each requested quantile. Each row represents an update with certain
#'    (reference_date, report_date, location) combination.
#' @template taus-template
#' @param value_type Either `"count"` or `"fraction"`.
#' @param fraction_input For fraction data, either `"value"` when preprocessing
#'   received one fraction column or `"numerator_denominator"` when it received
#'   separate numerator and denominator columns. Ignored for counts.
#'
#' @importFrom dplyr bind_cols select starts_with
exponentiate_preds <- function(
    test_data, taus, value_type = c("count", "fraction"),
    fraction_input = c("value", "numerator_denominator")) {
  value_type <- match.arg(value_type)
  fraction_input <- match.arg(fraction_input)
  pred_cols <- paste0("predicted_tau", taus)
  predictions <- exp(test_data[, pred_cols])
  if (value_type == "count" || fraction_input == "value") {
    predictions <- predictions - 1
  }

  # Drop original predictions and join on exponentiated versions
  test_data <- bind_cols(
    select(test_data, -starts_with("predicted")),
    predictions
  )

  return(test_data)
}

#' Retrieve or Train a Model
#'
#' This function retrieves a cached model if available or trains a new quantile regression model if necessary.
#' The trained model is saved with additional metadata attributes.
#'
#' @param model_path Path to the cached model file.
#' @param train_data Data frame containing the training data.
#' @param response Name of the response variable.
#' @param tau Quantile to be predicted (between 0 and 1).
#' @param sqrt_max_raw Maximum raw value at square root level.
#' @template gamma-template
#' @template lambda-template
#' @template covariates-template
#' @template lp_solver-template
#' @template train_models-template
#' @param model_backend Quantile-regression backend. `"quantreg"` is the
#'   default; `"quantgen"` preserves the previous implementation.
#' @param time_limit Optional solver time limit for the `quantgen` backend.

#' @return The trained or loaded model object.
#'
#' @importFrom stringr str_interp
get_model <- function(model_path, train_data, covariates, response, tau,
                      sqrt_max_raw, kept_bins,
                      lambda, gamma, lp_solver, train_models,
                      model_backend = c("quantreg", "quantgen"),
                      time_limit = NULL) {
  model_backend <- match.arg(model_backend)
  if (train_models || !file.exists(model_path)) {
    if (!train_models && !file.exists(model_path)) {
      warning(str_interp("user requested use of cached model but file ${model_path} does not exist; training new model"))
    }
    # Quantile regression
    vec_7dav <- train_data[["value_7dav_diff"]]
    vec_slope <- train_data[["value_slope_diff"]]
    normalize_difference <- function(x) {
      span <- max(x) - min(x)
      if (!is.finite(span) || span <= 0) return(rep(0, length(x)))
      (x - min(x)) / span
    }
    if (!is.null(vec_7dav) && !is.null(vec_slope)) {
      normalized_7dav_diff <- normalize_difference(vec_7dav)
      normalized_slope_diff <- normalize_difference(vec_slope)
      weights <- exp(-gamma * normalized_7dav_diff * normalized_slope_diff)
    } else if (!is.null(vec_7dav)) {
      normalized_7dav_diff <- normalize_difference(vec_7dav)
      weights <- exp(-gamma * normalized_7dav_diff)
    } else if (!is.null(vec_slope)) {
      normalized_slope_diff <- normalize_difference(vec_slope)
      weights <- exp(-gamma * normalized_slope_diff)
    } else {
      weights <- NULL
    }
    if (model_backend == "quantreg") {
      obj <- fit_quantreg_lasso(
        as.matrix(train_data[covariates]), train_data[[response]],
        taus = tau, lambda = lambda, case_weights = weights,
        standardize = TRUE
      )
    } else {
      if (!requireNamespace("quantgen", quietly = TRUE)) {
        stop("The optional 'quantgen' package is required only when model_backend='quantgen'. ",
             "Install quantgen or use the default model_backend='quantreg'.")
      }
      quantgen_args <- list(
        as.matrix(train_data[covariates]), train_data[[response]],
        tau = tau, lambda = lambda, standardize = TRUE,
        lp_solver = lp_solver, intercept = TRUE, weights = weights
      )
      if (!is.null(time_limit)) quantgen_args$time_limit <- time_limit
      obj <- do.call(quantgen::quantile_lasso, quantgen_args)
    }

    # Save model to cache.
    create_dir_not_exist(dirname(model_path))
    # add extra infomation
    attr(obj, "sqrt_max_raw") <- sqrt_max_raw
    attr(obj, "kept_bins") <- kept_bins
    attr(obj, "gamma") <- gamma
    attr(obj, "lambda") <- lambda
    attr(obj, "lp_solver") <- lp_solver
    attr(obj, "model_backend") <- model_backend
    saveRDS(obj, file=model_path)
  } else {
    # Load model from cache invisibly. Object has the same name as the original
    # model object, `obj`.
    message(str_interp("Loading from ${model_path}"))
    obj <- readRDS(model_path)
    cached_backend <- attr(obj, "model_backend")
    if (is.null(cached_backend)) {
      # Models written before backend metadata existed used quantgen.
      cached_backend <- "quantgen"
      attr(obj, "model_backend") <- cached_backend
    }
    if (!identical(cached_backend, model_backend)) {
      stop("Cached model backend is ", cached_backend,
           " but the requested backend is ", model_backend, ".")
    }
  }

  return(obj)
}

#' Fit weighted lasso quantile regressions with quantreg
#'
#' `quantreg` ignores its ordinary `weights` argument for penalized fits. This
#' adapter implements case-weighted check loss exactly by multiplying each
#' standardized design row (including the intercept) and its response by the
#' non-negative case weight before calling [quantreg::rq.fit.lasso()]. Since
#' the check loss is positively homogeneous,
#' `rho_tau(w * residual) = w * rho_tau(residual)` for `w >= 0`.
#' Linearly dependent predictor columns are omitted during fitting and assigned
#' zero coefficients in the returned model so that prediction still accepts the
#' original predictor matrix.
#'
#' @param x Numeric predictor matrix.
#' @param y Numeric response vector.
#' @param taus Quantile levels to fit.
#' @param lambda Non-negative lasso penalty. The intercept is unpenalized.
#' @param case_weights Optional non-negative observation weights.
#' @param standardize Whether to center and scale predictors before fitting.
#' @return A `delphirf_quantreg_lasso` object.
#' @export
fit_quantreg_lasso <- function(x, y, taus, lambda = 0.1,
                               case_weights = NULL, standardize = TRUE) {
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  y <- as.numeric(y)
  taus <- as.numeric(taus)
  if (nrow(x) != length(y)) stop("x and y have incompatible lengths.")
  if (!nrow(x) || !ncol(x)) stop("x must contain rows and predictors.")
  if (any(!is.finite(x)) || any(!is.finite(y))) {
    stop("x and y must be finite.")
  }
  if (!length(taus) || any(!is.finite(taus)) || any(taus <= 0 | taus >= 1)) {
    stop("taus must lie strictly between zero and one.")
  }
  if (length(lambda) != 1L || !is.finite(lambda) || lambda < 0) {
    stop("lambda must be one finite non-negative value.")
  }
  if (is.null(case_weights)) case_weights <- rep(1, nrow(x))
  case_weights <- as.numeric(case_weights)
  if (length(case_weights) != nrow(x) || any(!is.finite(case_weights)) ||
      any(case_weights < 0)) {
    stop("case_weights must be finite, non-negative, and match x rows.")
  }
  keep <- case_weights > 0
  if (!any(keep)) stop("At least one case weight must be positive.")
  x <- x[keep, , drop = FALSE]
  y <- y[keep]
  case_weights <- case_weights[keep]

  centers <- if (standardize) colMeans(x) else rep(0, ncol(x))
  scales <- if (standardize) apply(x, 2L, stats::sd) else rep(1, ncol(x))
  scales[!is.finite(scales) | scales <= sqrt(.Machine$double.eps)] <- 1
  standardized_x <- sweep(sweep(x, 2L, centers, "-"), 2L, scales, "/")
  design <- cbind(`(Intercept)` = 1, standardized_x)

  # rq.fit.lasso requires a full-rank design. Retain one column from each
  # linearly dependent set for fitting, then restore zero coefficients for the
  # omitted columns so prediction continues to accept the original matrix.
  design_qr <- qr(design, tol = sqrt(.Machine$double.eps), LAPACK = FALSE)
  active_columns <- sort(design_qr$pivot[seq_len(design_qr$rank)])
  dropped_columns <- setdiff(seq_len(ncol(design)), active_columns)
  fit_design <- design[, active_columns, drop = FALSE]

  # For non-negative w, rho_tau(w * (y - x beta)) equals
  # w * rho_tau(y - x beta). Scaling both y and the complete design row is
  # therefore an exact weighted-loss transformation; the lasso pseudo-rows
  # constructed inside rq.fit.lasso remain unweighted.
  weighted_design <- fit_design * case_weights
  weighted_y <- y * case_weights
  penalty <- c(0, rep(lambda, ncol(x)))[active_columns]
  fits <- lapply(taus, function(tau) {
    quantreg::rq.fit.lasso(
      weighted_design, weighted_y, tau = tau, lambda = penalty
    )
  })
  active_coefficients <- vapply(
    fits, function(fit) as.numeric(fit$coefficients),
    numeric(length(active_columns))
  )
  coefficients <- matrix(0, nrow = ncol(design), ncol = length(taus))
  coefficients[active_columns, ] <- active_coefficients
  rownames(coefficients) <- colnames(design)
  colnames(coefficients) <- paste0("tau", taus)
  structure(
    list(coefficients = coefficients, taus = taus,
         centers = centers, scales = scales,
         predictor_names = colnames(x), fits = fits,
         training_weights = case_weights,
         dropped_predictors = colnames(design)[dropped_columns]),
    class = "delphirf_quantreg_lasso"
  )
}

#' @export
predict.delphirf_quantreg_lasso <- function(object, newx, ...) {
  newx <- as.matrix(newx)
  storage.mode(newx) <- "double"
  if (ncol(newx) != length(object$centers)) {
    stop("newx does not have the fitted number of predictors.")
  }
  standardized_x <- sweep(
    sweep(newx, 2L, object$centers, "-"), 2L, object$scales, "/"
  )
  prediction <- cbind(`(Intercept)` = 1, standardized_x) %*%
    object$coefficients
  if (length(object$taus) == 1L) as.numeric(prediction) else prediction
}

#' @export
coef.delphirf_quantreg_lasso <- function(object, ...) {
  if (length(object$taus) == 1L) {
    stats::setNames(as.numeric(object$coefficients),
                    rownames(object$coefficients))
  } else {
    object$coefficients
  }
}

#' Construct filename for model with given parameters
#'
#' @template indicator-template
#' @template signal-template
#' @template geo-template
#' @template signal_suffix-template
#' @template lambda-template
#' @template gamma-template
#' @template value_type-template
#' @template test_lag_group-template
#' @param tau Decimal quantile to be predicted. Values must be between 0 and 1.
#' @param model_mode Boolean, indicates whether the file name is for a model.
#' @template training_end_date-template
#' @template training_days-template
#' @template model_save_dir-template
#'
#' @return Path to file containing model object.
#'
#' @importFrom stringr str_interp
#'
generate_filename <- function(indicator, signal,
                              geo_level, signal_suffix, lambda, gamma,
                              training_end_date, training_days=365, geo="",
                              value_type = "", test_lag_group="", tau="",
                              model_mode = TRUE,
                              model_save_dir=default_model_save_dir()) {
  if (lambda != "") {
    lambda <- str_interp("lambda${lambda}")
  }
  if (gamma!= "") {
    gamma <- str_interp("gamma${gamma}")
  }
  if (test_lag_group != "") {
    test_lag_group <- str_interp("lag${test_lag_group}")
  }
  if (is.numeric(training_days)) {
    training_days <- str_interp("tw${as.character(training_days)}")
  }
  if (tau != "") {
    tau <- str_interp("tau${tau}")
  }
  if (model_mode) {
    file_type <- ".rds"
  } else {
    file_type <- ".csv.gz"
  }
  training_end_date <- tryCatch(
    format(as.Date(training_end_date), "%Y%m%d"),
    error = function(e) {
      # Fallback action if formatting fails
      ""
    }
  )
  folder_components <- c(indicator, signal, signal_suffix, value_type, geo_level)
  filename_components <- c(training_end_date, geo, test_lag_group,
                           tau, training_days, lambda, gamma)

  foldername <- paste(folder_components[folder_components != ""], collapse="_")

  filename <- paste0(
    # Drop any empty strings.
    paste(filename_components[filename_components != ""], collapse="_"),
    file_type
  )
  return(file.path(model_save_dir, foldername, filename))
}


#' Create Parameters List
#'
#' This function generates a list of parameter names based on the provided training data and lag list.
#' It dynamically constructs parameter names using predefined constants and incorporates log lag adjustments
#' when multiple lag values exist.
#'
#' @param train_data Data frame containing training data, including lag values.
#' @param lagged_term_list Numeric vector specifying the list of lags to be considered.
#' @param temporal_resol Character; either "daily" or "weekly" resolution.
#' @details For daily data, all seven weekday indicators remain in the
#'   preprocessed data, while the model parameter list omits `Sun_ref` and
#'   `Sun_issue` to provide identifiable baseline categories.
#'
#' @export
#'
#' @importFrom dplyr mutate select
#'
create_params_list <- function(train_data, lagged_term_list, temporal_resol) {
  params_list <- c(
    WEEK_ISSUES[1],
    Y7DAV,
    paste0("log_value_7dav_lag", lagged_term_list),
    paste0("log_delta_value_7dav_lag", lagged_term_list)
  )
  if (length(unique(train_data$lag)) > 1) {
    params_list <- c(params_list, LOG_LAG)
  }

  modeled_weekdays <- setdiff(WEEKDAYS_ABBR, "Sun")
  weekday_params <- c(
    paste0(modeled_weekdays, "_ref"),
    paste0(modeled_weekdays, "_issue")
  )

  if (temporal_resol == "daily") c(params_list, weekday_params) else params_list
}
