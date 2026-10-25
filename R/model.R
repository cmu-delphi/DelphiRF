# Validate quantile levels once and preserve the first occurrence of each one.
normalize_taus <- function(taus) {
  taus <- as.numeric(taus)
  if (!length(taus) || any(!is.finite(taus)) || any(taus <= 0 | taus >= 1)) {
    stop("taus must lie strictly between zero and one.")
  }
  unique(taus)
}

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
#' @return The rows whose reporting lag falls in the padded lag interval.
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
#' @param taus Numeric vector of quantile levels. Duplicate levels are removed
#'   while preserving first-occurrence order.
#' @param covariates Character vector naming predictor columns.
#' @param response Character scalar naming the response column.
#' @param obj A fitted model object supporting [stats::predict()].
#' @param make_evaluation Logical; when `TRUE`, add `wis_working`, the WIS on
#'   the model's working (log) scale.
#' @return A tibble containing the input prediction rows, fitted quantiles,
#'   model metadata, and optionally `wis_working`.
#' @importFrom stats predict coef
#' @export
get_prediction <- function(test_data, taus, covariates, response, obj,
                           make_evaluation = TRUE) {
  taus <- normalize_taus(taus)
  # Ensure model object exists before prediction
  if (is.null(obj)) {
    stop("Model not found. Ensure the model is trained or loaded before prediction.")
  }

  # Generate predictions
  y_hat_all <- predict(obj, newx = as.matrix(test_data[covariates]))
  y_hat_all <- matrix(as.numeric(y_hat_all), nrow= dim(test_data)[1])
  if (length(taus) > 1L) {
    tau_order <- order(taus)
    ordered_predictions <- rearrange_quantile_predictions(y_hat_all[, tau_order, drop = FALSE])
    y_hat_all[, tau_order] <- ordered_predictions
  }
  test_data[paste0("predicted_tau", as.character(taus))] = y_hat_all

  # Perform evaluation if requested
  if (make_evaluation) {
    if (!(response %in% colnames(test_data))) {
      stop("Cannot evaluate predictions: response column '", response,
           "' is missing from test_data.")
    }
    test_data <- evaluate(test_data, taus, response = response, column = "wis_working")
  }

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
#' @keywords internal
weighted_interval_score <- function(taus, residuals) {
  # Twice the mean pinball loss.  In particular, when `taus = 0.5`,
  # this reduces exactly to absolute error and is symmetric in the sign of
  # the residual.  This matches evalcast::weighted_interval_score(), which
  # was used to produce the released paper results.
  mean(2 * pmax(
    taus * (-residuals),
    (1 - taus) * residuals
  ), na.rm = TRUE)
}

# Row-wise weighted interval score: twice the mean pinball loss over `taus`,
# the same quantity as weighted_interval_score() for each row.
interval_scores <- function(predictions, truth, taus) {
  predictions <- as.matrix(predictions)
  if (!nrow(predictions)) return(numeric(0))
  residuals <- predictions - as.numeric(truth)
  loss <- 2 * pmax(sweep(-residuals, 2L, taus, "*"), sweep(residuals, 2L, 1 - taus, "*"))
  scores <- rowMeans(loss, na.rm = TRUE)
  scores[is.nan(scores)] <- NA_real_
  scores
}

#' Evaluation of the test results based on WIS score
#'
#' @param test_data dataframe with a column containing the prediction results of
#'    each requested quantile. Each row represents an update with certain
#'    (reference_date, report_date, location) combination.
#' @template taus-template
#' @param response Character scalar naming the observed response column.
#' @param column Name of the WIS column to add.
#' @return `test_data` with a numeric WIS column added, computed on whatever
#'   scale `predicted_tau*` and `response` share.
#'
#' @export
evaluate <- function(test_data, taus, response, column = "wis") {
  taus <- normalize_taus(taus)
  pred_cols <- paste0("predicted_tau", taus)
  test_data[[column]] <- interval_scores(test_data[pred_cols], test_data[[response]], taus)
  test_data
}

# Return working-scale values to the reporting scale.
to_reporting_scale <- function(x, working_scale) {
  if (working_scale == "log_value") exp(x) - 1 else exp(x)
}

# Add `wis_reporting`: the WIS of the reporting-scale predictions against the
# target on the reporting scale. Expects exponentiate_preds() to have run.
add_reporting_wis <- function(test_data, taus, response, working_scale) {
  if (!response %in% names(test_data)) return(test_data)
  truth <- to_reporting_scale(test_data[[response]], working_scale)
  test_data$wis_reporting <- interval_scores(
    test_data[paste0("predicted_tau", taus)], truth, taus)
  test_data
}

# Working scales produced by data_preprocessing(). The user names one
# explicitly; there is no default because the inverse transformation differs.
WORKING_SCALES <- c("log_value", "log_ratio")

check_working_scale <- function(working_scale) {
  if (!is.character(working_scale) || length(working_scale) != 1L ||
      !working_scale %in% WORKING_SCALES) {
    stop("working_scale must be \"log_value\" (one value column, modelled as ",
         "log(value + 1)) or \"log_ratio\" (numerator and denominator columns, ",
         "modelled as log(numerator + 1) - log(denominator + 1)).")
  }
  working_scale
}

#' Un-log predicted values
#'
#' Inverts the response transformation used during preprocessing. Unsuffixed
#' `predicted_tau*` columns are returned on the reporting scale, while exact
#' model-scale values are retained in paired `predicted_tau*_working` columns.
#'
#' @param test_data dataframe with a column containing the prediction results of
#'    each requested quantile. Each row represents an update with certain
#'    (reference_date, report_date, location) combination.
#' @template taus-template
#' @template working_scale-template
#' @return `test_data` with `predicted_tau*` on the reporting scale and the
#'   model-scale values in `predicted_tau*_working`.
#'
#' @export
exponentiate_preds <- function(test_data, taus, working_scale) {
  taus <- normalize_taus(taus)
  working_scale <- check_working_scale(working_scale)
  pred_cols <- paste0("predicted_tau", taus)
  missing <- setdiff(pred_cols, names(test_data))
  if (length(missing)) {
    stop("Missing prediction columns: ", paste(missing, collapse = ", "))
  }
  working <- test_data[, pred_cols, drop = FALSE]
  names(working) <- paste0(pred_cols, "_working")
  test_data[pred_cols] <- to_reporting_scale(test_data[, pred_cols, drop = FALSE],
                                             working_scale)
  test_data[names(working)] <- working
  return(test_data)
}

# Version of the feature construction a cached model depends on. Increase it
# whenever data_preprocessing() or create_params_list() changes what a
# predictor column means, so models fitted under the old meaning are rejected.
FEATURE_SCHEMA_VERSION <- 2L

observation_weights <- function(train_data, gamma) {
  normalize_difference <- function(x) {
    span <- max(x) - min(x)
    if (!is.finite(span) || span <= 0) return(rep(0, length(x)))
    (x - min(x)) / span
  }
  vec_7dav <- train_data[["value_7dav_diff"]]
  vec_slope <- train_data[["value_slope_diff"]]
  if (!is.null(vec_7dav) && !is.null(vec_slope)) {
    exp(-gamma * normalize_difference(vec_7dav) * normalize_difference(vec_slope))
  } else if (!is.null(vec_7dav)) {
    exp(-gamma * normalize_difference(vec_7dav))
  } else if (!is.null(vec_slope)) {
    exp(-gamma * normalize_difference(vec_slope))
  } else {
    NULL
  }
}

# Write a model to a temporary file beside its destination, then rename it,
# so a reader never sees a partly written model.
save_model_atomically <- function(obj, model_path) {
  create_dir_not_exist(dirname(model_path))
  temporary <- tempfile(paste0(".", basename(model_path), "."), tmpdir = dirname(model_path))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(obj, file = temporary)
  if (!file.rename(temporary, model_path)) {
    stop("Could not move the fitted model into place: ", model_path, call. = FALSE)
  }
  invisible(model_path)
}

# Stop unless a cached model was fitted under exactly the requested contract.
check_fit_contract <- function(obj, contract, model_path) {
  cached <- attr(obj, "fit_contract")
  if (is.null(cached)) {
    stop("Cached model ", model_path, " has no fitting contract (it was saved by ",
         "an older DelphiRF); refit it with train_models = TRUE.", call. = FALSE)
  }
  fields <- union(names(cached), names(contract))
  differing <- fields[!vapply(fields, function(field) {
    identical(cached[[field]], contract[[field]])
  }, logical(1))]
  if (length(differing)) {
    stop("Cached model ", model_path, " was fitted under a different contract; ",
         "differing field(s): ", paste(differing, collapse = ", "),
         ". Refit it with train_models = TRUE.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Retrieve or Train a Model
#'
#' This function retrieves a cached model if available or trains a new quantile regression model if necessary.
#' The trained model is saved with its `gamma`, `lambda`, `lp_solver`,
#' `model_backend` and `fit_contract` as attributes.
#'
#' Observation weights are `exp(-gamma * d)`, where `d` is the min-max
#' normalized `value_7dav_diff` multiplied by the normalized
#' `value_slope_diff` (see [add_weights_related()]); a column that is absent is
#' left out of the product.
#'
#' The fitting contract records everything the fitted coefficients depend on:
#' the response, the ordered predictors, `taus`, `lambda`, `gamma`, the
#' backend (and LP solver for `quantgen`), the feature-schema version, a
#' fingerprint of the training predictors, response and weights, and the
#' caller's `fit_contract` entries (such as the training cutoff and target
#' lag). The first eight characters of the contract's hash are added to the
#' file name (`..._c3f9a2b1.rds`), so every distinct fit has its own file and
#' a load finds exactly the matching one. The stored contract is checked again
#' on loading, and loading stops with the differing fields if it does not
#' match. Models are written atomically: to a temporary file in the same
#' folder, then renamed.
#'
#' @param model_path Path to the cached model file, before the contract hash
#'   is added.
#' @param train_data Data frame containing the training data.
#' @param response Name of the response variable.
#' @param taus Numeric vector of quantile levels between zero and one. Duplicate
#'   levels are removed while preserving first-occurrence order.
#' @template gamma-template
#' @template lambda-template
#' @template covariates-template
#' @template lp_solver-template
#' @template train_models-template
#' @param model_backend Quantile-regression backend. `"quantreg"` is the
#'   default; `"quantgen"` preserves the previous implementation.
#' @param time_limit Optional solver time limit for the `quantgen` backend.
#' @param fit_contract Named list of further settings the model depends on,
#'   added to the contract described above.
#' @return The trained or loaded model object, with its file in the
#'   `model_path` attribute.
#'
#' @importFrom stringr str_interp
get_model <- function(model_path, train_data, covariates, response, taus,
                      lambda, gamma, lp_solver, train_models,
                      model_backend = c("quantreg", "quantgen"),
                      time_limit = NULL, fit_contract = list()) {
  model_backend <- match.arg(model_backend)
  x <- as.matrix(train_data[covariates])
  storage.mode(x) <- "double"
  y <- as.numeric(train_data[[response]])
  weights <- observation_weights(train_data, gamma)
  contract <- c(fit_contract, list(
    response = response, covariates = as.character(covariates),
    taus = as.numeric(taus), lambda = as.numeric(lambda), gamma = as.numeric(gamma),
    model_backend = model_backend,
    lp_solver = if (model_backend == "quantgen") lp_solver else NA_character_,
    feature_schema = FEATURE_SCHEMA_VERSION,
    data_fingerprint = rlang::hash(list(x = unname(x), y = y, weights = weights))
  ))
  model_path <- sub("\\.rds$", paste0("_c", substr(rlang::hash(contract), 1L, 8L), ".rds"),
                    model_path)

  if (train_models || !file.exists(model_path)) {
    if (!train_models && !file.exists(model_path)) {
      warning(str_interp("user requested use of cached model but file ${model_path} does not exist; training new model"))
    }
    if (model_backend == "quantreg") {
      obj <- fit_quantreg_lasso(
        x, y, taus = taus, lambda = lambda, case_weights = weights,
        standardize = TRUE
      )
    } else {
      if (!requireNamespace("quantgen", quietly = TRUE)) {
        stop("The optional 'quantgen' package is required only when model_backend='quantgen'. ",
             "Install quantgen or use the default model_backend='quantreg'.")
      }
      quantgen_args <- list(
        x, y, tau = taus, lambda = lambda, standardize = TRUE,
        lp_solver = lp_solver, intercept = TRUE, weights = weights
      )
      if (!is.null(time_limit)) quantgen_args$time_limit <- time_limit
      obj <- do.call(quantgen::quantile_lasso, quantgen_args)
    }

    attr(obj, "gamma") <- gamma
    attr(obj, "lambda") <- lambda
    attr(obj, "lp_solver") <- lp_solver
    attr(obj, "model_backend") <- model_backend
    attr(obj, "fit_contract") <- contract
    save_model_atomically(obj, model_path)
  } else {
    message(str_interp("Loading from ${model_path}"))
    obj <- readRDS(model_path)
    check_fit_contract(obj, contract, model_path)
  }
  attr(obj, "model_path") <- model_path

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
#' @param taus Quantile levels to fit. Duplicate levels are removed while
#'   preserving first-occurrence order.
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
  taus <- normalize_taus(taus)
  if (nrow(x) != length(y)) stop("x and y have incompatible lengths.")
  if (!nrow(x) || !ncol(x)) stop("x must contain rows and predictors.")
  if (any(!is.finite(x)) || any(!is.finite(y))) {
    stop("x and y must be finite.")
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

#' Predict using a fitted DelphiRF quantile-lasso model
#'
#' @param object A fitted `delphirf_quantreg_lasso` object.
#' @param newx A matrix or data frame of predictor values. When it has column
#'   names, they must match the fitted predictors and are reordered to the
#'   training order before prediction. An unnamed matrix is interpreted by
#'   position.
#' @param ... Additional arguments, currently ignored.
#' @return A numeric vector or matrix of quantile predictions.
#' @export
predict.delphirf_quantreg_lasso <- function(object, newx, ...) {
  predictor_names <- object$predictor_names
  supplied_names <- colnames(newx)
  if (length(predictor_names) && !is.null(supplied_names)) {
    missing_predictors <- setdiff(predictor_names, supplied_names)
    unexpected_predictors <- setdiff(supplied_names, predictor_names)
    duplicated_predictors <- unique(supplied_names[duplicated(supplied_names)])
    if (length(missing_predictors) || length(unexpected_predictors) ||
        length(duplicated_predictors)) {
      problems <- c(
        if (length(missing_predictors)) {
          paste0("missing: ", paste(missing_predictors, collapse = ", "))
        },
        if (length(unexpected_predictors)) {
          paste0("unexpected: ", paste(unexpected_predictors, collapse = ", "))
        },
        if (length(duplicated_predictors)) {
          paste0("duplicated: ", paste(duplicated_predictors, collapse = ", "))
        }
      )
      stop("newx predictor mismatch (", paste(problems, collapse = "; "), ").",
           call. = FALSE)
    }
    newx <- newx[, predictor_names, drop = FALSE]
  }
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

#' Extract coefficients from a fitted DelphiRF quantile-lasso model
#'
#' @param object A fitted `delphirf_quantreg_lasso` object.
#' @param ... Additional arguments, currently ignored.
#' @return A named coefficient vector or coefficient matrix.
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
#' @template geo_level-template
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


#' Create a Predictor-Name Vector
#'
#' This function generates a character vector of predictor names from the
#' training data and requested lag values. It dynamically constructs the
#' feature names and adds the inverse-lag predictor when the data contain more
#' than one reporting lag.
#'
#' @param train_data Data frame containing training data, including lag values.
#' @param lagged_term_list Numeric vector specifying the lags to be considered.
#' @param temporal_resol Character; either "daily" or "weekly" resolution.
#' @details For daily data, all seven weekday indicators remain in the
#'   preprocessed data, while the model parameter list omits `Sun_ref` and
#'   `Sun_issue` to provide identifiable baseline categories.
#'
#' @return A character vector naming the predictor columns.
#' @export
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
