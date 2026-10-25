# Convert a user-supplied vector to Date and give failures a useful context.
as_delphirf_date <- function(x, argument) {
  out <- tryCatch(as.Date(x), error = function(e) rep(as.Date(NA), length(x)))
  if (length(out) != length(x) || anyNA(out)) {
    stop(argument, " must contain valid, non-missing dates.", call. = FALSE)
  }
  out
}

infer_temporal_resolution <- function(reference_date) {
  dates <- sort(unique(reference_date))
  gaps <- unique(as.numeric(diff(dates)))
  if (length(gaps) && all(gaps %% 7 == 0)) "weekly" else "daily"
}

validate_nowcast_dots <- function(dots) {
  reserved <- intersect(
    names(dots),
    c("df", "testing_start_date", "working_scale", "geo", "value_type",
      "temporal_resol", "model_save_dir", "training_days", "genuine_training",
      "genuine_testing", "model_backend", "time_limit")
  )
  if (length(reserved)) {
    stop("Do not pass wrapper-controlled argument(s) through ...: ",
         paste(reserved, collapse = ", "), call. = FALSE)
  }
  invisible(dots)
}

bind_nowcast_results <- function(results, dropped) {
  out <- tibble::as_tibble(dplyr::bind_rows(results))
  attr(out, "dropped_test_rows") <- as.data.frame(dplyr::bind_rows(dropped))
  out
}

#' Produce an operational DelphiRF nowcast from a revision archive
#'
#' This is the main user-facing entry point for one forecast origin. It accepts
#' a raw revision archive, preprocesses each location independently, fits the
#' DelphiRF models, and returns forecasts
#' on the original reporting scale. Rows reported after `as_of` are removed
#' before preprocessing, so they cannot affect an historical nowcast.
#'
#' @param data A raw revision archive containing one row per observed release.
#' @param as_of Forecast origin. The default is the latest report date in
#'   `data`.
#' @param value_col One observed-value column for counts or directly supplied
#'   fractions, or two columns containing a fraction numerator and denominator.
#' @param refd_col Name of the reference-date column.
#' @param lag_col Name of the required reporting-lag column, in whole days.
#' @param ref_lag Target lag, in days, defining the completed value to nowcast.
#' @param geo_col Optional location column. When present, locations are
#'   preprocessed and modeled independently. When absent, the archive is
#'   treated as one series.
#' @param suffixes Suffixes passed to [data_preprocessing()].
#' @param lagged_term_list Optional positive feature lags, in days.
#' @param value_type Either `"count"` or `"fraction"`.
#' @param temporal_resol `"daily"`, `"weekly"`, or `"auto"`. Automatic
#'   detection chooses weekly only when all distinct reference-date gaps are
#'   multiples of seven days.
#' @param smoothed Whether the supplied value is already smoothed.
#' @param smoothed_target Whether to model the prepared seven-day-average
#'   target. By default this follows `smoothed`.
#' @param model_save_dir Directory used for fitted-model caching.
#' @param training_days Length of the training window in days.
#' @param genuine_training Whether to fit only on usable raw observations.
#' @param genuine_testing Whether to forecast only usable raw observations.
#'   The default is `FALSE` so operational nowcasts include rows created while
#'   completing the current reporting triangle.
#' @param model_backend Quantile-regression backend.
#' @param time_limit Optional solver time limit for the `quantgen` backend.
#' @param ... Additional modeling arguments accepted by
#'   [delphirf_nowcast_preprocessed()], such as `taus`, `test_lag_groups`,
#'   `lambda`, `gamma`, `lag_pad`, and `params_list`. Arguments controlled by
#'   this wrapper cannot be supplied through `...`.
#'
#' @return A tibble of forecasts. `forecast_date` identifies the forecast
#'   origin. If `geo_col` is present, that column is restored in the output.
#'   Quantile columns named `predicted_tau*` are on the reporting scale.
#' @export
delphirf_nowcast <- function(
    data, as_of = NULL, value_col = "value", refd_col = "reference_date",
    lag_col = "lag", ref_lag = REF_LAG,
    geo_col = "geo_value", suffixes = "", lagged_term_list = NULL,
    value_type = c("count", "fraction"),
    temporal_resol = c("auto", "daily", "weekly"), smoothed = FALSE,
    smoothed_target = smoothed, model_save_dir = default_model_save_dir(),
    training_days = TRAINING_DAYS, genuine_training = TRUE,
    genuine_testing = FALSE, model_backend = c("quantreg", "quantgen"),
    time_limit = NULL, ...) {
  value_type <- match.arg(value_type)
  temporal_resol <- match.arg(temporal_resol)
  model_backend <- match.arg(model_backend)
  dots <- list(...)
  validate_nowcast_dots(dots)
  if (!is.data.frame(data)) stop("data must be a data frame.", call. = FALSE)
  required <- unique(c(value_col, refd_col, lag_col))
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("data is missing required column(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  reference_date <- as_delphirf_date(data[[refd_col]], refd_col)
  lag <- suppressWarnings(as.numeric(data[[lag_col]]))
  if (anyNA(lag) || any(!is.finite(lag)) || any(lag < 0) ||
      any(lag != floor(lag))) {
    stop(lag_col, " must contain non-negative whole numbers of days.",
         call. = FALSE)
  }
  data[[lag_col]] <- lag
  report_date <- reference_date + lag

  if (is.null(as_of)) as_of <- max(report_date)
  as_of <- as_delphirf_date(as_of, "as_of")
  if (length(as_of) != 1L) stop("as_of must be one date.", call. = FALSE)

  known <- report_date <= as_of
  data <- data[known, , drop = FALSE]
  if (!nrow(data)) stop("No releases were reported on or before as_of.", call. = FALSE)
  reference_date <- reference_date[known]
  resolution <- if (temporal_resol == "auto") {
    infer_temporal_resolution(reference_date)
  } else {
    temporal_resol
  }
  working_scale <- if (value_type == "fraction" && length(value_col) >= 2L) {
    "log_ratio"
  } else {
    "log_value"
  }

  has_geo <- !is.null(geo_col) && geo_col %in% names(data)
  if (has_geo && anyNA(data[[geo_col]])) {
    stop(geo_col, " must not contain missing locations.", call. = FALSE)
  }
  rows_by_geo <- if (has_geo) {
    split(seq_len(nrow(data)), as.character(data[[geo_col]]), drop = TRUE)
  } else {
    list(all = seq_len(nrow(data)))
  }
  results <- list()
  dropped <- list()
  for (geo_name in names(rows_by_geo)) {
    rows <- rows_by_geo[[geo_name]]
    prepared <- data_preprocessing(
      data[rows, , drop = FALSE], value_col = value_col, refd_col = refd_col,
      lag_col = lag_col, ref_lag = ref_lag, suffixes = suffixes,
      lagged_term_list = lagged_term_list, value_type = value_type,
      temporal_resol = resolution, smoothed = smoothed,
      target_as_of_date = as_of)
    if (!nrow(prepared)) next
    call_args <- c(list(
      df = prepared, testing_start_date = as_of,
      smoothed_target = smoothed_target,
      lagged_term_list = lagged_term_list, temporal_resol = resolution,
      geo = geo_name, value_type = value_type, working_scale = working_scale,
      model_save_dir = model_save_dir, training_days = training_days,
      genuine_training = genuine_training, genuine_testing = genuine_testing,
      model_backend = model_backend, time_limit = time_limit), dots)
    result <- do.call(delphirf_nowcast_preprocessed, call_args)
    geo_dropped <- attr(result, "dropped_test_rows")
    result <- result[result$report_date == as_of, , drop = FALSE]
    if (nrow(result)) {
      result$forecast_date <- as_of
      if (has_geo) result[[geo_col]] <- data[[geo_col]][rows[1L]]
      results[[length(results) + 1L]] <- result
    }
    if (!is.null(geo_dropped) && nrow(geo_dropped)) {
      geo_dropped <- geo_dropped[
        geo_dropped$report_date == as_of, , drop = FALSE]
      if (nrow(geo_dropped)) {
        geo_dropped$forecast_date <- as_of
        if (has_geo) geo_dropped[[geo_col]] <- data[[geo_col]][rows[1L]]
        dropped[[length(dropped) + 1L]] <- geo_dropped
      }
    }
  }
  bind_nowcast_results(results, dropped)
}

#' Produce pseudoprospective DelphiRF nowcasts at multiple origins
#'
#' The complete archive is preprocessed once per location and reused at every
#' requested origin. [delphirf_nowcast_preprocessed()] still restricts model
#' training, weight anchors, and origin-date predictors to information available
#' by each `forecast_date`. Later releases supply retrospective target values
#' for evaluation but do not enter an earlier fit or its predictors.
#'
#' @inheritParams delphirf_nowcast
#' @param test_dates Dates at which to issue pseudoprospective nowcasts.
#' @param model_save_dir Directory used for fitted-model caching. One directory
#'   is shared across origins, allowing compatible fitted models to be reused.
#'
#' @return A tibble containing the forecasts from all requested origins, with
#'   the origin in `forecast_date`. Origins are run in increasing date order.
#'   When the complete archive contains a target matured after an origin, the
#'   corresponding retrospective WIS values are included.
#' @export
delphirf_backtest <- function(
    data, test_dates, value_col = "value", refd_col = "reference_date",
    lag_col = "lag", ref_lag = REF_LAG,
    geo_col = "geo_value", suffixes = "", lagged_term_list = NULL,
    value_type = c("count", "fraction"),
    temporal_resol = c("auto", "daily", "weekly"), smoothed = FALSE,
    smoothed_target = smoothed, model_save_dir = default_model_save_dir(),
    training_days = TRAINING_DAYS, genuine_training = TRUE,
    genuine_testing = FALSE, model_backend = c("quantreg", "quantgen"),
    time_limit = NULL, ...) {
  value_type <- match.arg(value_type)
  temporal_resol <- match.arg(temporal_resol)
  model_backend <- match.arg(model_backend)
  dots <- list(...)
  validate_nowcast_dots(dots)
  test_dates <- as_delphirf_date(test_dates, "test_dates")
  if (!length(test_dates)) stop("test_dates must contain at least one date.",
                                call. = FALSE)
  test_dates <- sort(unique(test_dates))

  if (!is.data.frame(data)) stop("data must be a data frame.", call. = FALSE)
  required <- unique(c(value_col, refd_col, lag_col))
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("data is missing required column(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  reference_date <- as_delphirf_date(data[[refd_col]], refd_col)
  lag <- suppressWarnings(as.numeric(data[[lag_col]]))
  if (anyNA(lag) || any(!is.finite(lag)) || any(lag < 0) ||
      any(lag != floor(lag))) {
    stop(lag_col, " must contain non-negative whole numbers of days.",
         call. = FALSE)
  }
  data[[lag_col]] <- lag
  report_date <- reference_date + lag
  archive_end <- max(report_date)
  if (any(test_dates > archive_end)) {
    stop("test_dates cannot be later than the latest report date in data (",
         archive_end, ").", call. = FALSE)
  }
  resolution <- if (temporal_resol == "auto") {
    infer_temporal_resolution(reference_date)
  } else {
    temporal_resol
  }
  working_scale <- if (value_type == "fraction" && length(value_col) >= 2L) {
    "log_ratio"
  } else {
    "log_value"
  }
  has_geo <- !is.null(geo_col) && geo_col %in% names(data)
  if (has_geo && anyNA(data[[geo_col]])) {
    stop(geo_col, " must not contain missing locations.", call. = FALSE)
  }
  rows_by_geo <- if (has_geo) {
    split(seq_len(nrow(data)), as.character(data[[geo_col]]), drop = TRUE)
  } else {
    list(all = seq_len(nrow(data)))
  }
  results <- list()
  dropped <- list()
  for (geo_name in names(rows_by_geo)) {
    rows <- rows_by_geo[[geo_name]]
    prepared <- data_preprocessing(
      data[rows, , drop = FALSE], value_col = value_col, refd_col = refd_col,
      lag_col = lag_col, ref_lag = ref_lag, suffixes = suffixes,
      lagged_term_list = lagged_term_list, value_type = value_type,
      temporal_resol = resolution, smoothed = smoothed,
      target_as_of_date = archive_end)
    if (!nrow(prepared)) next
    for (i in seq_along(test_dates)) {
      test_date <- test_dates[[i]]
      call_args <- c(list(
        df = prepared, testing_start_date = test_date,
        smoothed_target = smoothed_target,
        lagged_term_list = lagged_term_list, temporal_resol = resolution,
        geo = geo_name, value_type = value_type, working_scale = working_scale,
        model_save_dir = model_save_dir, training_days = training_days,
        genuine_training = genuine_training,
        genuine_testing = genuine_testing, model_backend = model_backend,
        time_limit = time_limit), dots)
      result <- do.call(delphirf_nowcast_preprocessed, call_args)
      geo_dropped <- attr(result, "dropped_test_rows")
      result <- result[result$report_date == test_date, , drop = FALSE]
      if (nrow(result)) {
        result$forecast_date <- test_date
        if (has_geo) result[[geo_col]] <- data[[geo_col]][rows[1L]]
        results[[length(results) + 1L]] <- result
      }
      if (!is.null(geo_dropped) && nrow(geo_dropped)) {
        geo_dropped <- geo_dropped[
          geo_dropped$report_date == test_date, , drop = FALSE]
        if (nrow(geo_dropped)) {
          geo_dropped$forecast_date <- test_date
          if (has_geo) geo_dropped[[geo_col]] <- data[[geo_col]][rows[1L]]
          dropped[[length(dropped) + 1L]] <- geo_dropped
        }
      }
    }
  }
  bind_nowcast_results(results, dropped)
}
