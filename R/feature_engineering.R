#' Add One-Hot Encoding for Day of the Week Based on Reference and Issue Dates
#'
#' This function adds one-hot encoded columns for the day of the week based on a specified date column.
#' Each unique day in the `wd` vector gets a corresponding binary column indicating whether the date falls on that day.
#'
#' @param df A data frame containing the date column to be encoded.
#' @param time_col A string specifying the column name that contains the date information.
#' @param suffix A string suffix to distinguish the encoded columns (e.g., "_ref" for reference dates).
#' @param wd A character vector specifying the days of the week. Defaults to
#'   all seven stable English abbreviations in `WEEKDAYS_ABBR`.
#'
#' @return A data frame with additional columns representing one-hot encoded days of the week.
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @export
add_dayofweek <- function(df, time_col, suffix, wd = WEEKDAYS_ABBR) {
  # Ensure the time column is in Date format
  df[[time_col]] <- as.Date(df[[time_col]])

  # Convert numeric ISO weekdays to the package's stable abbreviations. This
  # avoids locale-dependent labels and preserves the historical "Thurs" name.
  dayofweek <- WEEKDAYS_ABBR[as.integer(format(df[[time_col]], format = "%u"))]

  # Generate one-hot encoded columns
  for (i in seq_along(wd)) {
    df[[paste0(wd[i], suffix)]] <- as.numeric(dayofweek == wd[i])
  }
  return (df)
}


#' Add one-hot encoding for week of the month based on issue date
#'
#' This function calculates the week of the month for each date in the specified
#' column and adds corresponding one-hot encoded columns.
#'
#' @param df Data frame containing the date column.
#' @param time_col Name of the column containing date values.
#' @param wm Character vector naming indicator columns. The default creates
#'   `W1_issue`, `W2_issue`, and `W3_issue`; week 4 is the baseline category.
#'
#' @details
#' - The function first determines the week of the month for each date in `time_col`.
#' - It then creates one-hot encoded columns representing each week in `wm`.
#' - If a month has 5 or 6 weeks, the last week is merged with the first week.
#'
#' @return A modified data frame with additional one-hot encoded columns for the
#'         week of the month.
#'
#' @export
add_weekofmonth <- function(df, time_col, wm = WEEK_ISSUES) {
  weekofmonth <- get_weekofmonth(df[[time_col]])
  for (i in seq_along(wm)) {
    df[[paste0(wm[i])]] <- as.numeric(weekofmonth == i)
  }
  return (df)
}

#' Add 7-day moving average (7dav) to the dataset
#'
#' This function calculates the 7-day moving average (`7dav`) for a given value column,
#' grouped by `report_date`, which is determined using the reference date and lag column.
#'
#' @param df A data frame containing time series data.
#' @param value_col The name of the column containing values for which the 7-day moving average is computed.
#' @param refd_col The name of the column representing the reference date.
#' @param lag_col The name of the column indicating the lag in days.
#'
#' @details
#' - The function first computes `report_date` as `refd_col + lag_col`.
#' - It then calculates the trailing **7-day moving average** (`7dav`) over
#'   reference dates within each `report_date`, ensuring proper ordering of
#'   reference dates.
#' - The first six reference dates in each `report_date` group have no
#'   complete 7-day window, so their `value_7dav` is `NA`. After
#'   [fill_missing_updates()] these are the first six reference dates of the
#'   series, at every lag.
#'
#' @importFrom dplyr group_by arrange mutate ungroup
#' @importFrom zoo rollapply
#'
#' @return A modified data frame with an additional `value_7dav` column.
#'
#' @export
add_7davs <- function(df, value_col, refd_col, lag_col) {
  if (!all(c(value_col, refd_col, lag_col) %in% names(df))) {
    stop("One or more specified columns do not exist in the dataframe.")
  }
  # Calculate the 7-day moving average grouped by report_date
  df <- df %>%
    dplyr::mutate(report_date = !!sym(refd_col) + !!sym(lag_col)) %>%
    dplyr::group_by(report_date) %>%
    dplyr::arrange(!!sym(refd_col)) %>%
    dplyr::mutate(value_7dav = rollapply(!!sym(value_col), 7, mean, fill = NA, align = "right")) %>%
    ungroup()
  return (as.data.frame(df))
}

# Lag terms are reference-date gaps in days: positive whole numbers, and
# whole weeks for weekly data, where reference dates are 7 days apart.
check_lagged_term_list <- function(lagged_term_list, temporal_resol) {
  if (!temporal_resol %in% c("weekly", "daily")) {
    stop("Invalid temporal_resol value. Choose 'weekly' or 'daily'.")
  }
  if (!is.numeric(lagged_term_list) || !length(lagged_term_list) ||
      any(!is.finite(lagged_term_list)) || any(lagged_term_list <= 0) ||
      any(lagged_term_list != round(lagged_term_list))) {
    stop("lagged_term_list must contain positive whole numbers of days, e.g. c(1, 7) ",
         "for daily data or c(7, 14) for weekly data.", call. = FALSE)
  }
  if (temporal_resol == "weekly" && any(lagged_term_list %% 7 != 0)) {
    stop("For weekly data, lagged_term_list must be whole weeks in days (multiples of 7, ",
         "e.g. c(7, 14)); got ", paste(lagged_term_list, collapse = ", "), ".", call. = FALSE)
  }
  invisible(TRUE)
}

#' Add Lagged Terms Based on Temporal Resolution
#'
#' This function creates lagged versions of a specified value column based on
#' user-defined time steps and temporal resolution (daily or weekly).
#'
#' @param df Data frame containing time-series data.
#' @param value_col Character. Name of the column containing values to be lagged.
#' @param refd_col Character. Name of the reference date column.
#' @param lag_col Character. Name of the column containing lag values.
#' @param lagged_term_list Numeric vector. List of lag values to be applied. Default is `1:7`.
#' @param temporal_resol Character. Temporal resolution, either `"weekly"` or `"daily"`.
#'                       Default is `"weekly"`. Determines how lag shifts are applied.
#' @param start_lag Numeric, data frame, or `NULL`. A numeric value applies one
#'   initial-report lag, \eqn{l_{min}}, to every episode. A data frame supplies
#'   episode-specific initial lags and must contain the columns named by
#'   `refd_col` and `lag_col`, with one starting lag per reference date.
#'   `NULL` (default) uses the smallest lag present in `df`.
#'
#' @details
#' - The revision magnitude for lag `k` is
#'   `log(value_lagk + 1) - log(value_start_lagk + 1)`, where the start value is
#'   the value of the source episode at its corresponding `start_lag`.
#' - If `temporal_resol = "weekly"`, lag values are divided by 7 to match weekly steps.
#' - If `temporal_resol = "daily"`, lag values are applied directly.
#' - The function groups data by `report_date`, sorts it by `refd_col`, and applies lags.
#' - For each lag `X`, the function creates `<value_col>_lagX`,
#'   `<value_col>_start_lagX`, and `log_delta_<value_col>_lagX`.
#'
#' @return A modified data frame with additional lagged columns.
#'
#' @importFrom dplyr %>% group_by arrange mutate across ungroup
#' @importFrom rlang sym set_names
#' @export
add_lagged_terms <- function(df, value_col, refd_col, lag_col, lagged_term_list=1:7, temporal_resol = "weekly",
                             start_lag = NULL) {
  check_lagged_term_list(lagged_term_list, temporal_resol)

  # Set time step based on resolution
  timestep <- ifelse(temporal_resol == "weekly", 7, 1)

  df$report_date <- df[[refd_col]] + df[[lag_col]]

  df <- df %>%
    group_by(report_date) %>%  # Group by report_date
    dplyr::arrange(!!sym(refd_col), .by_group = TRUE) %>%  # Use refd_col for sorting
    dplyr::mutate(dplyr::across(
      .cols = !!sym(value_col),
      .fns = list(
        !!!rlang::set_names(
          lapply(lagged_term_list, function(x) ~ dplyr::lag(., x / timestep)),  # Apply lags, its the lag for timestep
          paste0("lag", lagged_term_list )  # Dynamically name shifted columns
        )
      )
    )) %>%
    ungroup()

  # ``start_lag`` may be one explicit lag shared by every episode or a
  # two-column lookup giving each reference date's first raw reporting lag.
  # The lookup keeps later-starting episodes anchored to their observed initial
  # report rather than to zeros introduced by grid completion.
  if (is.data.frame(start_lag)) {
    start_lookup <- start_lag[, c(refd_col, lag_col), drop = FALSE]
    start_lookup <- start_lookup[!is.na(start_lookup[[refd_col]]) &
                                   !is.na(start_lookup[[lag_col]]), , drop = FALSE]
    start_lookup <- stats::aggregate(start_lookup[[lag_col]],
                                     by = list(start_lookup[[refd_col]]), FUN = min)
    names(start_lookup) <- c(refd_col, lag_col)
    start_df <- merge(df, start_lookup, by = c(refd_col, lag_col))
    start_df <- start_df[, c(refd_col, value_col), drop = FALSE]
  } else {
    min_lag <- if (is.null(start_lag)) min(df[[lag_col]]) else start_lag
    start_df <- df[df[[lag_col]] == min_lag, c(refd_col, value_col)]
  }
  for (lag in lagged_term_list) {
    # Select and adjust the previous lagged data
    start_df_prev <- start_df
    start_df_prev[[refd_col]] <- start_df_prev[[refd_col]] + lag  # Adjust reference column
    colnames(start_df_prev)[colnames(start_df_prev) == value_col] <- paste0(value_col, "_start_lag", lag)

    # Merge back to original dataframe, ensuring distinct rows
    df <- merge(df,
                start_df_prev %>% distinct(.keep_all = TRUE),
                by=refd_col, all.x=TRUE)

    # Compute log difference, ensuring safety in calculations
    start_col <- paste0(value_col, "_start_lag", lag)
    current_col <- paste0(value_col, "_lag", lag)
    df[[paste0("log_delta_", value_col, "_lag", lag)]] <- log(df[[current_col]]+1) - log(df[[start_col]] + 1)
  }
  return (as.data.frame(df))
}

#' Construct one target record per reference date
#'
#' For a requested target lag `L`, the target matures on
#' `reference_date + L`. Its value is the latest raw report available at or
#' before lag `L`; an exact release at lag `L` is not required. The requested
#' target lag and maturity date are fixed across episodes, while the source
#' report lag is retained separately for auditing. The returned table is sparse
#' (one row per reference date) and is intended to be joined after feature-grid
#' filling.
#'
#' @param df Raw reporting-triangle data frame.
#' @param value_col Name or names of the raw value columns. These columns are
#'   retained while constructing the raw report history used for target lookup.
#' @param refd_col Name of the reference-date column.
#' @param lag_col Name of the day-based reporting-lag column.
#' @param ref_lag Requested target lag, in days. One value applies to the whole
#'   dataset.
#' @param temporal_resol Either `"daily"` or `"weekly"`.
#' @param target_as_of_date Optional date limiting target selection to reports
#'   available on or before that date. Targets whose maturity dates fall after
#'   this date are unavailable. When `NULL`, the maximum raw report date is
#'   used.
#' @return A compact data frame with one target record per reference date,
#'   including the requested `target_lag` and fixed `target_date`. Before that
#'   date is available, `target_source_lag` and `target_value_source` are
#'   missing; afterward they record the observed source lag and either
#'   `"exact_release"` or `"carried_forward"`. If an episode has no report at
#'   or before `ref_lag`, its target remains unavailable after maturity.
#' @export
create_target_lookup <- function(df, value_col, refd_col, lag_col, ref_lag,
                                 temporal_resol = "daily",
                                 target_as_of_date = NULL) {
  empty_target_lookup <- function() {
    data.frame(reference_date = as.Date(character()),
               target_date = as.Date(character()),
               target_lag = numeric(), target_source_lag = numeric(),
               target_value_source = character())
  }
  if (nrow(df) == 0) {
    return(empty_target_lookup())
  }
  raw <- df[, unique(c(refd_col, lag_col, value_col)), drop = FALSE]
  # A missing value represents no observed update. For a ratio, both the
  # numerator and denominator are required for the report to be usable.
  raw <- raw[stats::complete.cases(raw[, value_col, drop = FALSE]), , drop = FALSE]
  if (nrow(raw) == 0L) {
    return(empty_target_lookup())
  }
  raw[[refd_col]] <- as.Date(raw[[refd_col]])
  raw$report_date <- raw[[refd_col]] + raw[[lag_col]]
  if (temporal_resol == "weekly") {
    raw <- normalize_weekly_observations(raw, refd_col, lag_col)
  } else if (temporal_resol != "daily") {
    stop("Invalid temporal_resol. Choose either 'daily' or 'weekly'.")
  }
  valid <- !is.na(raw[[refd_col]]) & !is.na(raw[[lag_col]]) &
    is.finite(raw[[lag_col]]) & raw[[lag_col]] >= 0 &
    !is.na(raw$report_date)
  raw <- raw[valid, , drop = FALSE]
  if (nrow(raw) == 0L) {
    return(empty_target_lookup())
  }
  raw <- raw[order(raw[[refd_col]], raw[[lag_col]], raw$report_date), , drop = FALSE]
  if (is.null(target_as_of_date)) {
    target_as_of_date <- max(raw$report_date, na.rm = TRUE)
  }
  target_as_of_date <- as.Date(target_as_of_date)

  chosen <- lapply(split(raw, raw[[refd_col]]), function(g) {
    maturity_date <- as.Date(g[[refd_col]][1]) + ref_lag
    candidates <- which(g[[lag_col]] <= ref_lag)
    # The maturity date is part of the target contract even when it lies after
    # the archive's as-of date. In that case the target value and its source
    # audit fields remain unavailable, but consumers can still apply the
    # authoritative ``target_date <= as_of_date`` rule.
    if (maturity_date > target_as_of_date || length(candidates) == 0) {
      return(data.frame(
        reference_date = as.Date(g[[refd_col]][1]),
        target_date = maturity_date,
        target_lag = as.numeric(ref_lag),
        target_source_lag = NA_real_,
        target_value_source = NA_character_
      ))
    }
    idx <- utils::tail(candidates, 1)
    source_lag <- as.numeric(g[[lag_col]][idx])
    data.frame(
      reference_date = as.Date(g[[refd_col]][idx]),
      target_date = maturity_date,
      target_lag = as.numeric(ref_lag),
      target_source_lag = source_lag,
      target_value_source = if (source_lag == ref_lag) {
        "exact_release"
      } else {
        "carried_forward"
      }
    )
  })
  result <- dplyr::bind_rows(chosen)
  if (nrow(result) == 0) {
    return(empty_target_lookup())
  }
  result
}

attach_target_lookup <- function(df, target_lookup) {
  target_values <- df %>%
    select(reference_date, target_date = report_date,
           value_target = value_raw, value_target_7dav = value_7dav)
  attached <- df %>%
    left_join(target_lookup, by = "reference_date") %>%
    left_join(target_values, by = c("reference_date", "target_date"))
  unavailable <- is.na(attached$target_source_lag)
  attached$value_target[unavailable] <- NA_real_
  attached$value_target_7dav[unavailable] <- NA_real_
  attached
}

# Build a sparse lookup of usable observations in the raw archive. A row must
# contain every value required by the working scale; unchanged usable reports
# are retained, while raw-but-missing and grid-created rows are not.
create_genuine_event_lookup <- function(df, value_col, refd_col, lag_col,
                                        temporal_resol = "daily") {
  if (nrow(df) == 0L) {
    return(data.frame(
      reference_date = as.Date(character()),
      report_date = as.Date(character()),
      genuine_event = logical()
    ))
  }

  raw <- df[, unique(c(refd_col, lag_col, value_col)), drop = FALSE]
  # Raw rows without every value needed for the working scale are missing
  # updates, not genuine observations. Unchanged non-missing reports remain.
  raw <- raw[stats::complete.cases(raw[, value_col, drop = FALSE]), , drop = FALSE]
  if (nrow(raw) == 0L) {
    return(data.frame(
      reference_date = as.Date(character()),
      report_date = as.Date(character()),
      genuine_event = logical()
    ))
  }
  raw[[refd_col]] <- as.Date(raw[[refd_col]])
  raw$report_date <- raw[[refd_col]] + raw[[lag_col]]
  if (temporal_resol == "weekly") {
    raw <- normalize_weekly_observations(raw, refd_col, lag_col)
  } else if (temporal_resol != "daily") {
    stop("Invalid temporal_resol. Choose either 'daily' or 'weekly'.")
  }
  raw <- raw[!is.na(raw[[refd_col]]) & !is.na(raw$report_date) &
               !is.na(raw[[lag_col]]) & raw[[lag_col]] >= 0, , drop = FALSE]
  raw <- raw[order(raw[[refd_col]], raw$report_date), , drop = FALSE]
  if (nrow(raw) == 0L) {
    return(data.frame(
      reference_date = as.Date(character()),
      report_date = as.Date(character()),
      genuine_event = logical()
    ))
  }

  events <- data.frame(
    reference_date = as.Date(raw[[refd_col]]),
    report_date = as.Date(raw$report_date),
    genuine_event = TRUE
  )
  events <- events[!duplicated(events[c("reference_date", "report_date")]), , drop = FALSE]
  rownames(events) <- NULL
  events
}


#' Add log-transformed columns for specified numerical variables
#'
#' This function applies a log transformation to specified value-related columns
#' by adding 1 to avoid issues with zero values.
#'
#' @param df Data frame containing the numerical columns to be transformed.
#' @param lagged_term_list A vector of integers specifying the lagged columns to transform.
#'
#' @details
#' - The function creates log-transformed versions of the following columns:
#'   `"value_raw"`, `"value_7dav"`, `"value_target"`, `"value_target_7dav"`,
#'   and the lagged columns specified in `lagged_term_list` (e.g., `"value_7dav_lag1"`, `"value_7dav_lag2"`, etc.).
#' - The transformation used is `log(value + 1)`, which ensures stability when values are zero.
#'
#' @return A modified data frame with additional log-transformed columns.
#'
#' @export
add_log_transformed <- function(df, lagged_term_list) {
  cols <- c(c("value_raw", "value_7dav", "value_target", "value_target_7dav"),
            paste0("value_7dav_lag", lagged_term_list))

  # Apply log transformation
  for (col in cols) {
    if (col %in% colnames(df)) {  # Ensure the column exists in df before applying transformation
      df[[paste0("log_", col)]] <- log(df[[col]] + 1)
    }
  }
  return (as.data.frame(df))
}

#' Add date-related parameters to the dataset
#'
#' This function adds features related to the reference date and issue date,
#' including day-of-week and week-of-month effects.
#'
#' @param df Data frame containing the date columns.
#' @param refd_col Column name representing the reference date.
#' @param lag_col Column name representing the lag between the reference and issue date.
#' @param temporal_resol A string indicating the temporal resolution ("daily" or "weekly").
#'                       Defaults to "daily".
#' @details
#' - If `temporal_resol` is "daily", all seven one-hot encoded weekday
#'   columns are added for both `refd_col` and `"report_date"`.
#' - One-hot encoded week-of-month columns are added for `"report_date"` in all cases.
#'
#' @return A modified data frame with additional date-related feature columns.
#'
#' @export
add_params_for_dates <- function(df, refd_col, lag_col, temporal_resol = "daily") {
  df$report_date <- df[[refd_col]] + df[[lag_col]]
  if (temporal_resol == "daily") {
    df <- add_dayofweek(df, refd_col, "_ref", WEEKDAYS_ABBR)
    df <- add_dayofweek(df, "report_date", "_issue", WEEKDAYS_ABBR)
  }
  df <- add_weekofmonth(df, "report_date", WEEK_ISSUES)
  return(as.data.frame(df))
}

#' Data Preprocessing Function
#'
#' This function processes input data by handling missing values, computing lagged terms,
#' generating smoothed values, and transforming data for further analysis.
#' It supports both count and fraction data types and ensures consistency in column naming.
#' If two value column names are provided and the data type is "fraction", then the first
#' value column is treated as the column for numerators while the second column is treated as
#' the column for denominators.
#'
#' @param df Data frame containing the input data.
#' @param value_col Character vector specifying the column(s) containing values.
#' @param refd_col Name of the raw reference-date column.
#' @param lag_col Name of the raw reporting-lag column, measured in days.
#' @param ref_lag Target lag \eqn{L}, in days. The target of each reference
#'   date is its latest raw report at or before lag \eqn{L}.
#' @param suffixes Suffixes used to distinguish numerator and denominator
#'   feature columns when two value columns are supplied.
#' @param lagged_term_list Reference-date gaps, in days, for the lagged value
#'   and revision-magnitude features (see [add_lagged_terms()]). When `NULL`,
#'   `c(1, 7)` for daily data and `c(7, 14)` for weekly data.
#' @param value_type Character indicating the type of values ('count' or 'fraction').
#' @param temporal_resol Character specifying temporal resolution ('daily' or 'weekly').
#' @param smoothed Logical indicating that supplied values are already smoothed.
#'   When `TRUE`, preprocessing skips its seven-day moving average.
#' @param target_as_of_date Optional date limiting target construction to
#'   reports available on or before that date. When `NULL`, the maximum raw
#'   report date is used.
#' @param revision_start_lag Numeric, data frame, or `NULL`. A numeric lag
#'   applies one common initial report to every episode. A lookup data frame
#'   supplies the initial lag separately by reference date. `NULL` (default)
#'   builds that lookup internally from each episode's first genuine raw report,
#'   so later-starting episodes are not compared with zeros introduced by grid
#'   completion.
#' @details Lag 7 is always added to `lagged_term_list`, because weekly-change
#'   features are required downstream. Output rows have `lag < ref_lag`, and
#'   raw date and lag columns are normalized to `reference_date`, `report_date`,
#'   and `lag`. Internally the reporting grid keeps lags up to `ref_lag + 100`
#'   days (more if a lag term needs it), which covers every 7-day average and
#'   lag term of the returned rows. Rows are ordered by `reference_date`, then
#'   `report_date`.
#'
#'   The returned data includes a logical `genuine_event` column. `TRUE`
#'   identifies raw reports with a usable observed value, including unchanged
#'   repeated reports. A fraction report is usable only when both its numerator
#'   and denominator are present. Raw rows with missing required values, rows
#'   created by grid completion, and carry-forward rows are `FALSE`. Episodes
#'   with no usable raw report are excluded before grid completion. Within each
#'   retained episode, filling is performed before genuine-only training or
#'   testing filters are applied, so completed values can contribute to
#'   seven-day averages and lagged features. For two-column fractions,
#'   numerator and denominator are filled component-wise: a partial raw report
#'   is non-genuine, but its observed component can affect later constructed
#'   features. [DelphiRF()] can independently retain only genuine rows for
#'   training and testing. Counts and fractions
#'   supplied as one value column use `log(value + 1)`. Fractions supplied as
#'   numerator and denominator columns use
#'   `log(numerator + 1) - log(denominator + 1)`. For daily data, preprocessing
#'   retains all seven weekday indicators for reference and report dates;
#'   automatic model construction omits Sunday from each set as the baseline.
#' @return A tibble containing the prepared reporting triangle and model
#'   features.
#'
#' @importFrom dplyr full_join left_join distinct
#' @importFrom english english
#'
#' @export
data_preprocessing <- function(df, value_col, refd_col, lag_col, ref_lag,
                               suffixes=c(""), lagged_term_list = NULL, value_type="count",
                               temporal_resol="daily", smoothed=FALSE,
                               target_as_of_date = NULL,
                               revision_start_lag = NULL) {
  if (value_type == "count") {
    if (length(value_col) > 1) warning("Multiple value column names provided; only the first one will be used.")
    if (length(unique(suffixes)) > 1) warning("Multiple suffixes provided; only the first one will be used.")
    value_col <- value_col[1]
    suffixes <- suffixes[1]
  } else if (value_type == "fraction") {
    if (length(value_col) > 2) {
      warning("More than two value column names provided; only the first two will be used as numerator and denominator.")
      value_col <- value_col[1:2]
    } else if (length(value_col) == 1) {
      suffixes <- c("")
    }
    if (length(unique(suffixes)) > length(value_col)) {
      warning(sprintf("More than two suffixes provided; only the first %s will be used.", as.character(english(length(value_col)))))
      suffixes <- suffixes[1:length(value_col)]
    }
    if (length(unique(suffixes)) < length(value_col)) {
      warning("Insufficient suffixes provided for numerator and denominator; default suffixes '_num' and '_denom' will be used.")
      suffixes <- c("_num", "_denom")
    }
  }

  # Check lagged_term_list
  if (is.null(lagged_term_list)) {
    if (temporal_resol == "daily") {
      lagged_term_list <- c(1, 7)
    } else if (temporal_resol == "weekly") {
      lagged_term_list <- c(7, 14)
    } else {
      stop("Invalid temporal_resol value. Choose 'weekly' or 'daily'.")
    }
  }
  if (!7 %in% lagged_term_list) {
    lagged_term_list <- c(lagged_term_list, 7)
  } # Make sure we always have the lagged term from last week
  check_lagged_term_list(lagged_term_list, temporal_resol)
  if (temporal_resol == "weekly" && ref_lag %% 7 != 0) {
    warning(sprintf(paste0(
      "The target lag of %s days is not a whole number of weeks. Weekly reports ",
      "fall on 7-day steps, so no report lands exactly at the target lag and every ",
      "target will be missing; choose a target lag that is a multiple of 7."), ref_lag),
      call. = FALSE)
  }

  # Identify reference-date episodes from the raw archive before completing
  # the reporting grid. An episode with no row containing every required value
  # is excluded entirely. Missing rows within a usable episode remain in the
  # input so fill_missing_updates() can carry the preceding value forward.
  raw_reference_dates <- as.Date(df[[refd_col]])
  if (temporal_resol == "weekly") {
    raw_reference_dates <- raw_reference_dates +
      ((6L - as.POSIXlt(raw_reference_dates)$wday) %% 7L)
  }
  usable_rows <- stats::complete.cases(df[, value_col, drop = FALSE])
  eligible_reference_dates <- unique(raw_reference_dates[
    usable_rows & !is.na(raw_reference_dates)
  ])
  raw_episode_dates <- unique(raw_reference_dates[!is.na(raw_reference_dates)])
  excluded_reference_dates <- setdiff(raw_episode_dates, eligible_reference_dates)
  if (!length(eligible_reference_dates)) {
    return(tibble::tibble())
  }
  df <- df[
    !is.na(raw_reference_dates) & raw_reference_dates %in% eligible_reference_dates,
    , drop = FALSE
  ]

  genuine_event_lookup <- create_genuine_event_lookup(
    df, value_col, refd_col, lag_col, temporal_resol
  )
  # Revision magnitudes are measured against each episode's initial genuine
  # report. Reporting cadence can change between reference dates, so one
  # dataset-wide minimum lag would compare later-starting episodes with a zero
  # introduced by grid completion.
  if (is.null(revision_start_lag) && nrow(genuine_event_lookup) > 0) {
    revision_start_lag <- genuine_event_lookup
    revision_start_lag[[lag_col]] <- as.numeric(
      revision_start_lag$report_date - revision_start_lag$reference_date)
    revision_start_lag <- revision_start_lag[, c("reference_date", lag_col), drop = FALSE]
  }

  effective_target_as_of_date <- if (is.null(target_as_of_date)) {
    max(as.Date(df[[refd_col]]) + df[[lag_col]], na.rm = TRUE)
  } else {
    as.Date(target_as_of_date)
  }
  target_lookup <- create_target_lookup(
    df, value_col, refd_col, lag_col, ref_lag, temporal_resol,
    target_as_of_date = effective_target_as_of_date
  )

  # Only rows with lag < ref_lag are returned, but their 7-day averages and
  # lag terms read the same report date's earlier reference dates, at lags up
  # to ref_lag - 1 + max(lagged_term_list) + 6. The grid keeps a generous
  # ref_lag + 100 days (more if a lag term needs it) and drops larger lags.
  max_feature_lag <- ref_lag + max(100, max(lagged_term_list) + 6)

  dfList <- lapply(value_col, function(value_col) {
    filled_df <- fill_missing_updates(
      df, value_col, refd_col, lag_col, temporal_resol,
      max_report_override = effective_target_as_of_date,
      max_lag = max_feature_lag
    )
    if (!(smoothed) & (temporal_resol == "daily")){
      filled_df <- add_7davs(filled_df, "value_raw", "reference_date", "lag")
    } else {
      filled_df$value_7dav = filled_df$value_raw
    }
    filled_df <- add_lagged_terms(filled_df, "value_7dav", "reference_date", "lag", lagged_term_list,
                                  temporal_resol, start_lag = revision_start_lag)
    filled_df <- attach_target_lookup(filled_df, target_lookup)
    add_log_transformed(filled_df, lagged_term_list)
  })

  merged_df <- Reduce(
    function(x, y) full_join(
      x, y,
      by = c("reference_date", "report_date", "lag", "target_date",
             "target_lag", "target_source_lag", "target_value_source"),
      suffix = suffixes
    ),
    dfList
  )

  if (length(value_col) > 1) {
    cols <- colnames(merged_df)[grepl("value", colnames(merged_df), ignore.case = TRUE)]
    cols <- setdiff(cols, "target_value_source")
    pattern <- paste0("(", suffixes[1], "|", suffixes[2], ")$")
    clean_cols <- gsub(pattern, "", cols) # remove suffixes
    filtered_cols <- unique(clean_cols[grepl("log", clean_cols)])

    for (col in filtered_cols){
      col_clean <- sub("^log_", "", col)
      merged_df[[col]] <- merged_df[[paste0(col, suffixes[1])]] - merged_df[[paste0(col, suffixes[2])]]
      merged_df[[col_clean]] <- exp(merged_df[[col]])
    }
  }

  merged_df$inv_log_lag <- 1/(merged_df$lag + 1)
  merged_df <- add_params_for_dates(
    merged_df, "reference_date", "lag", temporal_resol
  )

  merged_df <- dplyr::left_join(
    merged_df, genuine_event_lookup,
    by = c("reference_date", "report_date")
  )
  merged_df$genuine_event <- !is.na(merged_df$genuine_event) &
    merged_df$genuine_event

  merged_df <- merged_df %>%
    filter(!(.data$reference_date %in% excluded_reference_dates)) %>%
    filter(.data$lag < ref_lag) %>%
    dplyr::arrange(.data$reference_date, .data$report_date)

  return(tibble::as_tibble(merged_df))
}


#' Build similarity-weight anchors for a forecast origin
#'
#' The observation weights of a model fit compare each training row with the
#' reporting context at the forecast origin. This function records that
#' context: for every reporting lag it keeps the latest row reported on or
#' before `forecast_origin`. All rows of `df` are eligible, including rows
#' carried forward by [fill_missing_updates()] and rows whose target has not
#' yet matured; only rows reported after the origin, and rows whose
#' `log_value_7dav` or `log_value_7dav_lag7` is missing, are excluded.
#'
#' @param df A data frame from [data_preprocessing()] with `report_date`,
#'   `lag`, `log_value_7dav` and `log_value_7dav_lag7`. Pass the complete data,
#'   not only the training rows.
#' @param forecast_origin Date (or character date) of the forecast origin.
#' @param by Optional character vector of further columns that identify a
#'   series, such as `"geo_value"` when `df` holds several locations; one
#'   anchor is kept per lag within each series.
#' @return A data frame with one row per lag (and `by` group): the `by`
#'   columns, `lag`, `ref_7dav_value` (the anchor's `log_value_7dav`) and
#'   `ref_slope_value` (its `log_value_7dav - log_value_7dav_lag7`).
#' @importFrom dplyr %>% group_by ungroup slice_max filter across all_of transmute
#' @export
create_weight_anchors <- function(df, forecast_origin, by = character()) {
  required <- c("report_date", "lag", "log_value_7dav", "log_value_7dav_lag7", by)
  missing_cols <- setdiff(required, colnames(df))
  if (length(missing_cols)) {
    stop("Missing column(s) for weight anchors: ", paste(missing_cols, collapse = ", "))
  }
  forecast_origin <- as.Date(forecast_origin)
  if (length(forecast_origin) != 1L || is.na(forecast_origin)) {
    stop("forecast_origin must be one valid date.")
  }
  df %>%
    as.data.frame() %>%
    filter(as.Date(.data$report_date) <= forecast_origin,
           is.finite(.data$log_value_7dav), is.finite(.data$log_value_7dav_lag7)) %>%
    group_by(across(all_of(c(by, "lag")))) %>%
    slice_max(.data$report_date, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(across(all_of(c(by, "lag"))),
              ref_7dav_value = .data$log_value_7dav,
              ref_slope_value = .data$log_value_7dav - .data$log_value_7dav_lag7) %>%
    as.data.frame()
}

#' Add Weight-Related Features to a Dataframe
#'
#' This function compares each row's log seven-day-average level and weekly log
#' change with the anchor for its reporting lag, as built by
#' [create_weight_anchors()] at the forecast origin. Apply it to training rows
#' only; test rows do not receive similarity weights. [get_model()] turns the
#' two distances into weights `exp(-gamma * level * trend)`, after min-max
#' normalizing each distance across the training rows of that model fit.
#'
#' @param df A data frame containing `log_value_7dav`, `log_value_7dav_lag7`,
#'   `lag` and any `by` columns used to build `anchors`.
#' @param anchors A data frame from [create_weight_anchors()]. Every column
#'   other than `ref_7dav_value` and `ref_slope_value` is a join key, so it
#'   must hold one anchor per key.
#' @return A data frame with `log_7dav_slope`, `ref_slope_value`,
#'   `ref_7dav_value`, `value_slope_diff`, and `value_7dav_diff` added.
#'   It is an error if a row with complete values has no anchor for its lag.
#' @importFrom dplyr %>% mutate left_join
#' @importFrom rlang .data
#' @export
add_weights_related <- function(df, anchors) {
  if (!all(c("log_value_7dav", "log_value_7dav_lag7", "lag") %in% colnames(df))) {
    stop("Missing required columns when adding weighted related features")
  }
  value_cols <- c("ref_7dav_value", "ref_slope_value")
  if (!is.data.frame(anchors) || !all(c("lag", value_cols) %in% names(anchors))) {
    stop("anchors must be a data frame from create_weight_anchors(), with lag, ",
         "ref_7dav_value and ref_slope_value columns.")
  }
  key <- setdiff(names(anchors), value_cols)
  missing_key <- setdiff(key, colnames(df))
  if (length(missing_key)) {
    stop("df lacks the anchor key column(s): ", paste(missing_key, collapse = ", "))
  }
  if (anyDuplicated(anchors[key])) {
    stop("anchors must contain one row per ", paste(key, collapse = " and "), ".")
  }

  combined_df <- as.data.frame(df) %>%
    dplyr::select(-dplyr::any_of(value_cols)) %>%
    mutate(
      log_7dav_slope = .data$log_value_7dav - .data$log_value_7dav_lag7
    ) %>%
    left_join(anchors, by = key) %>%
    mutate(
      value_slope_diff = abs(.data$log_7dav_slope - .data$ref_slope_value),
      value_7dav_diff = abs(.data$log_value_7dav - .data$ref_7dav_value)
    )

  unanchored <- is.na(combined_df$ref_7dav_value) &
    is.finite(combined_df$log_value_7dav) & is.finite(combined_df$log_value_7dav_lag7)
  if (any(unanchored)) {
    stop("No weight anchor for lag(s) ",
         paste(sort(unique(combined_df$lag[unanchored])), collapse = ", "),
         "; build anchors from the complete data available at the forecast origin.")
  }
  combined_df
}
