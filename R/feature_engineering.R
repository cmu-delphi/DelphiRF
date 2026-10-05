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
  df <- df %>% mutate({{ time_col }} := as.Date(.data[[time_col]]))

  # Convert numeric ISO weekdays to the package's stable abbreviations. This
  # avoids locale-dependent labels and preserves the historical "Thurs" name.
  dayofweek <- WEEKDAYS_ABBR[as.integer(format(df[[time_col]], format = "%u"))]

  # Generate one-hot encoded columns
  for (i in seq_along(wd)) {
    df[[paste0(wd[i], suffix)]] <- as.numeric(dayofweek == wd[i])
  }
  return (df)
}


#' Add grouped day-of-week one-hot columns
#'
#' Creates one binary column per group in `onehot_weekdays`. Each group is a
#' character vector of day abbreviations (from `WEEKDAYS_ABBR`); a row is 1 if
#' the date falls on any day in the group.  Column names are derived from the
#' list names when present, otherwise by concatenating the day abbreviations.
#'
#' @param df A data frame containing the date column.
#' @param time_col Name of the date column.
#' @param suffix Column name suffix (e.g. `"_ref"` or `"_issue"`).
#' @param onehot_weekdays Named or unnamed list of character vectors, each
#'   specifying a group of days (e.g. `list(Mon=c("Mon"), Weekends=c("Sat","Sun"))`).
#' @param drop_least_frequent If `TRUE` (default), omit the least frequent
#'   group when the supplied groups form an exhaustive seven-day partition,
#'   avoiding the dummy-variable trap. A non-exhaustive selection such as the
#'   default Monday/weekend groups is kept in full.
#'
#' @return `df` with one additional integer column per group.
#' @export
add_grouped_dayofweek <- function(df, time_col, suffix, onehot_weekdays,
                                  drop_least_frequent = TRUE) {
  df <- df %>% mutate({{ time_col }} := as.Date(.data[[time_col]]))
  dayofweek <- WEEKDAYS_ABBR[as.integer(format(df[[time_col]], format = "%u"))]
  group_names <- if (!is.null(names(onehot_weekdays))) {
    names(onehot_weekdays)
  } else {
    vapply(onehot_weekdays, function(grp) paste0(grp, collapse = ""), character(1))
  }
  members <- unlist(onehot_weekdays, use.names = FALSE)
  exhaustive_partition <- setequal(unique(members), WEEKDAYS_ABBR) &&
    !anyDuplicated(members)
  if (isTRUE(drop_least_frequent) && length(onehot_weekdays) > 1L &&
      exhaustive_partition) {
    counts <- vapply(onehot_weekdays, function(grp) sum(dayofweek %in% grp, na.rm = TRUE), numeric(1))
    keep <- seq_along(onehot_weekdays) != which.min(counts)
    attr(df, "dropped_weekday_group") <- group_names[which.min(counts)]
    onehot_weekdays <- onehot_weekdays[keep]
    group_names <- group_names[keep]
  }
  for (ii in seq_along(onehot_weekdays)) {
    df[[paste0(group_names[ii], suffix)]] <- as.integer(dayofweek %in% onehot_weekdays[[ii]])
  }
  df
}


#' Add one-hot encoding for week of the month based on issue date
#'
#' This function calculates the week of the month for each date in the specified
#' column and adds corresponding one-hot encoded columns.
#'
#' @param df Data frame containing the date column.
#' @param time_col Name of the column containing date values.
#' @param wm Character vector specifying week labels (e.g., "W1", "W2", "W3", "W4").
#'           Default is `WEEK_ISSUES`.
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
#' - It then calculates the **7-day moving average** (`7dav`) for each `report_date`,
#'   ensuring proper ordering of reference dates.
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
#'
#' @details
#' - If `temporal_resol = "weekly"`, lag values are divided by 7 to match weekly steps.
#' - If `temporal_resol = "daily"`, lag values are applied directly.
#' - The function groups data by `report_date`, sorts it by `refd_col`, and applies lags.
#' - New lagged columns are named as `"lagX"`, where `X` is the lag value.
#'
#' @return A modified data frame with additional lagged columns.
#'
#' @importFrom dplyr %>% group_by arrange mutate across ungroup
#' @importFrom rlang sym set_names
#' @export
add_lagged_terms <- function(df, value_col, refd_col, lag_col, lagged_term_list=1:7, temporal_resol = "weekly") {
  # Validate temporal resolution
  if (!temporal_resol %in% c("weekly", "daily")) {
    stop("Invalid temporal_resol value. Choose 'weekly' or 'daily'.")
  }

  # Set time step based on resolution
  timestep <- ifelse(temporal_resol == "weekly", 7, 1)

  df$report_date <- df[[refd_col]] + df[[lag_col]]

  df <- df %>%
    group_by(report_date) %>%  # Group by report_date
    dplyr::arrange(!!sym(refd_col), .by_group = TRUE) %>%  # Use refd_col for sorting
    dplyr::mutate(dplyr::across(
      .cols = !!sym(value_col),
      .fns = list(
        !!!setNames(
          lapply(lagged_term_list, function(x) ~ dplyr::lag(., x / timestep)),  # Apply lags, its the lag for timestep
          paste0("lag", lagged_term_list )  # Dynamically name shifted columns
        )
      )
    )) %>%
    ungroup()

  min_lag <- min(df[[lag_col]])
  for (lag in lagged_term_list) {
    # Select and adjust the previous lagged data
    start_df_prev <- df[df[[lag_col]] == min_lag, c(refd_col, value_col)]
    start_df_prev[[refd_col]] <- start_df_prev[[refd_col]] + lag  # Adjust reference column
    colnames(start_df_prev)[colnames(start_df_prev) == value_col] <- paste0(value_col, "_start_lag", lag)

    # Merge back to original dataframe, ensuring distinct rows
    df <- merge(df,
                start_df_prev[, !(colnames(start_df_prev) %in% c(lag_col))] %>% distinct(.keep_all = TRUE),
                by=refd_col, all.x=TRUE)

    # Compute log difference, ensuring safety in calculations
    start_col <- paste0(value_col, "_start_lag", lag)
    current_col <- paste0(value_col, "_lag", lag)
    df[[paste0("log_delta_", value_col, "_lag", lag)]] <- log(df[[current_col]]+1) - log(df[[start_col]] + 1)
  }
  return (as.data.frame(df))
}

#' Add target values based on a reference lag
#'
#' This function adds target values to the dataset by extracting values at a specified
#' reference lag (`ref_lag`). It merges these target values back into the original dataset.
#'
#' @param df Data frame containing the time-series data.
#' @param value_col Name of the column containing the value to be used as a target.
#' @param refd_col Name of the reference date column.
#' @param lag_col Name of the column indicating the lag (difference between reference and issue date).
#' @param ref_lag The reference lag to determine target values.
#' @param temporal_resol Character indicating temporal resolution ("daily" or "weekly").
#'
#' @details
#' - The function filters rows where `lag_col == ref_lag` to extract target values.
#' - Renames extracted value columns (`value_col` and `value_7dav`) as target values.
#' - Merges the extracted target values back into the original dataset based on `refd_col`.
#'
#' @return A modified data frame with additional columns:
#' - `value_target`: The target value at the specified `ref_lag`.
#' - `value_target_7dav`: The 7-day moving average of the target value.
#' - `target_date`: The corresponding issue date for the target.
#'
#' @importFrom dplyr rename
#' @export
add_targets <- function(df, value_col, refd_col, lag_col, ref_lag, temporal_resol) {
  available_lags <- sort(unique(df[[lag_col]]))
  effective_ref_lag <- min(available_lags[available_lags >= ref_lag])
  if (is.infinite(effective_ref_lag))
    stop(sprintf("ref_lag %d exceeds all available lags (max %d)", ref_lag, max(available_lags)))
  if (effective_ref_lag != ref_lag)
    message(sprintf("ref_lag %d not available; using next lag %d", ref_lag, effective_ref_lag))
  target_df <- df[df[[lag_col]] == effective_ref_lag, c(refd_col, "report_date", value_col, "value_7dav")]
  # Rename columns for clarity
  target_df <- target_df %>%
    dplyr::rename(
      value_target = !!sym(value_col),
      value_target_7dav = value_7dav,
      target_date = report_date
    )

  backfill_df <- merge(df, target_df, by=refd_col, all.x=TRUE)
  return (as.data.frame(backfill_df))
}

#' Construct one raw-aware target per reference date
#'
#' Selects the latest genuine value-changing revision in
#' `[ref_lag - lower_tolerance, ref_lag + upper_tolerance]`. If none exists,
#' it falls back to the latest raw value at or before the lower boundary.
#' The returned table is sparse (one row per reference date) and is intended
#' to be joined after feature-grid filling.
#'
#' @param df Raw reporting-triangle data frame.
#' @param value_col Name of the value column used to identify revisions.
#' @param refd_col Name of the reference-date column.
#' @param lag_col Name of the day-based reporting-lag column.
#' @param ref_lag Central target lag, in days.
#' @param lower_tolerance Non-negative days before `ref_lag` included in the
#'   genuine-revision search window.
#' @param upper_tolerance Non-negative days after `ref_lag` included in the
#'   genuine-revision search window.
#' @param temporal_resol Either `"daily"` or `"weekly"`.
#' @param target_as_of_date Optional date limiting target selection to reports
#'   available on or before that date. The effective upper target-window date
#'   is the earlier of this date and `reference_date + ref_lag + upper_tolerance`.
#' @return A compact data frame with at most one target per reference date.
#' @export
create_target_lookup <- function(df, value_col, refd_col, lag_col, ref_lag,
                                 lower_tolerance = 0, upper_tolerance = 0,
                                 temporal_resol = "daily",
                                 target_as_of_date = NULL) {
  if (lower_tolerance < 0 || upper_tolerance < 0) {
    stop("Target lag tolerances must be non-negative.")
  }
  if (nrow(df) == 0) {
    return(data.frame(reference_date = as.Date(character()),
                      target_date = as.Date(character()),
                      target_lag = numeric(), target_type = character()))
  }
  raw <- df[, unique(c(refd_col, lag_col, value_col)), drop = FALSE]
  raw[[refd_col]] <- as.Date(raw[[refd_col]])
  raw$report_date <- raw[[refd_col]] + raw[[lag_col]]
  if (temporal_resol == "weekly") {
    raw <- normalize_weekly_observations(raw, refd_col, lag_col)
  } else if (temporal_resol != "daily") {
    stop("Invalid temporal_resol. Choose either 'daily' or 'weekly'.")
  }
  raw <- raw[raw[[lag_col]] >= 0, , drop = FALSE]
  raw <- raw[order(raw[[refd_col]], raw[[lag_col]], raw$report_date), , drop = FALSE]
  lower <- ref_lag - lower_tolerance
  upper <- ref_lag + upper_tolerance

  chosen <- lapply(split(raw, raw[[refd_col]]), function(g) {
    if (!is.null(target_as_of_date)) {
      g <- g[g$report_date <= as.Date(target_as_of_date), , drop = FALSE]
      if (nrow(g) == 0) return(NULL)
    }
    values <- g[[value_col]]
    changed <- c(TRUE, (is.na(values[-1]) != is.na(values[-length(values)])) |
      (!is.na(values[-1]) & !is.na(values[-length(values)]) &
         values[-1] != values[-length(values)]))
    candidates <- which(changed & g[[lag_col]] >= lower & g[[lag_col]] <= upper)
    if (length(candidates) > 0) {
      idx <- tail(candidates, 1)
      type <- "revision"
    } else {
      fallback <- which(g[[lag_col]] <= lower)
      if (length(fallback) == 0) return(NULL)
      idx <- tail(fallback, 1)
      type <- "fallback"
    }
    data.frame(
      reference_date = as.Date(g[[refd_col]][idx]),
      target_date = as.Date(g$report_date[idx]),
      target_lag = as.numeric(g[[lag_col]][idx]),
      target_type = type
    )
  })
  result <- dplyr::bind_rows(chosen)
  if (nrow(result) == 0) {
    return(data.frame(reference_date = as.Date(character()),
                      target_date = as.Date(character()),
                      target_lag = numeric(), target_type = character()))
  }
  result
}

attach_target_lookup <- function(df, target_lookup) {
  target_values <- df %>%
    select(reference_date, target_date = report_date,
           value_target = value_raw, value_target_7dav = value_7dav)
  df %>%
    left_join(target_lookup, by = "reference_date") %>%
    left_join(target_values, by = c("reference_date", "target_date"))
}

# Build a sparse lookup indicating which rows were present in the raw archive
# rather than introduced by grid completion. Every observed report is retained,
# including reports whose value is unchanged from the preceding report.
create_genuine_event_lookup <- function(df, value_col, refd_col, lag_col,
                                        temporal_resol = "daily") {
  if (nrow(df) == 0L) {
    return(data.frame(
      reference_date = as.Date(character()),
      report_date = as.Date(character()),
      genuine_event = logical()
    ))
  }

  raw <- df[, unique(c(refd_col, lag_col)), drop = FALSE]
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
#' @param onehot_weekdays Named or unnamed list of character vectors defining day groups
#'   for one-hot encoding.  Each group becomes one binary column per date column.
#'   Names, when present, are used as column prefixes; otherwise day abbreviations are
#'   concatenated.  Defaults to `list(Mon=c("Mon"), Weekends=c("Sat","Sun"))`.
#'
#' @details
#' - If `temporal_resol` is "daily", one-hot encoded day-of-week group columns are added
#'   for both `refd_col` (reference date) and `"report_date"`.
#' - One-hot encoded week-of-month columns are added for `"report_date"` in all cases.
#'
#' @return A modified data frame with additional date-related feature columns.
#'
#' @export
add_params_for_dates <- function(df, refd_col, lag_col, temporal_resol = "daily",
                                  onehot_weekdays = list(Mon = c("Mon"), Weekends = c("Sat", "Sun"))) {
  df$report_date <- df[[refd_col]] + df[[lag_col]]
  if (temporal_resol == "daily") {
    # An exhaustive weekday partition drops one data-adaptive reference group
    # for each date axis. Non-exhaustive contrasts such as Mon/Weekends retain
    # every requested group because the unlisted weekdays are the baseline.
    df <- add_grouped_dayofweek(df, refd_col, "_ref", onehot_weekdays)
    df <- add_grouped_dayofweek(df, "report_date", "_issue", onehot_weekdays)
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
#' @param refd_col Character specifying the reference column for merging operations.
#' @param lag_col Character specifying the column representing lag information.
#' @param ref_lag Integer specifying the reference lag value.
#' @param suffixes Character vector containing suffixes for value columns.
#' @param lagged_term_list Numeric vector specifying the list of lags to be considered.
#' @param value_type Character indicating the type of values ('count' or 'fraction').
#' @param temporal_resol Character specifying temporal resolution ('daily' or 'weekly').
#' @param smoothed Logical indicating whether smoothing should be applied.
#' @param target_lag_lower_tolerance Non-negative days before `ref_lag` to
#'   search for genuine target revisions.
#' @param target_lag_upper_tolerance Non-negative days after `ref_lag` to
#'   search for genuine target revisions.
#' @param target_as_of_date Optional date limiting target construction to
#'   reports available on or before that date.
#' @param onehot_weekdays Named or unnamed list of weekday groups passed to
#'   [add_params_for_dates()] for daily calendar features.
#' @details The returned data includes a logical `genuine_event` column.
#'   `TRUE` identifies every report present in the raw triangle, including an
#'   unchanged repeated report. Rows created by grid completion or
#'   carry-forward filling are `FALSE`. This metadata is computed before
#'   missing-update filling so [DelphiRF()] can independently control
#'   observed-report filtering in training and testing. Counts and fractions
#'   supplied as one value column use `log(value + 1)`. Fractions supplied as
#'   numerator and denominator columns use
#'   `log(numerator + 1) - log(denominator + 1)`.
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
                               target_lag_lower_tolerance = 0,
                               target_lag_upper_tolerance = 0,
                               onehot_weekdays = list(Mon = c("Mon"), Weekends = c("Sat", "Sun")),
                               target_as_of_date = NULL) {
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
      warning("Only one value column provided; it will be treated as a fraction.")
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

  genuine_event_lookup <- create_genuine_event_lookup(
    df, value_col, refd_col, lag_col, temporal_resol
  )

  target_lookup <- create_target_lookup(
    df, value_col[1], refd_col, lag_col, ref_lag,
    target_lag_lower_tolerance, target_lag_upper_tolerance, temporal_resol,
    target_as_of_date = target_as_of_date
  )

  dfList <- lapply(value_col, function(value_col) {
    filled_df <- fill_missing_updates(df, value_col, refd_col, lag_col, temporal_resol)
    if (!(smoothed) & (temporal_resol == "daily")){
      filled_df <- add_7davs(filled_df, "value_raw", "reference_date", "lag")
    } else {
      filled_df$value_7dav = filled_df$value_raw
    }
    filled_df <- add_lagged_terms(filled_df, "value_7dav", "reference_date", "lag", lagged_term_list, temporal_resol)
    filled_df <- attach_target_lookup(filled_df, target_lookup)
    add_log_transformed(filled_df, lagged_term_list)
  })

  merged_df <- Reduce(
    function(x, y) full_join(
      x, y,
      by = c("reference_date", "report_date", "lag", "target_date",
             "target_lag", "target_type"),
      suffix = suffixes
    ),
    dfList
  )

  if (length(value_col) > 1) {
    cols <- colnames(merged_df)[grepl("value", colnames(merged_df), ignore.case = TRUE)]
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
  merged_df <- add_params_for_dates(merged_df, "reference_date", "lag", temporal_resol, onehot_weekdays)

  merged_df <- dplyr::left_join(
    merged_df, genuine_event_lookup,
    by = c("reference_date", "report_date")
  )
  merged_df$genuine_event <- !is.na(merged_df$genuine_event) &
    merged_df$genuine_event

  merged_df <- merged_df %>%
    filter(.data$lag < ref_lag)

  return(tibble::as_tibble(merged_df))
}


#' Add Weight-Related Features to a Dataframe
#'
#' This function calculates various weight-related features for a given dataframe,
#' including the difference in days, log slope, and differences in values relative to reference data.
#'
#' @param df A dataframe containing the necessary columns for computation.
#' @param comparison_date A Date object representing the reference date for comparison.
#' @return A dataframe with additional computed columns for weight-related metrics.
#' @importFrom dplyr %>% group_by ungroup select rename slice_max mutate left_join
#' @importFrom rlang .data
#' @export
add_weights_related <- function(df) {
  if (!all(c("report_date", "value_7dav",
             "value_7dav_lag7", "log_value_7dav",
             "log_value_7dav_lag7", "lag") %in% colnames(df))) {
    stop("Missing required columns when adding weighted related features")
  }

  combined_df <- df %>%
    mutate(
      log_7dav_slope = .data$log_value_7dav - .data$log_value_7dav_lag7
    )

  ref_values <- combined_df %>%
    group_by(.data$lag) %>%
    slice_max(.data$report_date, with_ties = FALSE) %>%  # Get the row with the max report_date per lag_col
    ungroup() %>%
    dplyr::select(.data$lag, .data$log_7dav_slope, .data$log_value_7dav) %>%
    dplyr::rename(
      ref_slope_value = .data$log_7dav_slope,
      ref_7dav_value = .data$log_value_7dav
    )

  # Join reference values back to combined_df
  combined_df <- combined_df %>%
    left_join(ref_values, by = "lag") %>%
    mutate(
      value_slope_diff = abs(.data$log_7dav_slope - .data$ref_slope_value),
      value_7dav_diff = abs(.data$log_value_7dav - .data$ref_7dav_value)
    )
  return(as.data.frame(combined_df))
}
