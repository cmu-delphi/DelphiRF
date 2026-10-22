# Helpers for completing a reporting triangle. Callers identify the raw value,
# reference-date, and reporting-lag columns; report dates are derived as
# reference date plus lag. Location columns are optional and are not used by
# these helpers.


normalize_weekly_observations <- function(df, refd_col, lag_col) {
  if (nrow(df) == 0) return(df)
  original_reference_date <- as.Date(df[[refd_col]])
  original_report_date <- original_reference_date + df[[lag_col]]
  epiweek_end <- function(x) x + ((6L - as.POSIXlt(x)$wday) %% 7L)
  df[[refd_col]] <- epiweek_end(original_reference_date)
  df$report_date <- epiweek_end(original_report_date)
  df[[lag_col]] <- as.numeric(df$report_date - df[[refd_col]])
  ordering <- order(df[[refd_col]], df$report_date,
                    original_report_date, original_reference_date)
  df <- df[ordering, , drop = FALSE]
  key <- paste(df[[refd_col]], df$report_date, sep = "\r")
  df[!duplicated(key, fromLast = TRUE), , drop = FALSE]
}

# Stop when a reference date has two different values on the same report date.
stop_on_conflicting_releases <- function(df, value_col, refd_col) {
  keys <- unique(df[c(refd_col, "report_date", value_col)])
  conflicting <- unique(keys[duplicated(keys[c(refd_col, "report_date")]),
                             c(refd_col, "report_date")])
  if (nrow(conflicting)) {
    shown <- utils::head(conflicting, 5)
    stop(sprintf(
      "%d reference date/report date pair(s) have more than one value in column '%s'; keep one release per pair. First: %s%s",
      nrow(conflicting), value_col,
      paste(sprintf("reference %s reported %s", format(shown[[refd_col]]),
                    format(shown$report_date)), collapse = "; "),
      if (nrow(conflicting) > 5) "; ..." else ""
    ), call. = FALSE)
  }
  invisible(TRUE)
}

#' Fill missing updates in a reporting triangle
#'
#' Creates a complete reference-date/report-date grid, carries the latest
#' previously reported value forward, and uses zero before the first report.
#' A missing value counts as no update, so the previous value is carried
#' forward. Two rows for the same reference and report date with different
#' values are an error that lists the conflicting dates.
#'
#' @param df A data frame containing the dataset with reference and lagged dates.
#' @param value_col A string specifying the column name containing the values
#'   to be filled and processed.
#' @template refd_col-template
#' @template lag_col-template
#' @param temporal_resol A string specifying the temporal resolution of the data.
#'   Accepts `"daily"` (default) or `"weekly"` for processing in daily or weekly intervals.
#' @param max_report_override Optional maximum report date through which missing
#'   updates should be generated.
#' @param max_lag Optional largest reporting lag, in days, to keep on the grid.
#'   `NULL` (default) keeps every lag. Carrying values forward along report
#'   dates never needs a larger lag, so rows up to `max_lag` are unchanged.
#' @return A data frame on the completed reporting grid with `value_raw`,
#'   `reference_date`, `report_date`, and `lag` columns.
#'
#' @importFrom tidyr fill pivot_wider pivot_longer replace_na expand_grid
#' @importFrom dplyr %>% select left_join mutate arrange distinct filter everything
#' @export
fill_missing_updates <- function(df, value_col, refd_col, lag_col, temporal_resol = "daily",
                              max_report_override = NULL, max_lag = NULL) {
  df <- df %>% distinct()  # Remove duplicates if any

  if (nrow(df) == 0) {
    return(data.frame())
  }
  df$report_date <- df[[refd_col]] + df[[lag_col]]
  stop_on_conflicting_releases(df, value_col, refd_col)

  if (temporal_resol == "weekly") {
    df <- normalize_weekly_observations(df, refd_col, lag_col)
  }

  # Generate a sequence of all possible dates
  if (temporal_resol == "daily"){
    all_reference_dates <- seq(min(df[[refd_col]]), max(df[[refd_col]]), by = "day")
    all_report_dates <- seq(min(df[["report_date"]]), max(df[["report_date"]]), by = "day")
    gap <- 1
  } else if (temporal_resol == "weekly"){
    all_reference_dates <- seq(min(df[[refd_col]]), max(df[[refd_col]]), by = "7 days")
    all_report_dates <- seq(min(df[["report_date"]]), max(df[["report_date"]]), by = "7 days")

    gap <- 7
  } else {
    stop("Invalid temporal_resol. Choose either 'daily' or 'weekly'.")
  }

  if (!is.null(max_report_override)) {
    max_report_override <- as.Date(max_report_override)
    if (max_report_override > max(all_report_dates)) {
      first_extra <- max(all_report_dates) + gap
      # A weekly as-of date can fall between two reporting-grid dates. In that
      # case there is no complete additional week to add; calling seq() with a
      # positive step and an earlier endpoint raises "wrong sign in 'by'".
      if (first_extra <= max_report_override) {
        extra <- seq(first_extra, max_report_override, by = gap)
        all_report_dates <- c(all_report_dates, extra)
      }
    }
  }

  # Create a complete grid of all combinations of reference_date and report_date
  complete_grid <- tidyr::expand_grid(
    .reference_date = all_reference_dates,
    report_date = all_report_dates
  )
  names(complete_grid)[names(complete_grid) == ".reference_date"] <- refd_col
  if (!is.null(max_lag)) {
    # Keep only the lags needed downstream; a value is carried forward from
    # smaller lags, so trimming larger ones changes nothing that is kept.
    grid_lag <- as.numeric(complete_grid$report_date - complete_grid[[refd_col]])
    complete_grid <- complete_grid[grid_lag <= max_lag, , drop = FALSE]
  }

  # Join with the original dataset and fill missing values
  filled_df <- complete_grid %>%
    left_join(df, by = c(refd_col, "report_date")) %>%
    filter(report_date >= .data[[refd_col]])  # Keep only valid issue-reference pairs

  pivot_df <- filled_df %>%
    dplyr::arrange(.data[["report_date"]]) %>%
    tidyr::pivot_wider(
      id_cols = "report_date",
      names_from = dplyr::all_of(refd_col),
      values_from = dplyr::all_of(value_col)
    )

  # Check for irregular gaps
  if (any(as.numeric(diff(pivot_df[["report_date"]]), units = "days") != gap)) {
    stop("Risk exists in forward filling due to irregular gaps in lag values.")
  }

  # Forward fill missing values
  pivot_df <- pivot_df %>% fill(everything(), .direction = "down")

  # Fill NAs with 0s
  pivot_df[is.na(pivot_df)] <- 0

  # Convert back to long format
  backfill_df <- pivot_df %>%
    tidyr::pivot_longer(-report_date, values_to = "value_raw", names_to = refd_col) %>%
    mutate(
      reference_date = as.Date(.data[[refd_col]]),
      lag = as.numeric(report_date - reference_date)
    ) %>%
    filter(lag >= 0, lag <= if (is.null(max_lag)) Inf else max_lag)
  return (as.data.frame(backfill_df))
}

#' Get week of a month info according to a date
#'
#' Weeks run Sunday to Saturday. The i-th week of the month is labelled i,
#' except the month's last week: its dates share a week with the first days
#' of the next month and are sparse, so they are labelled 1. A month spanning
#' six weeks is therefore labelled 1, 2, 3, 4, 5, 1.
#'
#' @param date A Date object.
#' @return An integer indicating the week number within the month.
#' @importFrom lubridate make_date days_in_month
get_weekofmonth <- function(date) {
  if (!inherits(date, "Date")) {
    stop("Input must be a Date object.")
  }

  # Extract year, month, and day
  year <- lubridate::year(date)
  month <- lubridate::month(date)
  day <- lubridate::day(date)

  # Compute the day of the week for the first day of the month
  firstdayofmonth <- lubridate::wday(
    lubridate::make_date(year, month, 1), week_start = 7
  ) # Sunday is the first day of the week.
  # Calculate which week the date belongs to
  week_number <- (day + firstdayofmonth - 2) %/% 7 + 1

  last_day <- days_in_month(date)
  last_day_week <- ((last_day + firstdayofmonth - 2) %/% 7) + 1

  week_number[week_number == last_day_week] <- 1

  return(week_number)
}
