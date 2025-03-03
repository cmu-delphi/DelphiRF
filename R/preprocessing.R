## Data Preprocessing
##
## The raw input data should have 4/5 basic columns:
## reference_date: reference date
## report_date: issue date/date of reporting
## geo_value: location
## lag: the number of days between issue date and the reference date
## counts: the number of counts used for estimation


#' Re-index, fill na, make sure all reference date have enough rows for updates
#' @param df A data.frame
#' @param refd_col A string specifying the column name representing the
#'   reference date (i.e., the date the report is issued for).
#' @param lag_col A string specifying the column name representing the lag
#'   (i.e., the difference between issue date and reference date).
#' @param min_refd the earliest reference date considered in the data
#' @param max_refd the latest reference date considered in the data
#' @param ref_lag An integer to indicate the reference lag
#'
#' @return df_new Data Frame with filled rows for missing lags
#'
#' @importFrom tidyr crossing
#' @importFrom stats setNames
#'
#' @export
fill_rows <- function(df, refd_col, lag_col, min_refd, max_refd, ref_lag) {
  # Full list of lags
  # +30 to have values for calculating 7-day averages
  lags <- min(df[[lag_col]]): (ref_lag + 30)
  refds <- seq(min_refd, max_refd, by="day") # Full list reference date
  row_inds_df <- as.data.frame(crossing(refds, lags)) %>%
    setNames(c(refd_col, lag_col))
  df_new = merge(x=df, y=row_inds_df,
                 by=c(refd_col, lag_col),  all.y=TRUE)
  return (df_new)
}

#' Fill missing updates in a time series dataset with lagged values
#' Get pivot table, filling NANs. If there is no update on issue date D but
#' previous reports exist for issue date D_p < D, all the dates between
#' [D_p, D] are filled with with the reported value on date D_p. If there is
#' no update for any previous issue date, fill in with 0.
#' This function creates a complete reference-report date grid and fills in missing updates.
#' If an update is missing on a certain issue date but previous updates exist,
#' it fills gaps with the last available value. If no previous updates exist, it fills with `0`.
#'
#' @param df A data frame containing the dataset with reference and lagged dates.
#' @param value_col A string specifying the column name containing the values
#'   to be filled and processed.
#' @template refd_col-template
#' @template lag_col-template
#' @param temporal_resol A string specifying the temporal resolution of the data.
#'   Accepts `"daily"` (default) or `"weekly"` for processing in daily or weekly intervals.
#'
#' @importFrom tidyr fill pivot_wider pivot_longer replace_na expand_grid
#' @importFrom dplyr %>% select left_join mutate arrange distinct filter everything
#' @export
fill_missing_updates <- function(df, value_col, refd_col, lag_col, temporal_resol="daily") {
  df <- df %>% distinct()  # Remove duplicates if any

  if (nrow(df) == 0) {
    return(data.frame())
  }
  df$report_date <- df[[refd_col]] + df[[lag_col]]

  # Generate a sequence of all possible dates
  if (temporal_resol == "daily"){
    all_reference_dates <- seq(min(df[[refd_col]]), max(df[[refd_col]]), by = "day")
    all_report_dates <- seq(min(df[["report_date"]]), max(df[["report_date"]]), by = "day")
    gap <- 1
  } else if (temporal_resol == "weekly"){
    all_reference_dates <- seq(min(df[[refd_col]]), max(df[[refd_col]]), by = "7 days")
    all_report_dates <- seq(min(df[["report_date"]]), max(df[["report_date"]]), by = "7 days")

    # Check if all reference dates in df are within the generated sequence
    if (!all(df[[refd_col]] %in% all_reference_dates)) {
      stop("The reference dates do not regularly have a gap of 7 days. Some reference dates will be ignored. Please check your input data.")
    }

    # Check if all reference dates in df are within the generated sequence
    if (!all(df$report_date %in% all_report_dates)) {
      stop("The report dates do not regularly have a gap of 7 days. Some report dates will be ignored. Please check your input data.")
    }

    gap <- 7
  } else {
    stop("Invalid temporal_resol. Choose either 'daily' or 'weekly'.")
  }

  # Create a complete grid of all combinations of reference_date and report_date
  complete_grid <- tidyr::expand_grid(
    !!refd_col := all_reference_dates,
    report_date = all_report_dates
  )

  # Join with the original dataset and fill missing values
  filled_df <- complete_grid %>%
    left_join(df, by = c(refd_col, "report_date")) %>%
    filter(report_date >= .data[[refd_col]])  # Keep only valid issue-reference pairs

  pivot_df <- filled_df %>%
    dplyr::arrange(.data[["report_date"]]) %>%
    tidyr::pivot_wider(id_cols = "report_date", names_from = refd_col, values_from = value_col)

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
      reference_date := as.Date(.data[[refd_col]]),
      lag = as.numeric(report_date - reference_date)
    ) %>%
    filter(lag>=0)
  return (as.data.frame(backfill_df))
}

#' Shift reference dates by a specified number of days
#'
#' This function shifts the reference date column by a given number of days.
#'
#' @param df A data frame containing a reference date column.
#' @param n_day An integer specifying the number of days to shift the reference date.
#'   Positive values shift forward, negative values shift backward.
#' @template refd_col-template
#'
#' @return A data frame with the reference date column shifted.
#' @export
add_shift <- function(df, n_day, refd_col) {
  df[[refd_col]] <- as.Date(df[[refd_col]]) + n_day
  return (df)
}

#' Get week of a month info according to a date
#'
#' Consider Epiweeks. Notice that
#' If there are 4, 5 or 6 weeks in total, the ith weeks is labeled as i
#' and the dates in the last week this month are actually in the same
#' week with the dates in the 1st week next month and those dates are
#' sparse. Thus, we assign the dates in the last week to the 1st week.
#' If there are 6 weeks in total, the 1st, 2nd, 3rd, 4th, 5th, 6th weeks
#' are labeled as c(1, 2, 3, 4, 5, 1) which means we will merge the first,
#' second and the last weeks together.
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
  firstdayofmonth <- lubridate::wday(lubridate::make_date(year, month, 1), week_start = 7) # Monday as the first day
  # Calculate which week the date belongs to
  week_number <- (day + firstdayofmonth - 2) %/% 7 + 1

  last_day <- days_in_month(date)
  last_day_week <- ((last_day + firstdayofmonth - 2) %/% 7) + 1

  week_number[week_number == last_day_week] <- 1

  return(week_number)
}



