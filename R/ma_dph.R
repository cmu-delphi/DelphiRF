#' Massachusetts COVID-19 confirmed cases with revision history
#'
#' Daily confirmed COVID-19 case counts from the Massachusetts Department of
#' Public Health (MA DPH), as reported in successive releases. Each row is the
#' value for one test date as it appeared in one release.
#'
#' @format A data frame with 257,381 rows and 6 variables:
#' \describe{
#'   \item{test_date}{Reference date: the date the tests were performed.}
#'   \item{total_positive}{Cumulative confirmed cases through `test_date`.}
#'   \item{new_positive}{Confirmed cases on `test_date`.}
#'   \item{confirmed_case_7d_avg}{Trailing 7-day average of `new_positive`.}
#'   \item{issue_date}{Report date of the release, 2021-01-04 to 2022-07-21.}
#'   \item{lag}{Days between `test_date` and `issue_date`.}
#' }
#' @source Massachusetts Department of Public Health COVID-19 data releases.
#' @usage data(ma_dph)
"ma_dph"
