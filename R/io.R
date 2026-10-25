#' Make sure data contains a `geo_value` field
#'
#' Renames a `fips` column to `geo_value` when `geo_value` is absent, and drops
#' `fips` when both are present.
#'
#' @param df A data frame with a `geo_value` column, a `fips` column, or both.
#' @return `df` with a `geo_value` column and no `fips` column.
#'
#' @importFrom dplyr rename select
#' @importFrom rlang .data
fips_to_geovalue <- function(df) {
  if ( !("geo_value" %in% colnames(df)) ) {
    if ( !("fips" %in% colnames(df)) ) {
      stop("Either `fips` or `geo_value` field must be available")
    }
    df <- rename(df, geo_value = fips)
  }
  if ( "fips" %in% colnames(df) ) {
    df <- select(df, -fips)
  }
  return(df)
}
