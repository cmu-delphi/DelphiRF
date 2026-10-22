library(testthat)

test_that("testing conversion of fips to geo_value", {
  geo_value = c("01001", "99999", "12347")
  fips = c("55555", "52390", "00111")

  # Fail if neither `fips` nor `geo_value` exist
  expect_error(fips_to_geovalue(data.frame(empty_col = fips)),
               "Either `fips` or `geo_value` field must be available")

  # Return same df if only `geo_value` exists
  df <- data.frame(geo_value)
  expect_equal(fips_to_geovalue(df), df)

  # Drop `fips` field if both `fips` and `geo_value` exist
  expect_equal(fips_to_geovalue(data.frame(geo_value, fips)), data.frame(geo_value))

  # Rename `fips` to `geo_value` if only `fips` exists
  expect_equal(fips_to_geovalue(data.frame(fips)), data.frame(geo_value = fips))
})
