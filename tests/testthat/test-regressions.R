test_that("modern tidyverse operations are warning-free", {
  weekday_data <- data.frame(
    event_date = as.Date("2024-01-01") + 0:6
  )
  expect_no_warning(
    weekday_result <- add_dayofweek(weekday_data, "event_date", "_event")
  )
  expect_equal(nrow(weekday_result), 7L)

  revision_data <- data.frame(
    ref_date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-04")),
    lag = c(0L, 1L, 2L),
    value = c(10, NA, 30)
  )
  expect_no_warning(
    filled_result <- fill_missing_updates(
      revision_data,
      value_col = "value",
      refd_col = "ref_date",
      lag_col = "lag",
      temporal_resol = "daily"
    )
  )
  expect_true(all(!is.na(filled_result$value_raw)))

  weight_data <- data.frame(
    report_date = as.Date("2024-01-01") + 0:3,
    lag = c(0L, 0L, 1L, 1L),
    value_7dav = c(10, 11, 12, 13),
    value_7dav_lag7 = c(8, 9, 10, 11),
    log_value_7dav = log1p(c(10, 11, 12, 13)),
    log_value_7dav_lag7 = log1p(c(8, 9, 10, 11))
  )
  expect_no_warning(weight_result <- add_weights_related(weight_data))
  expect_true(all(c("value_slope_diff", "value_7dav_diff") %in%
                    names(weight_result)))
})

test_that("loading DelphiRF in a clean R session has no filesystem side effects", {
  work_dir <- tempfile("delphirf-clean-load-")
  dir.create(file.path(work_dir, "R"), recursive = TRUE)
  writeLines("#' @template clean-load-probe", file.path(work_dir, "R", "probe.R"))
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  probe_script <- tempfile("delphirf-clean-load-", fileext = ".R")
  on.exit(unlink(probe_script), add = TRUE)
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "setwd(args[[1L]])",
    "before <- sort(list.files('.', all.files = TRUE, recursive = TRUE))",
    "suppressPackageStartupMessages(library(DelphiRF))",
    "after <- sort(list.files('.', all.files = TRUE, recursive = TRUE))",
    "if (!identical(before, after)) {",
    "  stop('Package load changed the working directory: ',",
    "       paste(setdiff(after, before), collapse = ', '))",
    "}"
  ), probe_script)

  rscript <- file.path(R.home("bin"), "Rscript")
  output <- system2(
    rscript,
    c("--vanilla", shQuote(probe_script), shQuote(work_dir)),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L

  expect_equal(status, 0L, info = paste(output, collapse = "\n"))
  expect_false(dir.exists(file.path(work_dir, "man-roxygen")))
})
