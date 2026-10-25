library(testthat)

# One location, target lag 7. Reference dates 2024-01-01 .. 2024-01-30 are
# each reported at lags 0 to 6 with a level that differs by reference date and
# lag, so every anchor is identifiable.
anchor_frame <- function() {
  grid <- expand.grid(reference_date = as.Date("2024-01-01") + 0:29, lag = 0:6)
  grid$report_date <- grid$reference_date + grid$lag
  grid$target_date <- grid$reference_date + 7
  grid$genuine_event <- TRUE
  level <- as.numeric(grid$reference_date - as.Date("2024-01-01")) * 10 + grid$lag
  grid$value_7dav <- 100 + level
  grid$value_7dav_lag7 <- 90 + level / 2
  grid$log_value_7dav <- log1p(grid$value_7dav)
  grid$log_value_7dav_lag7 <- log1p(grid$value_7dav_lag7)
  grid[order(grid$reference_date, grid$lag), ]
}

row_at <- function(df, report_date, lag) {
  df[df$report_date == as.Date(report_date) & df$lag == lag, ]
}

test_that("anchors come from the forecast origin, not the latest mature training row", {
  df <- anchor_frame()
  origin <- as.Date("2024-01-20")
  anchors <- create_weight_anchors(df, origin)

  expect_equal(sort(anchors$lag), 0:6)
  for (k in 0:6) {
    expected <- row_at(df, origin, k)            # reported on the origin itself
    anchor <- anchors[anchors$lag == k, ]
    expect_equal(anchor$ref_7dav_value, expected$log_value_7dav)
    expect_equal(anchor$ref_slope_value,
                 expected$log_value_7dav - expected$log_value_7dav_lag7)
  }

  # The latest mature training row at lag 0 (target_date <= origin) is the
  # 2024-01-13 reference date, which is not the anchor.
  mature <- df[df$target_date <= origin & df$lag == 0, ]
  latest_mature <- mature[which.max(mature$report_date), ]
  expect_false(isTRUE(all.equal(anchors$ref_7dav_value[anchors$lag == 0],
                                latest_mature$log_value_7dav)))
})

test_that("anchors include carried-forward rows and ignore rows after the origin", {
  df <- anchor_frame()
  origin <- as.Date("2024-01-20")
  df$genuine_event[df$report_date == origin] <- FALSE  # carried-forward states
  df$log_value_7dav[df$report_date > origin] <- 99      # must never be used
  anchors <- create_weight_anchors(df, origin)
  expect_equal(anchors$ref_7dav_value[anchors$lag == 3],
               row_at(df, origin, 3)$log_value_7dav)
  expect_false(any(anchors$ref_7dav_value == 99))
})

test_that("each lag takes its own latest report under an irregular schedule", {
  df <- anchor_frame()
  origin <- as.Date("2024-01-20")
  # Lag 2 stops being reported after 2024-01-15; lag 5 is reported only on
  # odd days of the month.
  df <- df[!(df$lag == 2 & df$report_date > as.Date("2024-01-15")), ]
  df <- df[!(df$lag == 5 & as.integer(format(df$report_date, "%d")) %% 2 == 0), ]
  anchors <- create_weight_anchors(df, origin)

  expect_equal(anchors$ref_7dav_value[anchors$lag == 2],
               row_at(df, "2024-01-15", 2)$log_value_7dav)
  expect_equal(anchors$ref_7dav_value[anchors$lag == 5],
               row_at(df, "2024-01-19", 5)$log_value_7dav)
  expect_equal(anchors$ref_7dav_value[anchors$lag == 1],
               row_at(df, origin, 1)$log_value_7dav)
})

test_that("training rows are measured against the anchor of their own lag", {
  df <- anchor_frame()
  origin <- as.Date("2024-01-20")
  anchors <- create_weight_anchors(df, origin)
  train <- df[df$target_date <= origin, ]
  weighted <- add_weights_related(train, anchors)

  for (k in c(0, 4)) {
    rows <- weighted[weighted$lag == k, ]
    anchor <- anchors[anchors$lag == k, ]
    expect_equal(rows$value_7dav_diff, abs(rows$log_value_7dav - anchor$ref_7dav_value))
    expect_equal(rows$value_slope_diff,
                 abs(rows$log_value_7dav - rows$log_value_7dav_lag7 - anchor$ref_slope_value))
  }
  expect_false(isTRUE(all.equal(anchors$ref_7dav_value[anchors$lag == 0],
                                anchors$ref_7dav_value[anchors$lag == 4])))
})

test_that("anchors are validated and a lag without an anchor is an error", {
  df <- anchor_frame()
  anchors <- create_weight_anchors(df, as.Date("2024-01-20"))
  expect_error(add_weights_related(df, anchors[anchors$lag != 3, ]),
               "No weight anchor for lag\\(s\\) 3")
  expect_error(add_weights_related(df, rbind(anchors, anchors)), "one row per lag")
  expect_error(add_weights_related(df, anchors[c("lag", "ref_7dav_value")]),
               "create_weight_anchors")
  expect_error(create_weight_anchors(df, NA), "one valid date")
})

test_that("anchors can be kept per location", {
  df <- rbind(transform(anchor_frame(), geo_value = "aa"),
              transform(anchor_frame(), geo_value = "bb",
                        log_value_7dav = log_value_7dav + 1))
  anchors <- create_weight_anchors(df, as.Date("2024-01-20"), by = "geo_value")
  expect_equal(nrow(anchors), 14L)
  weighted <- add_weights_related(df, anchors)
  expect_true(all(weighted$ref_7dav_value[weighted$geo_value == "bb"] >
                    weighted$ref_7dav_value[weighted$geo_value == "aa"]))
})

test_that("weights multiply the min-max normalized level and trend distances", {
  train <- data.frame(value_7dav_diff = c(0, 1, 2, 4),
                      value_slope_diff = c(4, 2, 1, 0))
  level <- train$value_7dav_diff / 4
  trend <- train$value_slope_diff / 4
  expect_equal(observation_weights(train, gamma = 2), exp(-2 * level * trend))
})

test_that("preprocessed nowcasting anchors weights at its own origin", {
  df <- anchor_frame()
  seen <- new.env(parent = emptyenv())
  local_mocked_bindings(
    revision_forecast = function(train_data, test_data, ...) {
      seen$train <- train_data
      seen$test <- test_data
      test_data
    },
    .package = "DelphiRF"
  )
  run <- function(origin) {
    delphirf_nowcast_preprocessed(df, testing_start_date = origin, working_scale = "log_value",
             test_lag_groups = "0", lag_pad = 0, training_days = 60)
    seen$train
  }

  early <- run(as.Date("2024-01-20"))
  expect_equal(unique(early$ref_7dav_value),
               row_at(df, "2024-01-20", 0)$log_value_7dav)
  expect_false(any(c("value_7dav_diff", "ref_7dav_value") %in% names(seen$test)))

  late <- run(as.Date("2024-01-27"))
  expect_equal(unique(late$ref_7dav_value),
               row_at(df, "2024-01-27", 0)$log_value_7dav)
})

test_that("a different weight anchor gets its own cached model", {
  df <- anchor_frame()
  train <- df[df$target_date <= as.Date("2024-01-20") & df$lag == 0, ]
  train$log_value_target <- train$log_value_7dav + 0.1
  test <- df[df$report_date == as.Date("2024-01-20") & df$lag == 0, ]
  test$log_value_target <- NA_real_
  cache <- tempfile("anchor-cache-")
  on.exit(unlink(cache, recursive = TRUE))
  forecast <- function(origin, train_models) {
    weighted <- add_weights_related(train, create_weight_anchors(df, origin))
    revision_forecast(
      weighted, test, taus = 0.5, smoothed_target = FALSE,
      params_list = "log_value_7dav", working_scale = "log_value",
      training_end_date = "2024-01-20", training_days = 60,
      model_save_dir = cache, train_models = train_models
    )
  }
  forecast(as.Date("2024-01-20"), TRUE)
  expect_warning(forecast(as.Date("2024-01-27"), FALSE), "does not exist; training new model")
  expect_length(list.files(cache, pattern = "[.]rds$", recursive = TRUE), 2L)
})
