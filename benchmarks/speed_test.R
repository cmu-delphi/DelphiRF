# Quantile regression backend speed comparison: quantreg vs quantgen
# Run from the repo root: Rscript benchmarks/speed_test.R

devtools::load_all(".", quiet = TRUE)

set.seed(42)

N_TRAIN    <- c(500, 2000, 5000)
N_COVARIATES <- 10
N_NONZERO  <- 3
LAMBDA     <- 0.1
N_REPS     <- 2

# Production quantile vector vs single tau
TAUS_PROD  <- TAUS   # 9 quantiles from constants.R
TAU_SINGLE <- 0.5

# Sparse ground-truth beta; active coefficients are +/-1
TRUE_BETA <- rep(0, N_COVARIATES)
TRUE_BETA[sample(N_COVARIATES, N_NONZERO)] <- sample(c(-1, 1), N_NONZERO, replace = TRUE)

make_data <- function(nn) {
  x <- matrix(rnorm(nn * N_COVARIATES), nrow = nn)
  eps <- rt(nn, df = 3)
  y <- x %*% TRUE_BETA + eps
  list(x = x, y = as.numeric(y))
}

has_quantgen <- requireNamespace("quantgen", quietly = TRUE)
if (!has_quantgen) message("quantgen not installed; only benchmarking quantreg backend")

time_fit <- function(fn) {
  t0 <- proc.time()[["elapsed"]]
  fn()
  proc.time()[["elapsed"]] - t0
}

# --- Section 1: raw solver speed (single tau vs full tau vector) ---
solver_results <- data.frame(
  n_train = integer(), backend = character(),
  taus = character(), rep = integer(), elapsed_sec = numeric()
)

for (nn in N_TRAIN) {
  dat <- make_data(nn)

  for (rr in seq_len(N_REPS)) {
    solver_results <- rbind(solver_results, data.frame(
      n_train = nn, backend = "quantreg", taus = "single",  rep = rr,
      elapsed_sec = time_fit(\() fit_quantile_lasso(dat$x, dat$y, tau = TAU_SINGLE, lambda = LAMBDA))
    ))
    solver_results <- rbind(solver_results, data.frame(
      n_train = nn, backend = "quantreg", taus = "all9", rep = rr,
      elapsed_sec = time_fit(\() fit_quantile_lasso(dat$x, dat$y, tau = TAUS_PROD, lambda = LAMBDA))
    ))
  }

  if (has_quantgen) {
    for (rr in seq_len(N_REPS)) {
      solver_results <- rbind(solver_results, data.frame(
        n_train = nn, backend = "quantgen", taus = "single", rep = rr,
        elapsed_sec = time_fit(\() quantgen::quantile_lasso(
          dat$x, dat$y, tau = TAU_SINGLE, lambda = LAMBDA,
          standardize = TRUE, intercept = TRUE, lp_solver = "glpk"))
      ))
      solver_results <- rbind(solver_results, data.frame(
        n_train = nn, backend = "quantgen", taus = "all9", rep = rr,
        elapsed_sec = time_fit(\() quantgen::quantile_lasso(
          dat$x, dat$y, tau = TAUS_PROD, lambda = LAMBDA,
          standardize = TRUE, intercept = TRUE, lp_solver = "glpk"))
      ))
    }
  }
}

smry <- aggregate(elapsed_sec ~ n_train + backend + taus, data = solver_results,
                  FUN = function(xx) c(mean = mean(xx), sd = sd(xx)))
smry <- do.call(data.frame, smry)
names(smry) <- c("n_train", "backend", "taus", "mean_sec", "sd_sec")
smry$mean_ms <- round(smry$mean_sec * 1000, 1)
smry$sd_ms   <- round(smry$sd_sec  * 1000, 1)

cat("\n=== Raw solver speed (ms) ===\n")
cat(sprintf("Covariates: %d | Reps per cell: %d\n\n", N_COVARIATES, N_REPS))
print(smry[order(smry$n_train, smry$taus, smry$backend),
           c("n_train", "taus", "backend", "mean_ms", "sd_ms")], row.names = FALSE)

if (has_quantgen) {
  cat("\n=== Relative speed (quantreg / quantgen, all9 taus) ===\n")
  for (nn in N_TRAIN) {
    qr <- smry$mean_sec[smry$n_train == nn & smry$backend == "quantreg" & smry$taus == "all9"]
    qg <- smry$mean_sec[smry$n_train == nn & smry$backend == "quantgen" & smry$taus == "all9"]
    if (length(qr) && length(qg) && qg > 0) {
      ratio <- qg / qr
      cat(sprintf("  n=%5d: quantreg is %.1fx %s\n", nn, max(ratio, 1/ratio),
                  ifelse(ratio >= 1, "faster", "slower")))
    }
  }
}

# --- Section 2: end-to-end revision_forecast speed ---
cat("\n=== End-to-end revision_forecast speed (ms) ===\n")
cat(sprintf("Reps per cell: %d\n\n", N_REPS))

e2e_results <- data.frame(
  n_train = integer(), backend = character(), rep = integer(), elapsed_sec = numeric()
)

make_rf_data <- function(nn, start_date) {
  dates <- seq(as.Date(start_date), by = "day", length.out = nn)
  data.frame(
    reference_date   = dates,
    report_date      = dates + 3L,
    lag              = 3L,
    value_7dav       = runif(nn, 0, 1),
    log_value_7dav   = rnorm(nn),
    log_value_target = rnorm(nn),
    value_7dav_diff  = rnorm(nn),
    value_slope_diff = rnorm(nn),
    Mon_ref          = sample(c(0L, 1L), nn, replace = TRUE)
  )
}

rf_base_args <- list(
  taus            = TAUS_PROD,
  smoothed_target = FALSE,
  params_list     = c("log_value_7dav", "Mon_ref"),
  temporal_resol  = "daily",
  lambda          = LAMBDA,
  gamma           = 0.1,
  indicator       = "bench",
  signal          = "sig",
  geo_level       = "state",
  geo             = "pa",
  make_predictions = TRUE
)

# Separate cache dirs so backends don't load each other's models
dir_qr <- file.path(tempdir(), "bench_quantreg")
dir_qg <- file.path(tempdir(), "bench_quantgen")
dir.create(dir_qr, showWarnings = FALSE)
dir.create(dir_qg, showWarnings = FALSE)

for (nn in N_TRAIN) {
  train_rf <- make_rf_data(nn, "2020-01-01")
  test_rf  <- make_rf_data(30, "2020-01-01")

  for (rr in seq_len(N_REPS)) {
    args_qr <- c(rf_base_args, list(
      train_data    = train_rf,
      test_data     = test_rf,
      training_days = nn,
      train_models  = TRUE,
      backend       = "quantreg",
      model_save_dir = dir_qr
    ))
    e2e_results <- rbind(e2e_results, data.frame(
      n_train = nn, backend = "quantreg", rep = rr,
      elapsed_sec = time_fit(\() do.call(revision_forecast, args_qr))
    ))
  }

  if (has_quantgen) {
    for (rr in seq_len(N_REPS)) {
      args_qg <- c(rf_base_args, list(
        train_data    = train_rf,
        test_data     = test_rf,
        training_days = nn,
        train_models  = TRUE,
        backend       = "quantgen",
        lp_solver     = "glpk",
        model_save_dir = dir_qg
      ))
      e2e_results <- rbind(e2e_results, data.frame(
        n_train = nn, backend = "quantgen", rep = rr,
        elapsed_sec = time_fit(\() do.call(revision_forecast, args_qg))
      ))
    }
  }
}

e2e_smry <- aggregate(elapsed_sec ~ n_train + backend, data = e2e_results,
                      FUN = function(xx) c(mean = mean(xx), sd = sd(xx)))
e2e_smry <- do.call(data.frame, e2e_smry)
names(e2e_smry) <- c("n_train", "backend", "mean_sec", "sd_sec")
e2e_smry$mean_ms <- round(e2e_smry$mean_sec * 1000, 1)
e2e_smry$sd_ms   <- round(e2e_smry$sd_sec  * 1000, 1)
print(e2e_smry[, c("n_train", "backend", "mean_ms", "sd_ms")], row.names = FALSE)

if (has_quantgen) {
  cat("\n=== End-to-end relative speed ===\n")
  for (nn in N_TRAIN) {
    qr <- e2e_smry$mean_sec[e2e_smry$n_train == nn & e2e_smry$backend == "quantreg"]
    qg <- e2e_smry$mean_sec[e2e_smry$n_train == nn & e2e_smry$backend == "quantgen"]
    if (length(qr) && length(qg) && qg > 0) {
      ratio <- qg / qr
      cat(sprintf("  n=%5d: quantreg is %.1fx %s\n", nn, max(ratio, 1/ratio),
                  ifelse(ratio >= 1, "faster", "slower")))
    }
  }
}

# --- Section 3: prediction agreement (all9 taus) ---
if (has_quantgen) {
  cat("\n=== Prediction agreement, all 9 taus (quantreg vs quantgen) ===\n")
  cat(sprintf("%-8s  %-10s  %-12s\n", "n_train", "pred_cor", "pred_maxdiff"))
  N_TEST <- 200
  for (nn in N_TRAIN) {
    set.seed(123)
    dat_tr <- make_data(nn)
    x_te   <- make_data(N_TEST)$x

    fit_qr <- fit_quantile_lasso(dat_tr$x, dat_tr$y, tau = TAUS_PROD, lambda = LAMBDA)
    fit_qg <- quantgen::quantile_lasso(dat_tr$x, dat_tr$y, tau = TAUS_PROD, lambda = LAMBDA,
                                       standardize = TRUE, intercept = TRUE, lp_solver = "glpk")

    pred_qr <- predict(fit_qr, newx = x_te)
    pred_qg <- predict(fit_qg, newx = x_te)

    cat(sprintf("%-8d  %-10.4f  %-12.4f\n", nn,
                cor(as.vector(pred_qr), as.vector(pred_qg)),
                max(abs(pred_qr - pred_qg))))
  }
}
