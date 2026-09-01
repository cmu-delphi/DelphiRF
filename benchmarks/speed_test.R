# Quantile regression backend speed comparison: quantreg vs quantgen
# Run from the repo root: Rscript benchmarks/speed_test.R

devtools::load_all(".", quiet = TRUE)

set.seed(42)

N_TRAIN <- c(500, 2000, 5000)
N_COVARIATES <- 10
N_NONZERO  <- 3          # sparse ground truth: 3 of 10 coefficients active
TAU <- 0.5
LAMBDA <- 0.1
N_REPS <- 30

# Sparse ground-truth beta; active coefficients are +/-1
TRUE_BETA <- rep(0, N_COVARIATES)
TRUE_BETA[sample(N_COVARIATES, N_NONZERO)] <- sample(c(-1, 1), N_NONZERO, replace = TRUE)

make_data <- function(nn) {
  x <- matrix(rnorm(nn * N_COVARIATES), nrow = nn)
  # Asymmetric noise (t_3) makes quantile != mean, so quantile reg is non-trivial
  eps <- rt(nn, df = 3)
  y <- x %*% TRUE_BETA + eps
  list(x = x, y = as.numeric(y))
}

has_quantgen <- requireNamespace("quantgen", quietly = TRUE)
if (!has_quantgen) {
  message("quantgen not installed; only benchmarking quantreg backend")
}

results <- data.frame(
  n_train      = integer(),
  backend      = character(),
  rep          = integer(),
  elapsed_sec  = numeric()
)

for (nn in N_TRAIN) {
  dat <- make_data(nn)

  for (rr in seq_len(N_REPS)) {
    t0 <- proc.time()[["elapsed"]]
    fit_quantile_lasso(dat$x, dat$y, tau = TAU, lambda = LAMBDA)
    t1 <- proc.time()[["elapsed"]]
    results <- rbind(results, data.frame(
      n_train = nn, backend = "quantreg", rep = rr, elapsed_sec = t1 - t0
    ))
  }

  if (has_quantgen) {
    for (rr in seq_len(N_REPS)) {
      t0 <- proc.time()[["elapsed"]]
      quantgen::quantile_lasso(dat$x, dat$y, tau = TAU, lambda = LAMBDA,
                               standardize = TRUE, intercept = TRUE,
                               lp_solver = "glpk")
      t1 <- proc.time()[["elapsed"]]
      results <- rbind(results, data.frame(
        n_train = nn, backend = "quantgen", rep = rr, elapsed_sec = t1 - t0
      ))
    }
  }
}

summary_tbl <- aggregate(elapsed_sec ~ n_train + backend, data = results,
                         FUN = function(xx) c(mean = mean(xx), sd = sd(xx)))
summary_tbl <- do.call(data.frame, summary_tbl)
names(summary_tbl) <- c("n_train", "backend", "mean_sec", "sd_sec")
summary_tbl$mean_ms <- round(summary_tbl$mean_sec * 1000, 1)
summary_tbl$sd_ms   <- round(summary_tbl$sd_sec  * 1000, 1)

cat("\n=== Quantile LASSO backend speed (ms) ===\n")
cat(sprintf("Covariates: %d | Reps per cell: %d\n\n", N_COVARIATES, N_REPS))
print(summary_tbl[, c("n_train", "backend", "mean_ms", "sd_ms")], row.names = FALSE)

if (has_quantgen && nrow(summary_tbl) > 0) {
  cat("\n=== Relative speed (quantreg / quantgen) ===\n")
  for (nn in N_TRAIN) {
    qr_mean <- summary_tbl$mean_sec[summary_tbl$n_train == nn & summary_tbl$backend == "quantreg"]
    qg_mean <- summary_tbl$mean_sec[summary_tbl$n_train == nn & summary_tbl$backend == "quantgen"]
    if (length(qr_mean) && length(qg_mean) && qg_mean > 0) {
      ratio <- qg_mean / qr_mean
      if (ratio >= 1) {
        cat(sprintf("  n=%5d: quantreg is %.1fx faster\n", nn, ratio))
      } else {
        cat(sprintf("  n=%5d: quantreg is %.1fx slower\n", nn, 1 / ratio))
      }
    }
  }
}

# --- Result comparison ---
if (has_quantgen) {
  cat("\n=== Prediction agreement (quantreg vs quantgen) ===\n")
  cat(sprintf("%-8s  %-10s  %-10s  %-10s  %-10s\n",
              "n_train", "coef_cor", "pred_cor", "coef_maxdiff", "pred_maxdiff"))

  N_TEST <- 200
  for (nn in N_TRAIN) {
    set.seed(123)
    dat_tr <- make_data(nn)
    dat_te <- make_data(N_TEST)
    x_tr <- dat_tr$x; y_tr <- dat_tr$y
    x_te <- dat_te$x

    fit_qr <- fit_quantile_lasso(x_tr, y_tr, tau = TAU, lambda = LAMBDA)
    fit_qg <- quantgen::quantile_lasso(x_tr, y_tr, tau = TAU, lambda = LAMBDA,
                                       standardize = TRUE, intercept = TRUE,
                                       lp_solver = "glpk")

    pred_qr <- predict(fit_qr, newx = x_te)
    pred_qg <- predict(fit_qg, newx = x_te)

    # quantgen returns a matrix (n x n_lambda); extract the single column
    if (is.matrix(pred_qg)) pred_qg <- pred_qg[, 1]

    coef_qr <- c(fit_qr$coefficients)
    coef_qg <- as.vector(coef(fit_qg))

    cat(sprintf("%-8d  %-10.4f  %-10.4f  %-10.4f  %-10.4f\n",
                nn,
                cor(coef_qr, coef_qg),
                cor(pred_qr, pred_qg),
                max(abs(coef_qr - coef_qg)),
                max(abs(pred_qr - pred_qg))))
  }
}
