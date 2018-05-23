#' @export
leader_follower <- function(x, y, lag.max, id = 1:2, each = 1, circular = FALSE, progress = FALSE) {
  stopifnot(length(x) == length(y))

  sequence <- seq(2 * lag.max + 1, length(x) - 2 * lag.max, by = each)
  lags <- -lag.max:lag.max
  locs <- apply(t(sequence), 2, function(x, lag) (x - lag):(x + lag), lag = lag.max)
  mat_x <- matrix(x[locs], nrow = nrow(locs), ncol = ncol(locs))
  mat_res <- matrix(NA, nrow = 2 * lag.max + 1, ncol = length(sequence),
                    dimnames = list(lags, sequence))

  if (progress)
    pb <- progress::progress_bar$new(format = "(:spin) [:bar] :percent",
                                     total = length(lags), clear = FALSE, width = 60)

  for (i in 1:length(lags)) {
    tmp_y <- shift(y, lags[i])
    mat_y <- matrix(tmp_y[locs], nrow = nrow(locs), ncol = ncol(locs))

    if (circular) {
      mat_res[i, ] <- sapply(1:ncol(mat_x), function(j) suppressWarnings(circular::cor.circular(mat_x[, j], mat_y[, j])))
    } else {
      mat_res[i, ] <- sapply(1:ncol(mat_x), function(j) cor(mat_x[, j], mat_y[, j], use = "na.or.complete"))
    }

    if (progress)
      pb$tick()
  }

  max_lag <- lags[apply(mat_res, 2, function(v) {
    idx <- which.max(v)
    l <- length(idx)
    if (l == 1) {
      idx
    } else if (l == 0) {
      NA
    } else {
      idx[sample(1:l, 1)]
    }
  })]
  max_corr <- apply(mat_res, 2, max, na.rm = TRUE)

  # tibble::tibble(id = id, theta = mean(max_lag[max_corr > thres], na.rm = TRUE) * c(1, -1))
  tibble::tibble(time = sequence, max_lag = max_lag, max_corr = max_corr)
}


