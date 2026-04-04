###############################################################################
# Value at Risk and Expected Shortfall via Monte Carlo Simulation
#
# Original implementations adapted from standard mathematical definitions and
# integrated into PerformanceAnalytics by Brian G. Peterson.
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
###############################################################################

VaR.montecarlo <- function(R, p, nsim = 10000, ...) {
  R <- checkData(R)
  alpha <- 1 - p

  # Calculate multivariate mean vector and covariance matrix
  mu <- apply(R, 2, mean, na.rm = TRUE)
  sigma <- cov(R, use = "pairwise.complete.obs")

  # Generate multivariate normal synthetic return paths using Cholesky decomposition
  chol_sigma <- chol(sigma)
  sim_mat <- matrix(rnorm(nsim * ncol(R)), ncol = ncol(R))
  sim_returns <- sweep(sim_mat %*% chol_sigma, 2, mu, "+")

  # Extract the requested empirical quantile from the simulated distribution
  result <- apply(sim_returns, 2, quantile, probs = alpha, na.rm = TRUE)

  # Invert to match risk measurement reporting convention
  result <- -1 * result

  result <- matrix(result, nrow = 1)
  colnames(result) <- colnames(R)
  rownames(result) <- "VaR"
  return(result)
}

ES.montecarlo <- function(R, p, nsim = 10000, ...) {
  R <- checkData(R)
  alpha <- 1 - p

  mu <- apply(R, 2, mean, na.rm = TRUE)
  sigma <- cov(R, use = "pairwise.complete.obs")

  chol_sigma <- chol(sigma)
  sim_mat <- matrix(rnorm(nsim * ncol(R)), ncol = ncol(R))
  sim_returns <- sweep(sim_mat %*% chol_sigma, 2, mu, "+")

  # Evaluate Expected Shortfall empirically from the simulated distribution
  result <- apply(sim_returns, 2, function(x) {
    cutoff <- quantile(x, probs = alpha, na.rm = TRUE)
    mean(x[x <= cutoff], na.rm = TRUE)
  })

  # Invert to match risk measurement reporting convention
  result <- -1 * result

  result <- matrix(result, nrow = 1)
  colnames(result) <- colnames(R)
  rownames(result) <- "ES"
  return(result)
}

VaR.montecarlo.portfolio <- function(R, p, weights, nsim = 10000, ...) {
  R <- checkData(R)

  # Calculate multivariate mean vector and covariance matrix
  mu <- apply(R, 2, mean, na.rm = TRUE)
  sigma <- cov(R, use = "pairwise.complete.obs")

  # Generate multivariate normal synthetic return paths using Cholesky decomposition
  chol_sigma <- chol(sigma)
  sim_mat <- matrix(rnorm(nsim * ncol(R)), ncol = ncol(R))
  sim_returns <- sweep(sim_mat %*% chol_sigma, 2, mu, "+")
  colnames(sim_returns) <- colnames(R)

  # Convert to xts with dummy dates to reuse historical portfolio math
  sim_xts <- xts::xts(sim_returns, order.by = as.Date(1:nsim, origin = "2000-01-01"))

  res <- VaR.historical.portfolio(sim_xts, p, weights)
  names(res)[1] <- "VaR"
  names(res)[3] <- "pct_contrib_VaR"
  return(res)
}

ES.montecarlo.portfolio <- function(R, p, weights, nsim = 10000, ...) {
  R <- checkData(R)

  # Calculate multivariate mean vector and covariance matrix
  mu <- apply(R, 2, mean, na.rm = TRUE)
  sigma <- cov(R, use = "pairwise.complete.obs")

  # Generate multivariate normal synthetic return paths using Cholesky decomposition
  chol_sigma <- chol(sigma)
  sim_mat <- matrix(rnorm(nsim * ncol(R)), ncol = ncol(R))
  sim_returns <- sweep(sim_mat %*% chol_sigma, 2, mu, "+")
  colnames(sim_returns) <- colnames(R)

  # Convert to xts with dummy dates to reuse historical portfolio math
  sim_xts <- xts::xts(sim_returns, order.by = as.Date(1:nsim, origin = "2000-01-01"))

  res <- ES.historical.portfolio(sim_xts, p, weights)
  names(res)[1] <- "ES"
  names(res)[3] <- "pct_contrib_ES"
  return(res)
}
