###############################################################################
# Value at Risk and Expected Shortfall via Generalized Pareto Distribution (GPD)
#
# Original implementation by Talgat Daniyarov in the archived 'VaR' package.
# Ported and integrated into PerformanceAnalytics by Brian G. Peterson.
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
###############################################################################

.gpd.lik <- function(a, x, u) {
  # Log-likelihood function for the Generalized Pareto Distribution
  if (a[1] <= 0) {
    return(1e6)
  }
  n <- length(x)
  y <- (x - u) / a[1]
  if (a[2] == 0) {
    ret <- n * log(a[1]) + sum(y)
  } else {
    y <- 1 + a[2] * y
    if (any(y <= 0)) {
      return(1e6)
    }
    ret <- n * log(a[1]) + sum(log(y)) * (1 + 1 / a[2])
  }
  return(ret)
}

.gpd.fit <- function(R, p, p.tr = 0.97, init = c(1.00, 0.3)) {
  # Returns the threshold, shape, scale, and degrees of freedom for GPD
  alpha <- 1 - p

  # Scale by 100 to ensure optim() doesn't encounter floating point underflow
  # on tiny return decimals
  y <- -as.numeric(na.omit(R)) * 100

  y.stdv <- sd(y)
  threshold <- qnorm(p.tr) * y.stdv

  x.cut <- y[y > threshold]
  n.cut <- length(x.cut)

  if (n.cut < 2) {
    return(NULL) # Cannot fit GPD without sufficient tail data
  }

  nnu <- length(y) / n.cut
  fit <- stats::optim(init, function(a) .gpd.lik(a, x.cut, threshold), method = "Nelder-Mead")

  return(list(
    a = fit$par,
    threshold = threshold,
    nnu = nnu,
    alpha = alpha
  ))
}

VaR.gpd <- function(R, p, ...) {
  R <- checkData(R)
  result <- apply(R, 2, function(x) {
    fit <- .gpd.fit(x, p = p, ...)
    if (is.null(fit)) {
      return(NA)
    }

    a <- fit$a
    threshold <- fit$threshold
    nnu <- fit$nnu
    alpha <- fit$alpha

    y.VaR <- threshold + (a[1] / a[2]) * (((alpha * nnu)^(-a[2])) - 1)

    # Return to normal decimal format and invert
    return(-y.VaR / 100)
  })

  result <- matrix(result, nrow = 1)
  colnames(result) <- colnames(R)
  rownames(result) <- "VaR"
  return(result)
}

ES.gpd <- function(R, p, ...) {
  R <- checkData(R)
  result <- apply(R, 2, function(x) {
    fit <- .gpd.fit(x, p = p, ...)
    if (is.null(fit)) {
      return(NA)
    }

    a <- fit$a
    threshold <- fit$threshold
    nnu <- fit$nnu
    alpha <- fit$alpha

    y.VaR <- threshold + (a[1] / a[2]) * (((alpha * nnu)^(-a[2])) - 1)
    y.ES <- (y.VaR + a[1] - a[2] * threshold) / (1 - a[2])

    # Return to normal decimal format and invert
    return(-y.ES / 100)
  })

  result <- matrix(result, nrow = 1)
  colnames(result) <- colnames(R)
  rownames(result) <- "ES"
  return(result)
}
