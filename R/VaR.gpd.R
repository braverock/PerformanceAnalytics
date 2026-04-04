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

.pdfVaReps <- function(y, eps, VaR, u, p) {
  if (eps == 0.00) {
    out <- -p * exp(y / (VaR - u)) / (VaR - u)
  } else {
    out1 <- (-1.0 + (p)^(-eps)) / (eps * (VaR - u))
    out2 <- (1.0 + out1 * eps * y)^(-1.0 / eps - 1.0)
    out <- out1 * out2
  }
  return(out)
}

.pdfESeps <- function(y, eps, ES, u, p) {
  out1 <- (eps + p^(-eps) - 1) / (1 - eps) / (ES - u)
  out <- (out1 / eps) * ((1 + out1 * y)^(-1 / eps - 1))
  return(out)
}

.gpd.fit <- function(R, p, p.tr = 0.97, init = c(1.00, 0.3), cflevel = 0.95, SE = FALSE) {
  # Returns the threshold, shape, scale, degrees of freedom, and optional CI for GPD
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

  a <- fit$par
  y.VaR <- threshold + (a[1] / a[2]) * (((alpha * nnu)^(-a[2])) - 1)
  y.ES <- (y.VaR + a[1] - a[2] * threshold) / (1 - a[2])

  VaR.interval <- NULL
  ES.interval <- NULL

  if (SE) {
    # Profile Log-Likelihood for VaR Confidence Interval
    VaR.LogLik <- function(R) {
      sum(log(.pdfVaReps((x.cut - threshold), a[2], R, threshold, alpha * nnu)))
    }
    VaR.LogLikDiff <- function(R) {
      val <- VaR.LogLik(R) - VaR.LogLik(y.VaR) + 0.5 * qchisq(cflevel, df = 1)
      if (is.nan(val)) val <- -1000000
      return(val)
    }

    # Safely find roots for confidence bounds
    tryCatch(
      {
        VaR.low <- uniroot(VaR.LogLikDiff, c(0.01, y.VaR))
        VaR.big <- uniroot(VaR.LogLikDiff, c(y.VaR, 99.99))
        VaR.interval <- c(VaR.low$root, VaR.big$root) / 100
      },
      error = function(e) {
        VaR.interval <<- c(NA, NA)
      }
    )

    # Profile Log-Likelihood for ES Confidence Interval
    ES.LogLik <- function(R) {
      sum(log(.pdfESeps((x.cut - threshold), a[2], R, threshold, alpha * nnu)))
    }
    ES.LogLikDiff <- function(R) {
      val <- ES.LogLik(R) - ES.LogLik(y.ES) + 0.5 * qchisq(cflevel, df = 1)
      if (is.nan(val)) val <- -1000000
      return(val)
    }

    tryCatch(
      {
        ES.low <- uniroot(ES.LogLikDiff, c(0.01, y.ES))
        ES.big <- uniroot(ES.LogLikDiff, c(y.ES, 99.99))
        ES.interval <- c(ES.low$root, ES.big$root) / 100
      },
      error = function(e) {
        ES.interval <<- c(NA, NA)
      }
    )
  }

  return(list(
    a = a,
    threshold = threshold,
    nnu = nnu,
    alpha = alpha,
    y.VaR = y.VaR,
    y.ES = y.ES,
    VaR.interval = VaR.interval,
    ES.interval = ES.interval
  ))
}

VaR.gpd <- function(R, p, SE = FALSE, ...) {
  R <- checkData(R)

  if (SE) {
    res_list <- lapply(1:ncol(R), function(i) {
      fit <- .gpd.fit(R[, i], p = p, SE = TRUE, ...)
      if (is.null(fit)) {
        return(c(VaR = NA, LCL = NA, UCL = NA))
      }
      # Inverting the bounds as well to match the return format
      return(c(VaR = -fit$y.VaR / 100, LCL = -fit$VaR.interval[2], UCL = -fit$VaR.interval[1]))
    })
    result <- do.call(cbind, res_list)
    colnames(result) <- colnames(R)
    rownames(result) <- c("VaR", "LCL", "UCL")
    return(result)
  } else {
    result <- apply(R, 2, function(x) {
      fit <- .gpd.fit(x, p = p, SE = FALSE, ...)
      if (is.null(fit)) {
        return(NA)
      }
      return(-fit$y.VaR / 100)
    })
    result <- matrix(result, nrow = 1)
    colnames(result) <- colnames(R)
    rownames(result) <- "VaR"
    return(result)
  }
}

ES.gpd <- function(R, p, SE = FALSE, ...) {
  R <- checkData(R)

  if (SE) {
    res_list <- lapply(1:ncol(R), function(i) {
      fit <- .gpd.fit(R[, i], p = p, SE = TRUE, ...)
      if (is.null(fit)) {
        return(c(ES = NA, LCL = NA, UCL = NA))
      }
      return(c(ES = -fit$y.ES / 100, LCL = -fit$ES.interval[2], UCL = -fit$ES.interval[1]))
    })
    result <- do.call(cbind, res_list)
    colnames(result) <- colnames(R)
    rownames(result) <- c("ES", "LCL", "UCL")
    return(result)
  } else {
    result <- apply(R, 2, function(x) {
      fit <- .gpd.fit(x, p = p, SE = FALSE, ...)
      if (is.null(fit)) {
        return(NA)
      }
      return(-fit$y.ES / 100)
    })
    result <- matrix(result, nrow = 1)
    colnames(result) <- colnames(R)
    rownames(result) <- "ES"
    return(result)
  }
}
