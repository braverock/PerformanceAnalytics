###############################################################################
# Value at Risk and Expected Shortfall via Lognormal Distribution
#
# Original implementation by Talgat Daniyarov in the archived 'VaR' package.
# Ported and integrated into PerformanceAnalytics by Brian G. Peterson.
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
###############################################################################

VaR.lognormal <- function(R, p, ...) {
  R <- checkData(R)
  alpha <- 1 - p

  result <- apply(R, 2, function(x) {
    x <- na.omit(x)

    # Convert arithmetic returns to geometric (log) returns
    y <- log(1 + x)

    y.mean <- mean(y)
    y.stdv <- sd(y)

    # Use exponential function to recover the arithmetic return threshold
    # Formula derived from Daniyarov's VaR.norm approximation
    # Reversing the threshold into a negative return scale
    y.VaR <- 1 - exp(y.mean - y.stdv * qnorm(p))
    return(y.VaR)
  })

  result <- matrix(result, nrow = 1)
  colnames(result) <- colnames(R)
  rownames(result) <- "VaR"
  return(result)
}

ES.lognormal <- function(R, p, ...) {
  R <- checkData(R)
  alpha <- 1 - p

  result <- apply(R, 2, function(x) {
    x <- na.omit(x)

    # Convert arithmetic returns to geometric (log) returns
    y <- log(1 + x)

    y.mean <- mean(y)
    y.stdv <- sd(y)

    # Expected shortfall of the lognormal distribution
    # E[X | X < q] where X ~ Lognormal(mu, sigma^2)
    # Computed using the closed-form lognormal tail expectation formula
    es_ratio <- exp(y.mean + y.stdv^2 / 2) * pnorm(qnorm(alpha) - y.stdv) / alpha

    y.ES <- 1 - es_ratio
    return(y.ES)
  })

  result <- matrix(result, nrow = 1)
  colnames(result) <- colnames(R)
  rownames(result) <- "ES"
  return(result)
}
