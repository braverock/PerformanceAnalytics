#' correlation matrix chart
#'
#' Visualization of a Correlation Matrix. On top the (absolute) value of the
#' correlation plus the result of the cor.test as stars. On bottom, the
#' bivariate scatterplots, with a fitted line. When the \code{method} parameter
#' is set to "spearman" or "kendall", the lower panel scatterplots are automatically
#' transformed to plot the monotonically ranked data rather than the raw values,
#' providing visual alignment with the evaluated correlation method.
#'
#'
#' @param R data for the x axis, can take matrix,vector, or timeseries
#' @param histogram TRUE/FALSE whether or not to display a histogram
#' @param method a character string indicating which correlation coefficient
#'           (or covariance) is to be computed.  One of "pearson"
#'           (default), "kendall", or "spearman", can be abbreviated.
#' @param pch See \code{\link{par}}
#' @param \dots any other passthru parameters into \code{\link{pairs}}
#' @note based on plot at originally found at addictedtor.free.fr/graphiques/sources/source_137.R
#' @author Peter Carl
#' @seealso \code{\link{table.Correlation}} \code{\link{par}}
#'
### keywords ts multivariate distribution models hplot
#' @examples
#'
#' data(managers)
#' chart.Correlation(managers[, 1:8], histogram = TRUE, pch = "+")
#'
#' @export
chart.Correlation <-
  function(R, histogram = TRUE, method = c("pearson", "kendall", "spearman"), pch = 1, ...) { # @author R Development Core Team
    # @author modified by Peter Carl
    # Visualization of a Correlation Matrix. On top the (absolute) value of the
    # correlation plus the result of the cor.test as stars. On botttom, the
    # bivariate scatterplots, with a fitted line

    x <- checkData(R, method = "matrix")

    if (missing(method)) method <- method[1] # only use one
    cormeth <- method

    # Published at https://addictedtor.free.fr/graphiques/sources/source_137.R
    panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs", method = cormeth, cex.cor, ...) {
      usr <- par("usr")
      on.exit(par(usr = usr))
      par(usr = c(0, 1, 0, 1))
      r <- cor(x, y, use = use, method = method) # MG: remove abs here
      txt <- format(c(r, 0.123456789), digits = digits)[1]
      txt <- paste(prefix, txt, sep = "")
      if (missing(cex.cor)) cex <- 0.8 / strwidth(txt)

      test <- cor.test(as.numeric(x), as.numeric(y), method = method)
      # borrowed from printCoefmat
      Signif <- symnum(test$p.value,
        corr = FALSE, na = FALSE,
        cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
        symbols = c("***", "**", "*", ".", " ")
      )
      # MG: add abs here and also include a 30% buffer for small numbers
      text(0.5, 0.5, txt, cex = cex * (abs(r) + .3) / 1.3)
      text(.8, .8, Signif, cex = cex, col = 2)
    }
    f <- function(t) {
      dnorm(t, mean = mean(x), sd = sd.xts(x))
    }

    # Custom lower panel to support method-specific visual scatterplots
    panel.smooth.custom <- function(x, y, col = par("col"), bg = NA, pch = par("pch"),
                                    cex = 1, col.smooth = "red", span = 2 / 3, iter = 3, ...) {
      if (cormeth %in% c("spearman", "kendall")) {
        x_rk <- rank(x, na.last = "keep")
        y_rk <- rank(y, na.last = "keep")

        # Rescale ranks to exactly match the uniform data bounds drawn by pairs()
        x <- min(x, na.rm = TRUE) + (x_rk - min(x_rk, na.rm = TRUE)) /
          (max(x_rk, na.rm = TRUE) - min(x_rk, na.rm = TRUE)) *
          (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

        y <- min(y, na.rm = TRUE) + (y_rk - min(y_rk, na.rm = TRUE)) /
          (max(y_rk, na.rm = TRUE) - min(y_rk, na.rm = TRUE)) *
          (max(y, na.rm = TRUE) - min(y, na.rm = TRUE))
      }

      points(x, y, pch = pch, col = col, bg = bg, cex = cex)
      ok <- is.finite(x) & is.finite(y)
      if (any(ok)) {
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = col.smooth, ...
        )
      }
    }

    # remove method from dotargs
    dotargs <- list(...)
    dotargs$method <- NULL
    rm(method)

    hist.panel <- function(x, ... = NULL) {
      par(new = TRUE)
      hist(x,
        col = "light gray",
        probability = TRUE,
        axes = FALSE,
        main = "",
        breaks = "FD"
      )
      lines(density(x, na.rm = TRUE),
        col = "red",
        lwd = 1
      )
      rug(x)
    }

    # Draw the chart
    title_str <- paste(toupper(substr(cormeth, 1, 1)), substring(cormeth, 2), "Correlation")

    if (!hasArg("main")) {
      if (histogram) {
        pairs(x, gap = 0, lower.panel = panel.smooth.custom, upper.panel = panel.cor, diag.panel = hist.panel, pch = pch, main = title_str, ...)
      } else {
        pairs(x, gap = 0, lower.panel = panel.smooth.custom, upper.panel = panel.cor, pch = pch, main = title_str, ...)
      }
    } else {
      if (histogram) {
        pairs(x, gap = 0, lower.panel = panel.smooth.custom, upper.panel = panel.cor, diag.panel = hist.panel, pch = pch, ...)
      } else {
        pairs(x, gap = 0, lower.panel = panel.smooth.custom, upper.panel = panel.cor, pch = pch, ...)
      }
    }
  }

###############################################################################
# R (https://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2020 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
