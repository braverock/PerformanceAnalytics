even <- function(x) x %% 2 == 0

odd <- function(x) x %% 2 == 1

sd.xts <- xts:::sd.xts

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("returns", "security"))
}

#' @importFrom methods hasArg
#' @importFrom utils globalVariables head packageDescription tail
#' @importFrom stats Box.test acf approxfun arima coef coefficients cor cor.test
#'             cov dcauchy density dlnorm dnorm fitted frequency lag lm loess.smooth
#'             median na.omit pacf pchisq pnorm ppoints predict prop.test qchisq qnorm
#'             qqline qqnorm qt quantile rnorm sd start symnum time uniroot var window runif
#' @importFrom grDevices recordPlot colorRamp rgb xy.coords
#' @importFrom graphics abline axis barplot box boxplot curve grid hist identify
#'             layout lines mtext pairs panel.smooth par plot plot.new
#'             plot.window plot.xy points rect rug segments strheight strwidth
#'             text title xinch yinch

#' @importFrom quadprog solve.QP
#' @import xts
#' @import zoo
NULL

###############################################################################
# R (https://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2026 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
