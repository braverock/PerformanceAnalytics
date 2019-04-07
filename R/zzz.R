.onLoad <- function(lib, pkg)
{   
    # Startup Mesage and Desription:
    MSG <- if(getRversion() >= "2.5") packageStartupMessage else message
    dsc <- packageDescription(pkg)
    if(interactive() || getOption("verbose")) { 
        # not in test scripts
        MSG(paste("\nPackage ", pkg, " (",dsc$Version,") loaded.\n",
            # dsc$Title, "\n", 
            "Copyright (c) 2004-2018 Peter Carl and Brian G. Peterson, ", 
            dsc$License, "\n", dsc$URL,
            "\n", sep=""))
    }
}

even <- function (x) x%%2==0

odd  <- function (x) x%%2==1

sd.xts <- xts:::sd.xts

#' @importFrom methods hasArg
#' @importFrom utils head packageDescription tail
#' @importFrom stats Box.test acf approxfun arima coef coefficients cor cor.test
#'             cov dcauchy density dlnorm dnorm fitted frequency lag lm loess.smooth
#'             median na.omit pacf pchisq pnorm ppoints predict qchisq qnorm
#'             qqline qqnorm qt quantile sd start symnum time var window
#' @importFrom graphics abline axis barplot box boxplot curve grid hist identify
#'             layout lines mtext pairs panel.smooth par plot plot.new
#'             plot.window plot.xy points rect rug segments strheight strwidth
#'             text title xinch yinch
#' @importFrom grDevices colorRamp rgb xy.coords
#' @importFrom quadprog solve.QP
#' @import xts
#' @import zoo
NULL

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2018 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
