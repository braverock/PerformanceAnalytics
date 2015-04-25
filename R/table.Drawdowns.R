#' Worst Drawdowns Summary: Statistics and Stylized Facts
#' 
#' Creates table showing statistics for the worst drawdowns.
#' 
#' Returns an data frame with columns: \cr 
#' \itemize{ 
#'   \item From starting period, high water mark 
#'   \item Trough period of low point 
#'   \item To ending period, when initial high water mark is recovered 
#'   \item Depth drawdown to trough (typically as percentage returns) 
#'   \item Length length in periods 
#'   \item toTrough number of periods to trough 
#'   \item Recovery number of periods to recover
#' }
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param top the number of drawdowns to include
#' @param digits number of digits to round results to
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic 
#' chaining (FALSE) to aggregate returns, default TRUE
#' @param \dots any other passthru parameters
#' 
#' @author Peter Carl
#' @seealso 
#' \code{\link{DownsideDeviation}} \cr 
#' \code{\link{maxDrawdown}} \cr
#' \code{\link{findDrawdowns}} \cr 
#' \code{\link{sortDrawdowns}} \cr
#' \code{\link{chart.Drawdown}} \cr 
#' \code{\link{table.DownsideRisk}}
#' @references Bacon, C. \emph{Practical Portfolio Performance Measurement and
#' Attribution}. Wiley. 2004. p. 88 \cr
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(edhec)
#' table.Drawdowns(edhec[,1,drop=FALSE])
#' table.Drawdowns(edhec[,12,drop=FALSE])
#' data(managers)
#' table.Drawdowns(managers[,8,drop=FALSE])
#' 
#' result=table.Drawdowns(managers[,1,drop=FALSE])
#' 
#' # This was really nice before Hmisc messed up 'format' from R-base
#' #require("Hmisc")
#' #textplot(Hmisc::format.df(result, na.blank=TRUE, numeric.dollar=FALSE, 
#' #           cdec=c(rep(3,4), rep(0,3))), rmar = 0.8, cmar = 1.5,  
#' #           max.cex=.9, halign = "center", valign = "top", row.valign="center", 
#' #           wrap.rownames=5, wrap.colnames=10, mar = c(0,0,3,0)+0.1) 
#' # title(main="Largest Drawdowns for HAM1")
#' 
#' @export
table.Drawdowns <-
function (R, top = 5, digits = 4, geometric=TRUE, ...)
{# @author Peter Carl

    # DESCRIPTION
    # Worst Drawdowns Summary: Statistics and Stylized Facts

    # Inputs:
    # R: Assumes returns rather than prices
    # top: the number of drawdowns to include

    # Output:
    # Creates a data.frame of the worst "n" drawdowns

    # FUNCTION:

    R = checkData(R[,1,drop=FALSE])
    R = na.omit(R)
    x = sortDrawdowns(findDrawdowns(R, geometric=geometric, ...))

    ndrawdowns = sum(x$return < 0)
    if (ndrawdowns < top){
        warning(paste("Only ",ndrawdowns," available in the data.",sep=""))
        top = ndrawdowns
    }

    result = data.frame(time(R)[x$from[1:top]], time(R)[x$trough[1:top]], time(R)[x$to[1:top]], base::round(x$return[1:top], digits), x$length[1:top], x$peaktotrough[1:top], ifelse(is.na(time(R)[x$to[1:top]]), NA, x$recovery[1:top]))

    colnames(result) = c("From", "Trough", "To", "Depth", "Length", "To Trough", "Recovery")
    dummy<-TRUE; if(!dummy) Depth<-NULL #dummy for R CMD check
    subset(result,subset=Depth<0)
    result
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2015 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
