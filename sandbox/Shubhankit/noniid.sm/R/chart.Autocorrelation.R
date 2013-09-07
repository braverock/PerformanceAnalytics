#' @title Stacked Bar Autocorrelation Plot
#' 
#' @description A wrapper to create box and whiskers plot of lagged autocorrelation analysis
#' 
#' @details We have also provided controls for all the symbols and lines in the chart.
#' One default, set by \code{as.Tufte=TRUE}, will strip chartjunk and draw a
#' Boxplot per recommendations by Burghardt, Duncan and Liu(2013)
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' an asset return
#' @return Stack Bar plot of lagged return  coefficients
#' @author Peter Carl, Brian Peterson, Shubhankit Mohan 
#' @seealso \code{\link[graphics]{boxplot}}
#' @references Burghardt, G., and L. Liu, \emph{ It's the Autocorrelation, Stupid (November 2012) Newedge
#' working paper.}
#' Paper Available at : \url{http://www.amfmblog.com/assets/Newedge-Autocorrelation.pdf}
#' @keywords Autocorrelation lag factors
#' @examples
#' 
#' data(edhec)
#' chart.Autocorrelation(edhec[,1])
#' 
#' @rdname chart.Autocorrelation
#' @export 
chart.Autocorrelation <-
  function (R)
  { # @author R
    
    # DESCRIPTION:
    # A wrapper to create box and whiskers plot, of autocorrelation lag coeffiecients
    # of the First six factors
    
    # R = checkData(R, method="xts")
    
# Graph autos with adjacent bars using rainbow colors
 
aa= table.Autocorrelation(R)
    chart.StackedBar(as.matrix(aa), main="ACF Lag Plot", ylab= "Value of Coefficient",
                   , xlab = NULL,colorset=bluemono)

   # Place the legend at the top-left corner with no frame  
   # using rainbow colors
   #legend("topright", c("1","2","3","4","5","6"), cex=0.6, 
#                   bty="n", fill=rich6equal);




}
###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Chart.Autocorrelation.R 2271 2012-09-02 01:56:23Z braverock $
#
###############################################################################
