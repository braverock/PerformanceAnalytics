#' Stacked Bar Plot of Autocorrelation Lag Coefficients
#' 
#' A wrapper to create box and whiskers plot of comparitive inputs
#' 
#' We have also provided controls for all the symbols and lines in the chart.
#' One default, set by \code{as.Tufte=TRUE}, will strip chartjunk and draw a
#' Boxplot per recommendations by Burghardt, Duncan and Liu(2013)
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' an asset return
#' @return Stack Bar plot of lagged return  coefficients
#' @author R 
#' @seealso \code{\link[graphics]{boxplot}}
#' @references Burghardt, Duncan and Liu(2013)  \emph{It's the autocorrelation, stupid}. AlternativeEdge Note  November, 2012 }
#' @keywords Autocorrelation lag factors
#' @examples
#' 
#' data(edhec[,1])
#' chart.Autocorrelation(edhec[,1])
#' 
#' 
#' @export 
chart.Autocorrelation <-
  function (R, ...)
  { # @author R
    
    # DESCRIPTION:
    # A wrapper to create box and whiskers plot, of autocorrelation lag coeffiecients
    # of the First six factors
    
    R = checkData(R, method="xts")
    
# Graph autos with adjacent bars using rainbow colors
 
aa= table.Autocorrelation(R)
    barplot(as.matrix(aa), main="ACF Lag Plot", ylab= "Value of Coefficient",
                   , xlab = NULL,col=rainbow(6))

   # Place the legend at the top-left corner with no frame  
   # using rainbow colors
   legend("topright", c("1","2","3","4","5","6"), cex=0.6, 
                   bty="n", fill=rainbow(6));




}