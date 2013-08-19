#'@title
#'Penance vs phi plot
#'
#'@description
#'
#'A plot for Penance vs phi for the given portfolio
#'The relationship between penance and phi is given by
#'
#'\deqn{penance = \frac{Maximum Drawdown}{Maximum Time Under Water}}
#'
#'Penance Measures how long it takes to recover from the maximum drawdown
#'as a multiple of the time it took to reach the bottom. Penance is smaller,
#'the higher the value of \eqn{\phi(Phi)} and the higher the ratio \eqn{\frac{\mu}{\sigma}}.
#'Positive serial autocorrelation leads to smaller Penance due to greater periods under 
#'water.
#'@param R an xts, vector, matrix, data frame,
#'timeSeries or zoo object of asset returns.
#'@param confidence the confidence level
#'@param type The type of distribution "normal" or "ar"."ar" stands for Autoregressive.
#'@param reference.grid if true, draws a grid aligned with the points on the x
#'and y axes
#'@param ylab set the y-axis label, as in \code{\link{plot}}
#'@param xlab set the x-axis label, as in \code{\link{plot}}
#'@param main set the chart title, as in \code{\link{plot}}
#'@param element.color set the element.color value as in \code{\link{plot}}
#'@param lwd set the width of the line, as in \code{\link{plot}}
#'@param pch set the pch value, as in \code{\link{plot}}
#'@param cex set the cex value, as in \code{\link{plot}}
#'@param cex.axis set the cex.axis value, as in \code{\link{plot}}
#'@param cex.main set the cex.main value, as in \code{\link{plot}}
#'@param ylim set the ylim value, as in \code{\link{plot}}
#'@param xlim set the xlim value, as in \code{\link{plot}}
#'
#'@author Pulkit Mehrotra
#'@seealso \code{\link{plot}} \code{\link{table.Penance}} \code{\link{MaxDD}} \code{\link{TuW}}
#'@keywords ts multivariate distribution models hplot
#'@examples
#'chart.Penance(edhec,0.95)
#'
#'@references Bailey, David H. and Lopez de Prado, Marcos,Drawdown-Based Stop-Outs and the ‘Triple Penance’ Rule(January 1, 2013).
#'
#'@export

chart.Penance<-function(R,confidence,type=c("ar","normal"),reference.grid = TRUE,main=NULL,ylab = NULL,xlab = NULL,element.color="darkgrey",lwd = 2,pch = 1,cex = 1,cex.axis=0.8,cex.lab = 1,cex.main = 1,xlim = NULL,ylim = NULL,...){

  # DESCRIPTION:
  # Draws the scatter plot of Phi vs Penance.
  
  # INPUT:
  # The Return Series of the portfolio is taken as the input. The Return 
  # Series can be an xts, vector, matrix, data frame, timeSeries or zoo object of
  # asset returns. The type of distribution , "normal" or non-normal "ar", The confidence 
  # level
  
  # All other inputs are the same as "plot" and are principally included
  # so that some sensible defaults could be set.
  
  # Output:
  # Draws the scatter plot of Phi vs Penance with some sensible defaults.
  
  # FUNCTION:
  
    x = checkData(R)
    columns = ncol(x)
    columnnames = colnames(x)
    phi = 1:columns
    penance = 1:columns
    for(column in 1:columns){
        phi[column] = cov(x[,column][-1],x[,column][-length(x[,column])])/(cov(x[,column][-length(x[,column])]))
        penance[column]<-MaxDD(x[,column],confidence,type = type)[1]/TuW(x[,column],confidence,type = type)
    }
    if(is.null(ylab)){
      ylab = "Penance"
    }
    if(is.null(xlab)){
      xlab = "Phi"
    }
    if(is.null(main)){
      main = "Penance vs Phi"
    }
    
    plot(x=phi,y=penance,xlab=xlab,ylab=ylab,main=main,lwd = lwd,pch=pch,cex = cex,cex.lab = cex.lab)
    text(phi,penance,columnnames,pos = 4,col=c(1:columns),cex = 0.8)
    if(reference.grid) {
      grid(col = element.color)
      abline(h = 0, col = element.color)
      abline(v = 0, col = element.color)
    }
}





