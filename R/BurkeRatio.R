#' Burke ratio of the return distribution
#'
#' To calculate Burke ratio we take the difference between the portfolio
#' return and the risk free rate and we divide it by the square root of the
#' sum of the square of the drawdowns. To calculate the modified Burke ratio
#' we just multiply the Burke ratio by the square root of the number of datas.
#'
#' \deqn{Burke Ratio = \frac{r_P - r_F}{\sqrt{\sum^{d}_{t=1}{D_t}^2}}}{Burke Ratio = (Rp - Rf) / (sqrt(sum(t=1..n)(Dt^2)))}
#'
#' \deqn{Modified Burke Ratio = \frac{r_P - r_F}{\sqrt{\sum^{d}_{t=1}\frac{{D_t}^2}{n}}}}{Modified Burke Ratio = (Rp - Rf) / (sqrt(sum(t=1..n)(Dt^2 / n)))}
#'
#' where \eqn{n} is the number of observations of the entire series, \eqn{d} is number of drawdowns, \eqn{r_P} is the portfolio return, \eqn{r_F} is the risk free rate and \eqn{D_t} the \eqn{t^{th}} drawdown.
#' 
#' @aliases BurkeRatio
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rf the risk free rate
#' @param modified a boolean to decide which ratio to calculate between Burke ratio and modified Burke ratio.
#' @param \dots any other passthru parameters
#' @author Matthieu Lestel
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.90-91
#' 
###keywords ts multivariate distribution models
#' @examples
#' data(portfolio_bacon)
#' print(BurkeRatio(portfolio_bacon[,1])) #expected 0.74
#' print(BurkeRatio(portfolio_bacon[,1], modified = TRUE)) #expected 3.65
#'
#' data(managers)
#' print(BurkeRatio(managers['1996']))
#' print(BurkeRatio(managers['1996',1])) 
#' print(BurkeRatio(managers['1996'], modified = TRUE))
#' print(BurkeRatio(managers['1996',1], modified = TRUE)) 
#'
#' @export 

BurkeRatio <- function (R, Rf = 0, modified = FALSE, ...)
{
    drawdown = c()
    R0 <- R
    R = checkData(R, method="matrix")
    if (ncol(R)==1 || is.null(R) || is.vector(R)) {
       calcul = FALSE
       n = length(R)

       number_drawdown = 0
       in_drawdown = FALSE
       peak = 1

        for (i in (1:length(R))) {
     	     if (!is.na(R[i])) {
     	    	calcul = TRUE
	     }
        }		      

       if(!calcul) {
            result = NA
	}
	else
	{
	period = Frequency(R)
       R = na.omit(R)
       for (i in (2:length(R))) {
          if (R[i]<0)
	  {
		if (!in_drawdown)
		{
			peak = i-1
			number_drawdown = number_drawdown + 1
			in_drawdown = TRUE
		}
	  }
	  else
	  {
	    if (in_drawdown)
	    {
	        temp = 1
		boundary1 = peak+1
		boundary2 = i-1
	        for(j in (boundary1:boundary2)) {
		     temp = temp*(1+R[j]*0.01)
		}
		drawdown = c(drawdown, (temp - 1) * 100)
		in_drawdown = FALSE
	    }
	  }
       }
	    if (in_drawdown)
	    {
	        temp = 1
		boundary1 = peak+1
		boundary2 = i
	        for(j in (boundary1:boundary2)) {
		     temp = temp*(1+R[j]*0.01)
		}
		drawdown = c(drawdown, (temp - 1) * 100)
		in_drawdown = FALSE
	    }

	    D = Drawdowns(R)

       Rp = (prod(1 + R))^(period / length(R)) - 1
         result = (Rp - Rf)/sqrt(sum(drawdown^2))
       if(modified)
       {
		result = result * sqrt(n)
       }
}
       return(result)
    }  
    else {
    	R = checkData(R)
        result = apply(R, MARGIN = 2, BurkeRatio, Rf = Rf, modified = modified, ...)
        result<-t(result)
        colnames(result) = colnames(R)
	if (modified)
	{
           rownames(result) = paste("Modified Burke ratio (Risk free = ",Rf,")", sep="")
	}
	else
	{
           rownames(result) = paste("Burke ratio (Risk free = ",Rf,")", sep="")
	}
        return(result)
    }
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
