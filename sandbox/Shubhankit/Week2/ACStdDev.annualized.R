#' calculate a multiperiod or annualized Autocorrleation adjusted Standard Deviation 
#'
#' @aliases sd.multiperiod sd.annualized StdDev.annualized
#' @param x an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param lag : number of autocorrelated lag factors inputted by user
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @param \dots any other passthru parameters
#' @author R
#' @seealso \code{\link[stats]{sd}} \cr
#' \url{http://wikipedia.org/wiki/inverse-square_law}
#' @references Burghardt, G., and L. Liu, \emph{ It's the Autocorrelation, Stupid (November 2012) Newedge
#' working paper.http://www.amfmblog.com/assets/Newedge-Autocorrelation.pdf \cr
#' @keywords ts multivariate distribution models
#' @examples
#' 
#'     data(edhec)
#'     ACsd.annualized(edhec,3)

#' 
#' @export
#' @rdname ACStdDev.annualized
ACStdDev.annualized <- ACsd.annualized <- ACsd.multiperiod <-
  function (x,lag, scale = NA, ...)
  {
    if(is.na(scale) && !xtsible(x))
      stop("'x' needs to be timeBased or xtsible, or scale must be specified." )
    
    if(is.na(scale)) {
      freq = periodicity(x)
      switch(freq$scale,
             #kChec
             minute = {stop("Data periodicity too high")},
             hourly = {stop("Data periodicity too high")},
             daily = {scale = 252},
             weekly = {scale = 52},
             monthly = {scale = 12},
             quarterly = {scale = 4},
             yearly = {scale = 1}
      )
    }
    
    if (is.vector(x)) {
      correl = acf(x,lag)
      
      #scale standard deviation by multiplying by the square root of the number of periods to scale by
      sqrt(scale*(1+ 2*sum(correl$acf[2:(lag+1)])))*sd(x, na.rm=TRUE)
    } else { 
      if(!xtsible(x) & is.na(scale))
        stop("'x' needs to be timeBased or xtsible, or scale must be specified." )
      x = checkData (x)
      
      result = apply(x, 2, sd.multiperiod, scale=scale)
      
      dim(result) = c(1,NCOL(x))
      colnames(result) = colnames(x)
      rownames(result) = "Annualized Standard Deviation"
      return(result)
    }
  }

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2013 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: ACStdDev.annualized.R 
#
###############################################################################
