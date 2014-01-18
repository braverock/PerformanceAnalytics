#' Skewness
#' 
#' compute skewness of a univariate distribution.
#' 
#' This function was ported from the RMetrics package fUtilities to eliminate a
#' dependency on fUtiltiies being loaded every time.  The function is identical
#' except for the addition of \code{\link{checkData} and column support.}
#'
#' \deqn{Skewness(moment) = \frac{1}{n}*\sum^{n}_{i=1}(\frac{r_i - \overline{r}}{\sigma_P})^3}{Skewness(moment) = sum((x-mean(x))^3/var(x)^(3/2))/length(x)}
#' \deqn{Skewness(sample) =  \frac{n}{(n-1)*(n-2)}*\sum^{n}_{i=1}(\frac{r_i - \overline{r}}{\sigma_{S_P}})^3 }{skewness(sample) = sum(((x-mean(x))/var(x))^3)*n/((n-1)*(n-2))}
#' \deqn{Skewness(fisher) = \frac{\frac{\sqrt{n*(n-1)}}{n-2}*\sum^{n}_{i=1}\frac{x^3}{n}}{\sum^{n}_{i=1}(\frac{x^2}{n})^{3/2}}}{Skewness(fisher)((sqrt(n*(n-1))/(n-2))*(sum(x^3)/n))/((sum(x^2)/n)^(3/2))}
#'
#' where \eqn{n} is the number of return, \eqn{\overline{r}} is the mean of the return
#' distribution, \eqn{\sigma_P} is its standard deviation and \eqn{\sigma_{S_P}} is its
#' sample standard deviation
#' 
#' @param na.rm a logical. Should missing values be removed?
#' @param method a character string which specifies the method of computation.
#' These are either \code{"moment"} or \code{"fisher"} The \code{"moment"}
#' method is based on the definitions of skewnessfor distributions; these forms
#' should be used when resampling (bootstrap or jackknife). The \code{"fisher"}
#' method correspond to the usual "unbiased" definition of sample variance,
#' although in the case of skewness exact unbiasedness is not possible. The 
#' \code{"sample"} method gives the sample skewness of the distribution.
#' @param x a numeric vector or object.
#' @param \dots arguments to be passed.
#' @author Diethelm Wuertz, Matthieu Lestel
#' @seealso
#' 
#' \code{\link{kurtosis}}
#' @references Carl Bacon, \emph{Practical portfolio performance measurement 
#' and attribution}, second edition 2008 p.83-84
#' @keywords univar
#' @examples
#' 
#' ## mean -
#' ## var -
#'    # Mean, Variance:
#'    r = rnorm(100)
#'    mean(r)
#'    var(r)
#' 
#' ## skewness -
#'    skewness(r)
#' data(managers)
#' skewness(managers)
#' 
#' @export
skewness <-
    function (x, na.rm = FALSE, method = c("moment", "fisher", "sample"), ...)
{
    # @author Diethelm Wuertz
    # @author Brian Peterson   (modify for PerformanceAnalytics)

    # Description:
    #   Returns the value of the skewness of a distribution function.

    # Details:
    #   Missing values can be handled.

    # FUNCTION:

    # Method:
    method = match.arg(method)

    R=checkData(x,method="matrix")

    columns = ncol(R)
    columnnames=colnames(R)
    # FUNCTION:
    for(column in 1:columns) {
        x = as.vector(na.omit(R[,column]))
        #x = R[,column]

        if (!is.numeric(x)) stop("The selected column is not numeric")

        # Remove NAs:
        if (na.rm) x = x[!is.na(x)]

        # Warnings:
        if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
            warning("argument is not numeric or logical: returning NA")
            return(as.numeric(NA))}

        # Skewness:
        n = length(x)
        if (is.integer(x)) x = as.numeric(x)

        # Selected Method:
        if (method == "moment") {
            skewness = sum((x-mean(x))^3/sqrt(var(x)*(n-1)/n)^3)/length(x)
        }
        if (method == "fisher") {
            if (n < 3)
                skewness = NA
            else
                skewness = ((sqrt(n*(n-1))/(n-2))*(sum(x^3)/n))/((sum(x^2)/n)^(3/2))
        }
	if (method == "sample") {
            skewness = sum((x-mean(x))^3/sqrt(var(x)*(n-1)/n)^3)*n/((n-1)*(n-2))
	}

        skewness=array(skewness)
        if (column==1) {
            #create data.frame
            result=data.frame(skewness=skewness)
        } else {
            skewness=data.frame(skewness=skewness)
            result=cbind(result,skewness)
        }

    } #end columns loop

    if(ncol(result) == 1) {
        # some backflips to name the single column zoo object
        result = as.numeric(result)
    }
    else{
        colnames(result) = columnnames
        rownames(result) = "Skewness"
    }
    # Return Value:
    result

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
