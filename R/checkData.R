#' check input data type and format and coerce to the desired output type
#' 
#' This function was created to make the different kinds of data classes at
#' least \emph{seem} more fungible.  It allows the user to pass in a data
#' object without being concerned that the function requires a matrix,
#' data.frame, vector, xts, or timeSeries object.  By using \code{checkData}, 
#' the function "knows" what data format it has to work with.
#' 
#' 
#' @param x a vector, matrix, data.frame, xts, timeSeries or zoo object to be
#' checked and coerced
#' @param na.rm TRUE/FALSE Remove NA's from the data? used only with 'vector'
#' @param quiet TRUE/FALSE if false, it will throw warnings when errors are
#' noticed, default TRUE
#' @param method type of coerced data object to return, one of
#' c("xts", "zoo", "data.frame", "matrix", "vector"), default "xts"
#' @param \dots any other passthru parameters
#' @author Peter Carl
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(edhec)
#' x = checkData(edhec)
#' class(x)
#' head(x)
#' tail(x)
#' # Note that passing in a single column loses the row and column names
#' x = checkData(edhec[,1])
#' class(x)
#' head(x)
#' # Include the "drop" attribute to keep row and column names
#' x = checkData(edhec[,1,drop=FALSE])
#' class(x)
#' head(x)
#' x = checkData(edhec, method = "matrix")
#' class(x)
#' head(x)
#' x = checkData(edhec[,1], method = "vector")
#' class(x)
#' head(x)
#' 
#' @export 
checkData <- function (x, method = c("xts", "zoo", "data.frame", "matrix", "vector"), na.rm = TRUE, quiet = TRUE, ...)
{ # @author Peter Carl

    # Description:

    # This function was created to make the different kinds of data classes
    # at least _seem_ more fungible.  It allows the user to pass in a data
    # object without being concerned that the function requires a matrix,
    # data.frame, or timeSeries object.  By using this, the function "knows"
    # what data format it has to work with.

    # Inputs:
    # x: a matrix, data.frame or timeSeries object of data to be checked and
    # transformed if necessary.

    # Outputs:
    # Produces a data object with named rows and columns.

    # FUNCTION:

    method = method[1]

    # Function is organized by the target format
    switch(method,
        vector = {
            # First, we'll check to see if we have more than one column.
            if (NCOL(x) > 1) {
                if(!quiet)
                    warning("The data provided is not a vector or univariate time series.  Used only the first column")
                x = x[,1]
            }
            if (na.rm) 
                x = na.omit(x)
            x = as.vector(x)
        },
        matrix = {
            x = as.matrix(x, ncol = NCOL(x))
        },
        data.frame = {
            x = as.data.frame(x)
        },
        zoo = {
            if(inherits(x, what="zoo")){ # xts or zoo object
                x = as.zoo(x)
            }
            else {
                if(class(x) == "matrix" | class(x) == "data.frame"){
                    x= zoo(x, order.by = as.POSIXct(rownames(x)))  
                }
                else{
                    if(class(x) == "numeric"){
                        if(is.null(names(x)))
                            x= zoo(matrix(x, ncol=NCOL(x)))
                        else
                            x= zoo(matrix(x, ncol=NCOL(x)), order.by=as.POSIXct(names(x)))
                    }
                }
            }
        },
        xts = {
			if(is.xts(x)) return(x)
            if(!xtsible(x))
                if(class(x) == "numeric"){
                    x= zoo(matrix(x, ncol=NCOL(x)))
                    if(!quiet)
                        warning("The data cannot be converted into a time series.  Returning a 'zoo' object. ")
                }
                else
                    stop("The data cannot be converted into a time series.  If you are trying to pass in names from a data object with one column, you should use the form \'data[rows, columns, drop = FALSE]\'.  Rownames should have standard date formats, such as '1985-03-15'. ")
            else x = try.xts(x)
        }
    ) # end switch

    # RESULTS
    return(x)

}

###############################################################################

checkDataMatrix <-
function (x, na.rm = TRUE, quiet = TRUE, ...)
{
    checkData(x, method = "matrix", na.rm = na.rm, quiet = quiet, ...)
}

###############################################################################

checkDataVector <-
function (x, na.rm = TRUE, quiet = TRUE, ...)
{
    checkData(x, method = "vector", na.rm = na.rm, quiet = quiet, ...)
}

###############################################################################

checkDataZoo <- 
function (x, na.rm = TRUE, quiet = TRUE, ...)
{
    checkData(x, method = "zoo", na.rm = na.rm, quiet = quiet, ...)
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
###############################################################################
