`checkData` <-
function (R, method = "zoo", na.rm = FALSE, quiet = FALSE, ...)
{ # @author Peter Carl

    # Description:

    # This function was created to make the different kinds of data classes
    # at least _seem_ more fungible.  It allows the user to pass in a data
    # object without being concerned that the function requires a matrix,
    # data.frame, or timeSeries object.  By using this, the function "knows"
    # what data format it has to work with.

    # Inputs:
    # R: a matrix, data.frame or timeSeries object of data to be checked and
    # transformed if necessary.


    # Outputs:
    # Produces a data object with named rows and columns.

    # FUNCTION:

    # For matrixes and zoo objects, we test to see if there are rows, columns
    # and labels.  If there are not column labels, we provide them.  Row labels
    # we leave to the coersion functions.

    if(!is.zoo(R)){
        R = as.matrix(R)
    
        # Test for rows and columns
        if(is.null(ncol(R))) 
            stop("There don't seem to be any columns in the data provided.  If you are trying to pass in names from a zoo object with one column, you should use the form 'data.zoo[rows, columns, drop = FALSE]'.")
    
        if(is.null(nrow(R)))
            stop("No rows in the data provided.")
    
        # Test for rownames and column names
        if(is.null(colnames(R))) {
            columns = ncol(R)
            if(!quiet)
                warning("No column names in the data provided. To pass in names from a data.frame, you should use the form 'data[rows, columns, drop = FALSE]'.")
            columnnames = for(column in 1:columns) {paste("Column.",column,sep="")}
            colnames(R) = columnnames
        }
    
        if(is.null(rownames(R)))
            if(!quiet)
                warning("No row names in the data provided. To pass in names from a data.frame, you should use the form 'data[rows, columns, drop = FALSE]'.")
    
        # Coerce a zoo object from the matrix.
        # We fill in column names where needed, and let the coersion
        # function fill in rownames if they are missing.
        if (method == "zoo") {
            if(is.null(rownames(R)))
                R = zoo(R)
            else
                R = zoo(R, order.by = rownames(R))
        }
    }

    if (method == "matrix")
        R = as.matrix(R)

    # Now follows the tests specific to vectors
    if (method == "vector"){
    
        # First, we'll check to see if we have more than one column.
        if (NCOL(R) > 1) {
            if(!quiet)
                warning("The data provided is not a vector or univariate time series.  Used only the first column")
            R = R[,1]
        }
    
        # Second, we'll hunt for NA's and remove them if required
        if (any(is.na(R))) {
            if(na.rm) {
                # Try to remove any NA's
                R = R[!is.na(R)]
                if(!quiet){
                    warning("The following slots have NAs.")
                    warning(paste(R@na.removed," "))
                }
            }
            else {
                if(!quiet)
                    warning("Data contains NA's.")
            }
        }
    
        # Third, we'll check to see if we have any character data
        if (!is.numeric(R)){
            if(!quiet)
                warning("The data does not appear to be numeric.")
            # Try to coerce the data
            # x = as.numeric(x)
        }
    
        # Fourth, we'll see if we have more than one data point.
        if (NROW(R) <= 1) {
            if(!quiet)
                warning("Only one row provided.")
        }
    
        # @todo: Add check for stopifnot(is.atomic(y))???

        R = as.vector(R)
    }
    
    # RESULTS
    return(R)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: checkData.R,v 1.5 2007-03-16 03:22:13 peter Exp $
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.4  2007/03/13 17:58:46  peter
# - simplified to use as.matrix for all data types entered
#
# Revision 1.3  2007/03/13 03:56:28  peter
# - added warning for vector
#
# Revision 1.2  2007/03/13 03:44:24  peter
# - added log
#
###############################################################################