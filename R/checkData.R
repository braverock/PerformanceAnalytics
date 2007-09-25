`checkData` <-
function (x, method = c("zoo","matrix","vector","data.frame"), na.rm = FALSE, quiet = TRUE, ...)
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

    method = method[1] # grab the first value if this is still a vector, to avoid varnings

    # For matrixes and zoo objects, we test to see if there are rows, columns
    # and labels.  If there are not column labels, we provide them.  Row labels
    # we leave to the coersion functions.

    if(!is.zoo(x)){
        x = as.matrix(x)

        # Test for rows and columns
        if(is.null(ncol(x)))
            stop("There don\'t seem to be any columns in the data provided.  If you are trying to pass in names from a zoo object with one column, you should use the form \'data.zoo[rows, columns, drop = FALSE]\'.")

        if(is.null(nrow(x)))
            stop("No rows in the data provided.")

        # Test for rownames and column names
        if(method != "vector" & is.null(colnames(x))) {
            columns = ncol(x)
            if(!quiet)
                warning("No column names in the data provided. To pass in names from a data.frame, you should use the form \'data[rows, columns, drop = FALSE]\'.")
            columnnames = for(column in 1:columns) {paste("Column.",column,sep="")}
            colnames(x) = columnnames
        }

        if(method != "vector" & is.null(rownames(x)))
            if(!quiet)
                warning("No row names in the data provided. To pass in names from a data.frame, you should use the form \'data[rows, columns, drop = FALSE]\'.")

        # Coerce a zoo object from the matrix.
        # We fill in column names where needed, and let the coersion
        # function fill in rownames if they are missing.
        if (method == "zoo") {
            if(is.null(rownames(x)))
                x = zoo(x)
            else
                x = zoo(x, order.by = rownames(x))
        }
    }
    else {
        if(is.null(ncol(x)) & dim(as.matrix(x))[2] == 1) {
            #warning("If you are trying to pass in names from a zoo object with one column, you should use the form 'data.zoo[rows, columns, drop = FALSE]'.")
            x = as.matrix(x)
            colnames(x) = "Column"
            x = zoo(x, order.by = rownames(x))
        }
    }

    if (method == "matrix")
        x = as.matrix(x)


    if (method == "data.frame")
        x = as.data.frame(x)

    # Now follows the tests specific to vectors
    if (method == "vector"){

        # First, we'll check to see if we have more than one column.
        if (NCOL(x) > 1) {
            if(!quiet)
                warning("The data provided is not a vector or univariate time series.  Used only the first column")
            x = x[,1]
        }

        # Second, we'll hunt for NA's and remove them if required
        if (any(is.na(x))) {
            if(na.rm) {
                # Try to remove any NA's
                x = na.omit(x)
                if(!quiet){
                    warning("The following slots have NAs.")
                    warning(paste(x@na.removed," "))
                }
            }
            else {
                if(!quiet)
                    warning("Data contains NA\'s.")
            }
        }

        # Third, we'll check to see if we have any character data
        if (!is.numeric(x)){
            if(!quiet)
                warning("The data does not appear to be numeric.")
            # Try to coerce the data
            # x = as.numeric(x)
        }

        # Fourth, we'll see if we have more than one data point.
        if (NROW(x) <= 1) {
            if(!quiet)
                warning("Only one row provided.")
        }

        # @todo: Add check for stopifnot(is.atomic(y))???

        x = as.vector(x)
    }

    # RESULTS
    return(x)

}

###############################################################################

`checkDataMatrix` <-
function (x, na.rm = TRUE, quiet = TRUE, ...)
{
    checkData(x, method = "matrix", na.rm = na.rm, quiet = quiet, ...)
}

###############################################################################

`checkDataVector` <-
function (x, na.rm = TRUE, quiet = TRUE, ...)
{
    checkData(x, method = "vector", na.rm = na.rm, quiet = quiet, ...)
}

###############################################################################

`checkDataZoo` <-
function (x, na.rm = TRUE, quiet = TRUE, ...)
{
    checkData(x, method = "zoo", na.rm = na.rm, quiet = quiet, ...)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: checkData.R,v 1.15 2007-09-25 04:29:09 peter Exp $
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.14  2007/08/16 12:58:22  peter
# - fix quote format for sntax highlighting
#
# Revision 1.13  2007/08/16 12:57:31  peter
# - fix quote format for syntax highlighting
#
# Revision 1.12  2007/08/15 20:09:47  brian
# - fix quote format for sntax highlighting
#
# Revision 1.11  2007/04/09 12:31:27  brian
# - syntax and usage changes to pass R CMD check
#
# Revision 1.10  2007/04/09 03:18:29  peter
# - forces column name in single column object
#
# Revision 1.9  2007/03/24 13:30:19  brian
# - remove spurious parentheses added by editor
#
# Revision 1.8  2007/03/24 13:28:07  brian
# - fix cut and paste error in checkDataZoo wrapper
#
# Revision 1.7  2007/03/24 13:24:27  brian
# - add wrappers for deprecated checkDataMatrix checkDataVector checkDataZoo
# - replaces files checkDataMatrix.R  checkDataVector.R  checkDataZoo.R
#
# Revision 1.6  2007/03/20 03:29:53  peter
# - shut off rowname and columnname warnings when method == "vector"
#
# Revision 1.5  2007/03/16 03:22:13  peter
# - doesn't re-zoo a zoo object, since that might change it's index format
#
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
