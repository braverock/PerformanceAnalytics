`checkData` <-
function (x, method = c("xts", "zoo","matrix","vector","data.frame"), na.rm = FALSE, quiet = TRUE, ...)
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

    # if (!require("xts")) stop("xts package not available")

    method = method[1] # grab the first value if this is still a vector, to avoid varnings
    if(is.vector(x)){
        x = as.matrix(x)
    }

    if(!is.vector(x) & !inherits(x, what="zoo")){
    # For matrixes and data.frame objects, we test to see if there are rows, columns
    # and labels.  If there are not column labels, we provide them.  Row labels
    # we leave to the coersion functions.

        # Test for rows and columns
        if(is.null(NCOL(x)) )
            warning("There don\'t seem to be any columns in the data provided.  If you are trying to pass in names from a zoo object with one column, you should use the form \'data.zoo[rows, columns, drop = FALSE]\'.")


        if(is.null(NROW(x)))
            stop("No rows in the data provided.")

        # Test for rownames and column names
        if(is.null(colnames(x))) {
            columns = NCOL(x)
            if(!quiet)
                warning("No column names in the data provided. To pass in names from a data.frame, you should use the form \'data[rows, columns, drop = FALSE]\'.")
            columnnames = NULL
            for(column in 1:columns) {columnnames = c(columnnames, paste("Column.",column,sep=""))}
            colnames(x) = columnnames
        }

        if(is.null(rownames(x)))
            if(!quiet)
                warning("No row names in the data provided. To pass in names from a data.frame, you should use the form \'data[rows, columns, drop = FALSE]\'.")

        # First we check whether we have a zoo object
        # eventually this will change with the xts code below
### IS THIS BLOCK NECESSARY?
        if(!inherits(x,what="zoo")){
            x = as.matrix(x)
            if(is.null(rownames(x)))
                x = zoo(x)
            else
                x = zoo(x, order.by = rownames(x))
        }
    }

    switch(method,
        vector = {
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
        },
        data.frame = {
            x = as.data.frame(x)
        },
        xts = {
            #try xts
            if(xtsible(x)) x = xts(x, ...)
            # else
            #possibly try date formats with warnings
            #data was already coerced to zoo above
        },
        zoo = {
            x = as.zoo(x)
        },
        matrix = {
            x = as.matrix(x)
        }
    ) # end switch

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
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: checkData.R,v 1.24 2009-04-19 13:15:25 brian Exp $
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.23  2009-03-20 21:20:54  peter
# - makes vectors a single col matrix for naming again
#
# Revision 1.22  2009-03-20 20:48:48  peter
# - fixes vector to zoo translation
#
# Revision 1.21  2009-03-11 03:42:06  peter
# - added switch for xts, set as default
#
# Revision 1.20  2009-01-11 12:55:56  brian
# - convert 'zoo' method to use xts
# - TODO add reclass capability
#
# Revision 1.18  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.17  2007/10/18 13:56:53  peter
# - fixed error labeling data without columnnames
#
# Revision 1.16  2007/10/03 02:43:06  peter
# - single column zoo objects will have the time class and order preserved
# - yearmon class should be ordered correctly as a result
#
# Revision 1.15  2007/09/25 04:29:09  peter
# - added data.frame as method
#
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
