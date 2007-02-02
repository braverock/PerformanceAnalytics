`checkDataMatrix` <-
function (x, na.rm = FALSE, ...)
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
    # na.rm: removes rows of data with NA's in any column.  This is useful
    # for calculating correlations, etc., that require complete data.  Note
    # that this passes back only the first contiguous set of data, to retain
    # the regular timeseries nature of the data.

    # Outputs:
    # Produces a matrix object with named rows and columns.

    # FUNCTION:
    if (class(x) == "numeric") {
        # Note that column and row labels will be blank.
        y = as.matrix(x)
    }

    if (class(x) == "matrix") {
        y = x
    }

    if (class(x) == "data.frame") {
        y = as.matrix(x)
    }

    if (class(x) == "timeSeries") {
        # timeSeries objects keep the data in a matrix and append a set of
        # meta-data.  We just need the information stored in the 'Data' slot.
        #y = x@Data
        y = seriesData(x)
    }

    if (na.rm) {
        #y = na.contiguous(y)
        y = na.omit(y)
    }
    return(y)

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: checkDataMatrix.R,v 1.1 2007-02-02 19:06:15 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
###############################################################################