`checkDataMatrix` <-
function (R, na.rm = FALSE, ...)
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
    # na.rm: removes rows of data with NA's in any column.  This is useful
    # for calculating correlations, etc., that require complete data.  Note
    # that this passes back only the first contiguous set of data, to retain
    # the regular timeseries nature of the data.

    # Outputs:
    # Produces a matrix object with named rows and columns.

    # FUNCTION:
    if (class(R) == "numeric") {
        # Note that column and row labels will be blank.
        R = as.matrix(R)
    }

    if (class(R) == "matrix") {
        R = R
    }

    if (class(R) == "data.frame") {
        R = as.matrix(R)
    }

    if (class(R) == "zoo") {
        R = as.matrix(R)
    }

    if (class(R) == "timeSeries") {
        # timeSeries objects keep the data in a matrix and append a set of
        # meta-data.  We just need the information stored in the 'Data' slot.
        #y = x@Data
        R = seriesData(R)
    }

    if (na.rm) {
        #y = na.contiguous(R)
        R = na.omit(R)
    }
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
# $Id: checkDataMatrix.R,v 1.4 2007-02-28 04:42:11 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.3  2007/02/26 22:04:36  brian
# - changes in functions to pass "R CMD check" for package
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################