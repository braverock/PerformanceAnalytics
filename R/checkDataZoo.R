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
    # for calculating correlations, etc., that require complete data.  

    # Outputs:
    # Produces a "zoo" timeseries object with named rows and columns.

    # FUNCTION:
    if (class(R) != "zoo") {

        if (class(R) == "numeric") {
            # Note that column and row labels will be blank.
            R = as.matrix(R)
        }
    
#         if (class(R) == "matrix") {
            # Do nothing
#         }
    
#         if (class(R) == "data.frame") {
#             R = as.matrix(R)
#         }
    
        if (class(R) == "timeSeries") {
            # timeSeries objects keep the data in a matrix and append a set of
            # meta-data.  We just need the information stored in the 'Data' slot.
            #y = x@Data
            R = seriesData(R)
        }

        R = zoo(R, order.by = rownames(R))
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
# $Id: checkDataZoo.R,v 1.1 2007-03-09 22:41:03 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
#
###############################################################################