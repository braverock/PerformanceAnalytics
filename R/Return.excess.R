`Return.excess` <-
function (R, rf = 0)
{ # @author Peter Carl

    # DESCRIPTION:
    # Calculates the returns of an asset in excess of the given 
    # "risk free rate" for the period.

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns
    # rf: a measure of the risk free rate, whether a period average
    #     (a single number) or a timeseries vector

    # Outputs:
    # A timeseries of the calculated series

    # FUNCTION:

    # Transform input data to a timeseries (zoo) object
    R = checkData(R, method="zoo")
    reference.name = ""
    result.zoo = zoo(NA)
    # if the risk free rate is delivered as a timeseries, we'll check it
    # and convert it to a zoo object.
    if(!is.null(dim(rf))){
        rf = checkData(rf, method = "zoo")
	reference.name = paste(" > ",colnames(rf),sep="")
    }
    else {
	reference.name = paste(" > ",base::round(rf, 4)*100,"%",sep="")
    }

    ## arithmetic on zoo objects intersects them first
#    R.excess = R[,1,drop = FALSE] - rf
#    R.excess = R[drop = FALSE] - rf # this won't handle multiple columns correctly

    # Get dimensions and labels
    columns.a = ncol(R)
#    columns.b = ncol(rf)
    columnnames.a = colnames(R)
#    columnnames.b = colnames(rf)

    for(column.a in 1:columns.a) { # for each asset passed in as R
#        for(column.b in 1:columns.b) { # against each asset passed in as Rf
            R.excess = zoo(NA)
            R.excess = R[ , column.a, drop=FALSE] - rf #[ , column.b, drop=FALSE]
            if(column.a == 1) { #& column.b == 1
                if(rf[1] == 0){
                    colnames(R.excess) = columnnames.a[column.a]
                }
                else {
                    colnames(R.excess) = paste(columnnames.a[column.a], reference.name, sep = "")
                }
                result.zoo = R.excess
 #               colnames(result.zoo) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = " > ")
            }
            else {
#                nextcolumn = data.frame(Value = z, row.names = znames)
                if(rf[1] == 0){
	                colnames(R.excess) = columnnames.a[column.a]
                }
                else {
	                colnames(R.excess) = paste(columnnames.a[column.a], reference.name, sep = "")
                }
#                 colnames(R.excess) = paste(columnnames.a[column.a], columnnames.b[column.b], sep = " > ")
                result.zoo = merge (result.zoo, R.excess)
            }
 #       }
    }


    # RESULTS:
    return(result.zoo)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Return.excess.R,v 1.8 2007-09-26 02:54:58 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.7  2007/08/14 23:20:07  peter
# - added conditional labeling to columns
#
# Revision 1.6  2007/08/14 21:37:05  peter
# - removed support for multiple columns in Rf
# - now works for numeric Rf
#
# Revision 1.5  2007/08/14 01:19:40  peter
# - function handles multiple columns for both R and Rf
#
# Revision 1.4  2007/05/15 19:47:38  peter
# - handles multiple column objects
#
# Revision 1.3  2007/03/16 13:59:20  peter
# - added cvs footer
#
#
###############################################################################
