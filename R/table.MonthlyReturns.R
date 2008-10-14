`table.MonthlyReturns` <-
function (R, ci = 0.95, digits = 4)
{# @author Peter Carl

    # DESCRIPTION
    # Monthly Returns Summary: Statistics and Stylized Facts

    # Inputs:
    # R: Assumes returns rather than prices

    # Output:
    # Returns a basic set of statistics that match the period of the data passed
    # in (e.g., monthly returns will get monthly statistics)

    # FUNCTION:

    y = checkData(R, method = "zoo")

    # Set up dimensions and labels
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    rownames = rownames(y)

    cl.vals = function(x, ci) {
            x = x[!is.na(x)]
            n = length(x)
            if (n <= 1)
            return(c(NA, NA))
            se.mean = sqrt(var(x)/n)
            t.val = qt((1 - ci)/2, n - 1)
            mn = mean(x)
            lcl = mn + se.mean * t.val
            ucl = mn - se.mean * t.val
            c(lcl, ucl)
    }
# for each column, do the following:
    for(column in 1:columns) {
        x = as.vector(y[,column])
        x.length = length(x)
        x = x[!is.na(x)]
        x.na = x.length - length(x)
        z = c(
            length(x), 
            x.na, min(x), 
            as.numeric(quantile(x, prob = 0.25, na.rm = TRUE)), 
            median(x), 
            mean(x), 
            exp(mean(log(1+x)))-1,
            as.numeric(quantile(x, prob = 0.75, na.rm = TRUE)), 
            max(x), 
            sqrt(var(x)/length(x)),
            cl.vals(x, ci)[1], 
            cl.vals(x, ci)[2], 
            var(x), 
            sqrt(var(x)),
            skewness(x), 
            kurtosis(x)
            )
        z = base::round(as.numeric(z),digits)
        znames = c(
            "Observations", 
            "NAs", 
            "Minimum", 
            "Quartile 1", 
            "Median", 
            "Arithmetic Mean",
            "Geometric Mean", 
            "Quartile 3", 
            "Maximum", 
            "SE Mean", 
            paste("LCL Mean (",ci,")",sep=""),
            paste("UCL Mean (",ci,")",sep=""), 
            "Variance", 
            "Stdev", 
            "Skewness", 
            "Kurtosis"
            )
        if(column == 1) {
            resultingtable = data.frame(Value = z, row.names = znames)
        }
        else {
            nextcolumn = data.frame(Value = z, row.names = znames)
            resultingtable = cbind(resultingtable, nextcolumn)
        }
    }
    colnames(resultingtable) = columnnames
    ans = resultingtable
    ans
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: table.MonthlyReturns.R,v 1.8 2008-10-14 14:37:29 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.7  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.6  2007/03/20 13:47:12  peter
# - changed to checkData
# - cleaned up alignment
#
# Revision 1.5  2007/03/05 03:19:26  peter
# - removed firstcolumn as an attribute
#
# Revision 1.4  2007/02/25 18:23:40  brian
# - change call to round() to call base::round() to fix conflict with newest fCalendar
#
# Revision 1.3  2007/02/22 22:14:46  brian
# - standardize use of 'digits' as a parameter to round()
#
# Revision 1.2  2007/02/07 13:24:49  brian
# - fix pervasive comment typo
#
# Revision 1.1  2007/02/02 19:06:15  brian
# - Initial Revision of packaged files to version control
# Bug 890
#
###############################################################################
