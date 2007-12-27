`table.Autocorrelation` <-
function (R, digits = 4)
{# @author Peter Carl

    # DESCRIPTION:
    # This is a wrapper for calculating the first six autocorrelation 
    # coefficients and Q-statistic for significance against
    # each column of the data provided.

    # Inputs:
    # R

    # Output:
    # A data table of autocorrelation coefficients \rho and corresponding 
    # Q(6)-statistic for each column in R.

    # FUNCTION:

    R = checkData(R, method = "zoo")

    # Get dimensions and labels
    columns = ncol(R)
    columnnames = colnames(R)

    # Calculate
    for(column in 1:columns) { # for each asset passed in as R
        y = checkData(R[,column], method="vector", na.rm = TRUE)

        acflag6 = acf(y,plot=F,lag.max=6)[[1]][2:7]
        LjungBox =  Box.test(y,type="Ljung-Box",lag=6)
        values = c(acflag6, LjungBox$p.value)
        values = base::round(as.numeric(values),digits)

        if(column == 1) {
            result.df = data.frame(Value = values)
            colnames(result.df) = columnnames[column]
        }
        else {
            nextcol = data.frame(Value = values)
            colnames(nextcol) = columnnames[column]
            result.df = cbind(result.df, nextcol)
        }
    }

    rownames(result.df) = c("rho1", "rho2", "rho3", "rho4", "rho5", "rho6","Q(6) p-value")

    result.df

# > t(table.Autocorrelation(managers))
#                rho1    rho2    rho3    rho4    rho5    rho6 Q(6) p-value
# HAM1         0.2157 -0.0436 -0.0536 -0.1733 -0.0124  0.0355       0.0801
# HAM2         0.1966  0.2933  0.0292  0.0438  0.0360  0.1602       0.0028
# HAM3         0.0042  0.1825  0.0180  0.0793 -0.0796  0.1886       0.0788
# HAM4         0.1833 -0.1014 -0.1773 -0.0935 -0.0006  0.0119       0.0748
# HAM5        -0.0787 -0.1665 -0.0164  0.2085 -0.1287 -0.1207       0.1716
# HAM6         0.1164  0.1565  0.0077 -0.0638 -0.0835 -0.1303       0.5955
# EDHEC.LS.EQ  0.2084  0.0801  0.0283 -0.0441 -0.0496  0.1799       0.0898
# SP500.TR    -0.0134 -0.0336  0.0514 -0.0878  0.0853  0.0776       0.7487
# US.10Y.TR    0.0398 -0.1739  0.1049 -0.0355 -0.1116 -0.0602       0.2199
# US.3m.TR     0.9224  0.9081  0.8968  0.8746  0.8363  0.8127       0.0000


}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: table.Autocorrelation.R,v 1.1 2007-10-05 03:24:03 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
#
###############################################################################