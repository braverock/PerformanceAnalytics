#' Monthly and Calendar year Return table
#' 
#' Returns a table of returns formatted with years in rows, months in columns,
#' and a total column in the last column.  For additional columns in \code{R},
#' annual returns will be appended as columns.
#' 
#' 
#' @aliases table.CalendarReturns table.Returns
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param digits number of digits to round results to for presentation
#' @param as.perc TRUE/FALSE if TRUE, multiply simple returns by 100 to get \%
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns,
#' default TRUE
#' @note This function assumes monthly returns and does not currently have
#' handling for other scales.
#' 
#' This function defaults to the first column as the monthly returns to be
#' formatted.
#' @author Peter Carl
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' t(table.CalendarReturns(managers[,c(1,7,8)]))
#' 
#' # prettify with format.df in hmisc package
#' require("Hmisc")
#' result = t(table.CalendarReturns(managers[,c(1,8)]))
#' 
#' textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE, 
#'          cdec=rep(1,dim(result)[2])), rmar = 0.8, cmar = 1,  
#'          max.cex=.9, halign = "center", valign = "top", 
#'          row.valign="center", wrap.rownames=20, wrap.colnames=10, 
#'          col.rownames=c( rep("darkgray",12), "black", "blue"), 
#'          mar = c(0,0,3,0)+0.1)
#'          
#' title(main="Calendar Returns")
#' 
#' @export
table.CalendarReturns <-
function (R, digits = 1, as.perc = TRUE, geometric = TRUE)
{# @author Peter Carl

    # DESCRIPTION:

    # Monthly and Calendar year Return table

    # Inputs:
    # R: assumes an input of ***monthly*** returns, such as:
    # > head(monthlyReturns.ts)
    #             Actual S&P500TR
    # 2002-12-31  0.0395  -0.0588
    # 2003-01-31 -0.0105  -0.0262
    # 2003-02-28 -0.0316  -0.0150
    # 2003-03-31 -0.0107   0.0097
    # 2003-04-30  0.0799   0.0824
    # 2003-05-30  0.0897   0.0527

    # This function defaults to the first column as the returns to be
    # formatted, but the function will format the column specified.

    # Outputs:
    # Returns a table of returns formatted with years in rows, months in
    # columns, and a total column in the last column, e.g.,

    #        Jan   Feb   Mar   Apr  May   Jun   Jul   Aug   Sep   Oct   Nov   Dec
    # 2002    NA    NA    NA    NA   NA    NA    NA    NA    NA    NA    NA  3.95
    # 2003 -1.05 -3.16 -1.07  7.99 8.97  1.49 -2.03 -0.79  4.32 -0.89 -1.21  3.05
    # 2004  1.83 -0.70  2.45  0.36 0.13 -1.92  0.66 -1.74 -1.37 -0.08  3.90 -0.52
    # 2005  2.00  1.28 -1.04 -0.18 1.64  1.91  4.24  1.92 -1.63  1.45 -0.35  0.00
    # 2006  2.10    NA    NA    NA   NA    NA    NA    NA    NA    NA    NA    NA
    #       Year
    # 2002  3.95
    # 2003 15.83
    # 2004  2.87
    # 2005 11.68
    # 2006  2.10

    # FUNCTION:

    ri = checkData(R, method = "zoo")

    columns = ncol(ri)
    columnnames = colnames(ri)
    rownames = rownames(ri)

    # get the start and end years from the object of monthly returns
    firstyear = as.numeric(format(strptime(as.POSIXct(time(ri)[1]),"%Y-%m-%d"), "%Y"))
    lastyear = as.numeric(format(strptime(as.POSIXct(time(ri)[length(ri[,1])]), "%Y-%m-%d"), "%Y"))

    # create vectors for year and month labels
    year = format(strptime(as.POSIXct(time(ri)), "%Y-%m-%d"), "%Y")
    month = format(strptime(as.POSIXct(time(ri)), "%Y-%m-%d"), "%b")

    # create an empty target data frame, labeled correctly
    monthlabels = strftime(seq.Date(as.Date('2000-01-01'),length.out=12,by='months'),format='%b')
    rowlabels = (firstyear:lastyear)

    # for the column specified, do the following:
    for(column in 1:columns){
        target.df = as.data.frame(matrix(data = as.numeric(NA), length(rowlabels), length(monthlabels), dimnames = list(rowlabels, monthlabels)))

        # for the length of the monthly return data vector
        #   figure out the month and year of the data point and
        #   put it into the matrix
        for (i in 1:length(ri[,1])) {
            if(!is.na(ri[i,column])) {
                target.df[year[i],month[i]] = ri[i,column]
            }
        }

        # calculate calendar year returns
        #    first, create a target data.frame
        yearcol=as.data.frame(matrix(data = as.numeric(NA), length(rowlabels), 1, dimnames = list(rowlabels, columnnames[column])))

        #    next, calculate the cumulative return for each year
        for (i in 1:length(yearcol[,1])) {
			if(geometric)
				yearcol[i,columnnames[column]] = prod(1 + na.omit(as.numeric(target.df[i,])))-1
			else
				yearcol[i,columnnames[column]] = sum(as.numeric(target.df[i,]), na.rm=TRUE)
			if(yearcol[i,columnnames[column]]== 0) 
                yearcol[i,columnnames[column]] = NA
        }
        #Now, append the results to the other data.frame
        target.df=cbind(target.df,yearcol)

        # are we going to show these numbers as a percentage? or raw?
        if(as.perc)
            multiplier = 100
        else
            multiplier = 1

        target.df=target.df*multiplier
        target.df = base::round(target.df, digits)

        if(column == 1)
            result.df = target.df
        else {
            result.df = cbind(result.df,target.df[,13])

        }

    }
    colnames(result.df) = c(monthlabels,columnnames)
    result.df

    # To plot the result to a graphics object:
    # require(gplot)
    # textplot(target.df, halign = "center", valign="center",cex=.8)
    # strangely, textplot won't fill in trailing zeros

}

table.Returns <-
function (R, digits = 1, as.perc = TRUE, geometric = TRUE)
{
    # deprecated wrapper function
    table.CalendarReturns(R=R, digits = digits, as.perc = as.perc, geometric = geometric)
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
#
###############################################################################
