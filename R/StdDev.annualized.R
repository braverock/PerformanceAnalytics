sd.multiperiod <-
function (x, scale = NA)
{
    if (is.vector(x)) {
        #scale standard deviation by multiplying by the square root of the number of periods to scale by
        if(!xtsible(x) & is.na(scale))
            stop("'x' needs to be timeBased or xtsible, or scale must be specified." )
        x = checkData (x)
        if(is.na(scale)) {
            freq = periodicity(x)
            switch(freq$scale,
                minute = {stop("Data periodicity too high")},
                hourly = {stop("Data periodicity too high")},
                daily = {scale = 252},
                weekly = {scale = 52},
                monthly = {scale = 12},
                quarterly = {scale = 4},
                yearly = {scale = 1}
            )
        }
        sqrt(scale)*sd(x, na.rm=TRUE)
    } else { 
        result = apply(x, 2, sd.multiperiod, scale=scale)
        dim(result) = c(1,NCOL(x))
        colnames(result) = colnames(x)
        rownames(result) = "Annualized Standard Deviation"
        return(result)
    }
}

sd.annualized <-
function (x, scale = NA)
{   # wrapper function for backwards compatibility
    sd.multiperiod(x, scale = scale)
}

StdDev.annualized <-
function (R, scale = NA)
{   # wrapper function for backwards compatibility
    sd.multiperiod(R, scale = scale)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2010 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################