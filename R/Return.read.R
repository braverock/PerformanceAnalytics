Return.read <-
function (filename=stop("Please specify a filename"), frequency = c("d","m","q","i","o"), format.in = c("ISO8601","excel","oo","gnumeric"), sep = ",", header = TRUE, check.names = FALSE, ...)
{ # @author Peter Carl

# A simple wrapper with some defaults for read.zoo

# format.in is a parameter that says how the data being read is formatted.  Although
# the default is set to the ISO 8601 standard (which can also be set as "%F"),
# most spreadsheets have less sensible date formats as defaults.

    format.in = format.in[1]
    frequency = frequency[1]

    switch(format.in,
        ISO8601 = { # the least confusing default to use, %Y-%m-%d or %F
            format = "%F"
        },
        excel = { # default date format for MS Excel spreadsheet csv
            format = "%m/%d/%Y"
        },
        oo = { # default date format for OpenOffice spreadsheet csv
            format = "%m/%d/%y" # this is a operating system dependent format
        },
        gnumeric = { # default date format for Gnumeric spreadsheet
            format = "%d-%b-%Y"
        },
        { format = format.in }
    )

# Sets the date format conditioned on the frequency passed in
# @todo: indexFormat(x) <- "%b %Y" instead
    switch(frequency,
        d = { # set to daily timeseries
            FUN = as.Date
        },
        m = { # set to monthly timeseries
            FUN = as.yearmon
        },
        q = { # set to quarterly timeseries
            FUN = as.yearqtr
        },
        i = { # set to irregular timeseries
            FUN = as.POSIXct
        },
        { # set to other timeseries
            FUN = NULL
        }
    )
    result = read.zoo(filename, sep=sep, format=format, FUN=FUN, header=header, check.names = check.names, ...)
    if(xtsible(result)) result = xts(result)
#     rownames(result) = as.character(as.Date(time(result)))

    result

}
###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################