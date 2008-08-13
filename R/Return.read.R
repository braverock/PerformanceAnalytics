`Return.read` <-
function (filename=stop("Please specify a filename"), frequency = c("m","d","q","i","o"), format.in = c("ISO8601","excel","oo","gnumeric"), sep = ",", header = TRUE, check.names = FALSE, ...)
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
    rownames(result) = as.character(as.Date(time(result)))

    result

}
###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2008 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Return.read.R,v 1.5 2008-08-13 03:32:10 peter Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.4  2008-06-02 16:05:19  brian
# - update copyright to 2004-2008
#
# Revision 1.3  2007/09/24 02:32:31  peter
# - added check.names as a parameter
# - set default so that names are not checked
# - spaces will not be replaced by dots in column names by default
#
# Revision 1.2  2007/08/16 14:11:44  peter
# - clarified comments
#
# Revision 1.1  2007/08/15 03:37:43  peter
# - first entry in CVS
# - wrapper for read.zoo with a few case conditions for setting defaults
#
###############################################################################