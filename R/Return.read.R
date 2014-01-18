#' Read returns data with different date formats
#' 
#' A simple wrapper of read.zoo with some defaults for different date formats
#' and xts conversion
#' 
#' The parameter 'format.in' takes several values, including: 
#' \describe{
#'   \item{excel}{default date format for MS Excel spreadsheet csv format, which is "\%m/\%d/\%Y"} 
#'   \item{oo}{default date format for OpenOffice spreadsheet csv format, "\%m/\%d/\%y", although this may be operating system dependent}
#'   \item{gnumeric}{default date format for Gnumeric spreadsheet, which is "\%d-\%b-\%Y"} 
#'   \item{...}{alternatively, any specific format may be passed in, such as "\%M/\%y"} 
#' }
#' 
#' @param filename the name of the file to be read
#' @param frequency \itemize{ 
#' 	\item "d" sets as a daily timeseries using \code{\link{as.Date}}, 
#' 	\item "m" sets as monthly timeseries using \code{\link[zoo]{as.yearmon}}, 
#'  \item "q" sets as a quarterly timeseries using \code{\link[zoo]{as.yearqtr}}, and 
#' 	\item "i" sets as irregular timeseries using \code{\link{as.POSIXct}} 
#' }
#' @param format.in says how the data being read is formatted.  Although the
#' default is set to the ISO 8601 standard (which can also be set as "%F"),
#' most spreadsheets have less sensible date formats as defaults. See below.
#' @param sep separator, default is ","
#' @param header a logical value indicating whether the file contains the names
#' of the variables as its first line.
#' @param check.names logical. If TRUE then the names of the variables in the
#' data frame are checked to ensure that they are syntactically valid variable
#' names. If necessary they are adjusted (by make.names) so that they are, and
#' also to ensure that there are no duplicates. See
#' \code{\link[utils]{read.table}}
#' @param \dots passes through any other parameters to
#' \code{\link[zoo]{read.zoo}}
#' @author Peter Carl
#' @seealso \code{\link[zoo]{read.zoo}}, \code{\link[utils]{read.table}}
#' @keywords ts multivariate distribution models
#' @examples
#' 
#'      \dontrun{
#'      Return.read("managers.cvs", frequency="d")
#'      }
#' 
#' @export
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
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################
