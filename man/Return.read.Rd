\name{Return.read}
\alias{Return.read}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Return.read(filename = stop("Please specify a filename"),
                 frequency = c("m", "d", "q", "i", "o"), format.in =
                 c("ISO8601", "excel", "oo", "gnumeric"), sep = ",",
                 header = TRUE, check.names = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{ ~~Describe \code{filename} here~~ }
  \item{frequency}{ ~~Describe \code{frequency} here~~ }
  \item{format.in}{ ~~Describe \code{format.in} here~~ }
  \item{sep}{ ~~Describe \code{sep} here~~ }
  \item{header}{ ~~Describe \code{header} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (filename=stop("Please specify a filename"), frequency=c("m","d","q","i","o"), format.in=c("ISO8601","excel","oo","gnumeric"), sep=",", header=TRUE, ...)
{ # @author Peter Carl

# A simple wrapper with some defaults for read.zoo

# format is a parameter that says how the data being read is formatted.  Although
# the default is set to the ISO 8601 standard (which can also be set as "\%F"),
# most spreadsheets have less sensible date formats as defaults.

    format.in = format.in[1]
    frequency = frequency[1]

    switch(format.in,
        ISO8601 = { # the least confusing default to use, \%Y-\%m-\%d or \%F
            format = "\%F"
        },
        excel = { # default date format for MS Excel spreadsheet csv
            format = "\%m/\%d/\%Y"
        },
        oo = { # default date format for OpenOffice spreadsheet csv
            format = "\%m/\%d/\%y" # this is a operating system dependent format
        },
        gnumeric = { # default date format for Gnumeric spreadsheet
            format = "\%d-\%b-\%Y"
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
    result = read.zoo(filename, sep=sep, format=format, FUN=FUN, header=header, ...)

    result

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
