\name{Return.read}
\alias{Return.read}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Read returns data with different date formats }
\description{
  A simple wrapper of read.zoo with some defaults for different date formats
}
\usage{
Return.read(filename = stop("Please specify a filename"),
                 frequency = c("m", "d", "q", "i", "o"), format.in =
                 c("ISO8601", "excel", "oo", "gnumeric"), sep = ",",
                 header = TRUE, check.names = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{filename}{ the name of the file to be read }
    \item{frequency}{
        \itemize{
            \item "d" sets as a daily timeseries using \code{\link{as.Date}},
            \item "m" sets as monthly timeseries using \code{\link[zoo]{as.yearmon}},
            \item "q" sets as a quarterly timeseries using \code{\link[zoo]{as.yearqtr}}, and
            \item "i" sets as irregular timeseries using \code{\link{as.POSIXct}}
        }
    }
    \item{format.in}{ says how the data being read is formatted.  Although
        the default is set to the ISO 8601 standard (which can also be set as "\%F"),
        most spreadsheets have less sensible date formats as defaults. See below.
    }
    \item{sep}{ separator, default is "," }
    \item{header}{ a logical value indicating whether the file contains the
            names of the variables as its first line.
    }
    \item{check.names}{logical. If TRUE then the names of the variables in the data frame are checked to ensure that they are syntactically valid variable names. If necessary they are adjusted (by make.names) so that they are, and also to ensure that there are no duplicates. See \code{\link[utils]{read.table}} }
    \item{\dots}{ passes through any other parameters to \code{\link[zoo]{read.zoo}} }
}

\details{
The parameter 'format.in' takes several values, including:
    \itemize{
        \item excel: default date format for MS Excel spreadsheet csv format, which is "\%m/\%d/\%Y",
        \item oo: default date format for OpenOffice spreadsheet csv format, "\%m/\%d/\%y", although this may be operating system dependent,
        \item gnumeric: default date format for Gnumeric spreadsheet, which is "\%d-\%b-\%Y",
        \item ...: alternatively, any specific format may be passed in, such as "\%M/\%y"
    }
}

\value{
A zoo object containing a representation of the data in the file.
}
\author{ Peter Carl }

\seealso{ \code{\link[zoo]{read.zoo}}, \code{\link[utils]{read.table}}}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }