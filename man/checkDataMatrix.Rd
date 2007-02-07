\name{checkDataMatrix}
\alias{checkDataMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
checkDataMatrix(x, na.rm = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
  \item{\dots}{ any other passthru parameters }
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
function (x, na.rm = FALSE, ...)
{ # @author Peter Carl

    # Description:

    # This function was created to make the different kinds of data classes
    # at least _seem_ more fungible.  It allows the user to pass in a data
    # object without being concerned that the function requires a matrix,
    # data.frame, or timeSeries object.  By using this, the function "knows"
    # what data format it has to work with.

    # Inputs:
    # x: a matrix, data.frame or timeSeries object of data to be checked and
    # transformed if necessary.
    # na.rm: removes rows of data with NA's in any column.  This is useful
    # for calculating correlations, etc., that require complete data.  Note
    # that this passes back only the first contiguous set of data, to retain
    # the regular timeseries nature of the data.

    # Outputs:
    # Produces a matrix object with named rows and columns.

    # FUNCTION:
    if (class(x) == "numeric") {
        # Note that column and row labels will be blank.
        y = as.matrix(x)
    }

    if (class(x) == "matrix") {
        y = x
    }

    if (class(x) == "data.frame") {
        y = as.matrix(x)
    }

    if (class(x) == "timeSeries") {
        # timeSeries objects keep the data in a matrix and append a set of
        # meta-data.  We just need the information stored in the 'Data' slot.
        #y = x@Data
        y = seriesData(x)
    }

    if (na.rm) {
        #y = na.contiguous(y)
        y = na.omit(y)
    }
    return(y)

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
