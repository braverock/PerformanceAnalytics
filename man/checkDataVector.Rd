\name{checkDataVector}
\alias{checkDataVector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
checkDataVector(R, na.rm = TRUE, quiet = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
  \item{quiet}{ ~~Describe \code{quiet} here~~ }
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
function (R, na.rm = TRUE, quiet = TRUE, ...)
{ # @author Peter Carl

    # Description:

    # This function was created to make the different kinds of data classes
    # at least _seem_ more fungible.  It allows the user to pass in a data
    # object without being concerned that the function requires a matrix,
    # data.frame, or timeSeries object.  By using this, the function "knows"
    # what data format it has to work with.

    # Inputs:
    # x: the data provided.
    # na.rm: default behavior is to remove NA's
    # quiet: if true, it will throw warnings when errors are noticed

    # Outputs:
    # Produces a numeric vector or an NA and any associated warnings.

    # FUNCTION:

    x = as.vector(R)

    # First, we'll check to see if we have more than one column.
    if (NCOL(x) > 1) {
        if(!quiet)
            warning("The data provided is not a vector or univariate time series.  Used only the first column")
        x = x[,1]
    }

    # Second, we'll hunt for NA's and remove them if required
    if (any(is.na(x))) {
        if(na.rm) {
            # Try to remove any NA's
            x = x[!is.na(x)]
            if(!quiet){
                warning("The following slots have NAs.")
                warning(paste(x@na.removed," "))
            }
        }
        else {
            if(!quiet)
                warning("Data contains NA's.")
        }
    }

    # Third, we'll check to see if we have any character data
    if (!is.numeric(x)){
        if(!quiet)
            warning("The data is not numeric.")
        # Try to coerce the data
        x = as.numeric(x)
    }

    # Fourth, we'll see if we have more than one data point.
    if (NROW(x) <= 1) {
        if(!quiet)
            warning("Only one row provided.  Returning NA.")
        return(NA)
    }

    # @todo: Add check for stopifnot(is.atomic(y))???

    return(as.vector(x))

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
