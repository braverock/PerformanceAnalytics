\name{checkData}
\alias{checkData}
\alias{checkDataMatrix}
\alias{checkDataVector}
\alias{checkDataZoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ check input data type and format and coerce to the desired output type }
\description{
This function was created to make the different kinds of data classes at least emph{seem} more fungible.  It allows the user to pass in a data object without being concerned that the function requires a matrix, data.frame, or timeSeries object.  By using this, the function "knows" what data format it has to work with.
}
\usage{
checkData(x, method = c("zoo", "matrix", "vector","data.frame"), na.rm = FALSE, quiet = TRUE, ...)
checkDataMatrix(x, na.rm = TRUE, quiet = TRUE, ...)
checkDataVector(x, na.rm = TRUE, quiet = TRUE, ...)
checkDataZoo(x, na.rm = TRUE, quiet = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector, matrix, data frame, timeSeries or zoo object to be checked and coerced }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the data? }
  \item{quiet}{ TRUE/FALSE if false, it will throw warnings when errors are noticed, default TRUE }
  \item{method}{ type of coerced data object to return, one of c("zoo","matrix","vector"), default "zoo" }
  \item{\dots}{ any other passthru parameters }
}
\value{
checkDataMatrix returns a data.frame

checkDataVector returns a vector

checkDataZoo returns a zoo object

checkData will make a reasonable assumption, or return the object requested by method
}
\details{
The \code{checkData} function grew out of three older functions: \code{checkDataMatrix},\code{checkDataVector}, and \code{checkDataZoo}.  All three of these functions are provided as wrappers to \code{checkData} for backwards compatibility and for clarity inside existing code.  It is worth noting that the older checkData* functions set \code{na.rm=TRUE} by default.  The user is free to specify a value for this parameter to change the behavior.
}
\author{ Peter Carl }
\examples{
data(edhec)
x = checkData(edhec)
class(x)
head(x)
tail(x)
# Note that passing in a single column loses the row and column names
x = checkData(edhec[,1])
class(x)
head(x)
# Include the "drop" attribute to keep row and column names
x = checkData(edhec[,1,drop=FALSE])
class(x)
head(x)
x = checkData(edhec, method = "matrix")
class(x)
head(x)
x = checkData(edhec[,1], method = "vector")
class(x)
head(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }