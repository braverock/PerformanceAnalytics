\name{checkData}
\alias{checkData}
\alias{checkDataMatrix}
\alias{checkDataVector}
\alias{checkDataZoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ check input data type and format to normalize to a vector }
\description{
This function was created to make the different kinds of data classes at least emph{seem} more fungible.  It allows the user to pass in a data object without being concerned that the function requires a matrix, data.frame, or timeSeries object.  By using this, the function "knows" what data format it has to work with.
}
\usage{
checkData(x, method = c("zoo","matrix","vector"), na.rm = FALSE, quiet = FALSE, ...)
checkDataMatrix(R, na.rm = FALSE, quiet = TRUE, ...)
checkDataVector(R, na.rm = TRUE, quiet = TRUE, ...)
checkDataZoo(R, na.rm = TRUE, quiet = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector, matrix, data frame, timeSeries or zoo object to be checked and coerced }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the data? }
  \item{quiet}{ TRUE/FALSE if false, it will throw warnings when errors are noticed, default TRUE }
  \item{\dots}{ any other passthru parameters }
}
\value{
checkDataMatrix returns a data.frame

checkDataVector returns a vector

checkDataZoo returns a zoo object

checkData will make a reasonable assumption, or return the object requested by method
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