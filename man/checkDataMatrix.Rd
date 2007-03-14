\name{checkDataMatrix}
\alias{checkDataMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ check input data type and format to normalize to a vector }
\description{
This function was created to make the different kinds of data classes at least emph{seem} more fungible.  It allows the user to pass in a data object without being concerned that the function requires a matrix, data.frame, or timeSeries object.  By using this, the function "knows" what data format it has to work with.
}
\usage{
checkDataMatrix(R, na.rm = FALSE, quiet = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
  \item{quiet}{ TRUE/FALSE if false, it will throw warnings when errors are noticed, default TRUE }
  \item{\dots}{ any other passthru parameters }
}
\value{
transformed vector
}
\author{ Peter Carl }
\seealso{
    \code{\link{checkDataVector}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }