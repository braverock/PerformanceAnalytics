\name{cumprod.column}
\alias{cumprod.column}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ wrapper to caclulate cumprod on all columns in a matrix }
\description{
wrapper to caclulate cumprod on all columns in a matrix
}
\usage{
cumprod.column(R, na.rm = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, or timeSeries of returns }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
  \item{\dots}{ any other passthru parameters }
}
\value{
transformed matrix of cumprods
}
\author{ Peter Carl }
\seealso{
    \code{\link{cummax.column}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }