\name{cummax.column}
\alias{cummax.column}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ wrapper to calculate cummax on all columns in a matrix }
\description{
wrapper to calculate cummax on all columns in a matrix
}
\usage{
cummax.column(R, na.rm = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
  \item{\dots}{ any other passthru parameters }
}
\value{
transformed matrix of cummaxs
}
\author{ Peter Carl }
\seealso{
    \code{\link{cumprod.column}}
    \code{\link{cummax}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }