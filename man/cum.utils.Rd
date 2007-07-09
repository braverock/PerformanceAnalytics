\name{cum.uils}
\alias{cumprod.column}
\alias{cummax.column}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ wrapper to calculate cumprod on all columns in a matrix }
\description{
calculate cumulative values for the observation series given, including multicolumn extensions to cummax and cumprod
    \tabular{ll}{
        \code{\link{cummax.column}} \tab cumulative maxima by column, useful for drawdowns \cr
        \code{\link{cumprod.column}} \tab cumulative product by column, useful for compound returns \cr
    }

}
\usage{
cumprod.column(x, na.rm = TRUE, ...)
cummax.column(x, na.rm = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector, matrix, data frame, timeSeries or zoo object of observations }
  \item{na.rm}{ TRUE/FALSE Remove NA's from the returns? }
  \item{\dots}{ any other passthru parameters }
}
\value{
transformed matrix of cumprod's  or cummax's
}
\author{ Peter Carl }
\seealso{
    \code{\link{cumprod}}
    \code{\link{cummax}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }