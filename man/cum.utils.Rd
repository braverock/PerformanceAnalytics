\name{cum.utils}
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

A prior version of thesefunctions included specific NA handling.  This was removed due to a conflict with the general cumulative functions in the R core.  Please be aware that you may have issues with series of different lengths, and may need to explicitly handle NAs as a result.

}
\usage{
cumprod.column(x)
cummax.column(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector, matrix, data frame, timeSeries or zoo object of observations }
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