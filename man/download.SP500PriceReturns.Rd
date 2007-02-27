\name{download.SP500PriceReturns}
\alias{download.SP500PriceReturns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ download S&P Returns }
\description{
downloads S&P 500 prices and calculates \emph{returns} using get.hist.quote for "^gspc"
}
\usage{
download.SP500PriceReturns(start = "1998-01-01", end = NULL, compression=c("m","d"), method=c("simple","compound"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{start}{start date in YYYY-mm-dd format, default "1998-01-01"}
  \item{end}{end date in YYYY-mm-dd format}
  \item{compression}{"m" for monthly returns and "d" for daily returns, default monthly}
  \item{method}{"simple" or "compound" as a passthru to \code{\link{CalculateReturns}}}
}
\value{
matrix of simplei or compound returns on the S&P 500 at the timescale specified
}
\details{
}
\references{ }
\author{ Peter Carl }
\note{
}
\seealso{
   \code{\link{CalculateReturns}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
