\name{download.SP500PriceReturns}
\alias{download.SP500PriceReturns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ download S&P Prices }
\description{
downloads daily S&P 500 \emph{returns} using get.hist.quote for "^gspc"
}
\usage{
download.SP500PriceReturns(start = "1998-01-01", end = NULL, scale=c("m","d"), method=c("simple","compound"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{start}{start date in YYYY-mm-dd format, default "1998-01-01"}
  \item{end}{end date in YYYY-mm-dd format}
  \item{scale}{"m" for monthly returns and "d" for daily returns, default monthly}
  \item{method}{"simple" or "compound" as a passthru to \code{\link{CalculateReturns}}}
}
\value{
matrix of simpleor compound returns on the S&P 500 at the scale specified
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