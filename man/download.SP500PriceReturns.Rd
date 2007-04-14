\name{download.SP500PriceReturns}
\alias{download.SP500PriceReturns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ download S \& P Prices and calculate S \& P returns }
\description{
This function downloads daily/monthly S \& P 500 \emph{prices} using \code{\link[tseries]{get.hist.quote}} for "\^gspc" and calculate S \& P \emph{returns} using \code{\link{CalculateReturns}}.
}
\usage{
download.SP500PriceReturns(start = "1998-01-01", end = NULL, compression=c("m","d"), method=c("compound","simple"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{start}{start date in YYYY-mm-dd format, default "1998-01-01"}
  \item{end}{end date in YYYY-mm-dd format}
  \item{compression}{"m" for monthly returns and "d" for daily returns, default monthly, as passthru to \code{\link[tseries]{get.hist.quote}}}
  \item{method}{"simple" or "compound" as a passthru to \code{\link{CalculateReturns}}, default compound}
}
\value{
matrix of simple or compound returns on the S \& P 500 at the daily/monthly scale specified
}
\details{
}
\references{ }
\author{ Peter Carl }
\note{
}
\seealso{
   \code{\link{CalculateReturns}}
   \code{\link{download.RiskFree}}
   \code{\link[tseries]{get.hist.quote}}
}
\examples{
sp500 = download.SP500PriceReturns(start = "1996-12-31", end = "2007-01-01")
class(sp500)
head(sp500)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }