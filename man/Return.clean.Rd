\name{Return.clean}
\alias{Return.clean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ clean returns in a time series to to provide more robust risk estimates }
\description{
A function that provides access to multiple methods for cleaning outliers from return data.
}
\usage{
Return.clean(R, method = "boudt", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{method}{ currently only one method is supported, "boudt", which applies the function \code{\link{clean.boudt}} to R }
  \item{...}{ parameters passed into the underlying cleaning function }
}
\details{
This is a wrapper for offering multiple data cleaning methods for data objects containing returns.

The primary value of data cleaning lies in creating a more
robust and stable estimation of the distribution generating the
large majority of the return data. The increased robustness and
stability of the estimated moments using cleaned data should be
used for portfolio construction. If an investor wishes to
have a more conservative risk estimate, cleaning may not be
indicated for risk monitoring. 

In actual practice, it is probably best to back-test the results of
both cleaned and uncleaned series to see what works best when forecasting risk with the
particular combination of assets under consideration.

In this version, only one method is supported.  See \code{\link{clean.boudt}} for more details.
}
\value{
zoo object with the same dimensions as R, containing data cleaned with the method selected.
}
\references{

}

\author{
Peter Carl
}
\note{
}

\seealso{ \code{\link{clean.boudt}} }

\examples{
data(managers)
Return.clean(managers)
chart.BarVaR(managers[,1,drop=FALSE], show.clean=TRUE, clean="boudt", lwd=2, methods="ModifiedVaR")
}

\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }