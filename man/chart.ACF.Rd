\name{chart.ACF}
\alias{chart.ACF}
\alias{chart.ACFplus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create ACF chart or ACF with PACF two-panel chart }
\description{
  Creates an ACF chart or a two-panel plot with the ACF and PACF set to some specific defaults.
}
\usage{
chart.ACF(R, maxlag = NULL, elementcolor = "gray", main = NULL, ...)
chart.ACFplus(R, maxlag = NULL, elementcolor = "gray", main = NULL, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{maxlag}{ the number of lags to calculate for, optional }
  \item{elementcolor}{ the color to use for chart elements, defaults to "gray" }
  \item{main}{ title of the plot; uses the column name by default. }
  \item{\dots}{ any other passthru parameters }
}
\value{
    Returns a one- or two-panel chart.
}
\author{ Peter Carl }
\note{ 
Inspired by the website: \url{http://www.stat.pitt.edu/stoffer/tsa2/Rcode/acf2.R}
"...here's an R function that will plot the ACF and PACF of a time series 
at the same time on the SAME SCALE, and it leaves out the zero lag in the 
ACF: acf2.R. If your time series is in x and you want the ACF and PACF of 
x to lag 50, the call to the function is acf2(x,50). The number of lags 
is optional, so acf2(x) will use a default number of lags [sqrt(n) + 10, where 
n is the number of observations]."

That description made a lot of sense, so it's implemented here for both the ACF alone and the ACF with the PACF.
}
\seealso{  \code{\link{plot}} }
\examples{
data(edhec)
chart.ACFplus(edhec[,1,drop=FALSE])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
\keyword{ hplot }
