\name{chart.BenchmarkSR}
\alias{chart.BenchmarkSR}
\title{Benchmark Sharpe Ratio Plots}
\usage{
  chart.BenchmarkSR(R = NULL, S = NULL, main = NULL,
    ylab = NULL, xlab = NULL, element.color = "darkgrey",
    lwd = 2, pch = 1, cex = 1, cex.axis = 0.8, cex.lab = 1,
    cex.main = 1,
    vs = c("sharpe", "correlation", "strategies"),
    xlim = NULL, ylim = NULL, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{S}{Number of strategies}

  \item{ylab}{set the y-axis label, as in
  \code{\link{plot}}}

  \item{xlab}{set the x-axis label, as in
  \code{\link{plot}}}

  \item{main}{set the chart title, as in
  \code{\link{plot}}}

  \item{element.color}{set the element.color value as in
  \code{\link{plot}}}

  \item{lwd}{set the width of the line, as in
  \code{\link{plot}}}

  \item{pch}{set the pch value, as in \code{\link{plot}}}

  \item{cex}{set the cex value, as in \code{\link{plot}}}

  \item{cex.axis}{set the cex.axis value, as in
  \code{\link{plot}}}

  \item{cex.main}{set the cex.main value, as in
  \code{\link{plot}}}

  \item{cex.lab}{set the cex.lab value, as in
  \code{\link{plot}}}

  \item{vs}{The values against which benchmark SR has to be
  plotted. can be "sharpe","correlation" or "strategies"}

  \item{ylim}{set the ylim value, as in \code{\link{plot}}}

  \item{xlim}{set the xlim value, as in \code{\link{plot}}}

  \item{\dots}{any other passthru variable}
}
\description{
  Benchmark Sharpe Ratio Plots are used to give the
  relation ship between the Benchmark Sharpe Ratio and
  average correlation,average sharpe ratio or the number of
  #'strategies keeping other parameters constant. Here
  average Sharpe ratio , average correlation stand for the
  average of all the strategies in the portfolio. The
  original point of the return series is also shown on the
  plots.

  The equation for the Benchamark Sharpe Ratio is.

  \deqn{SR_B =
  \overline{SR}\sqrt{\frac{S}{1+(S-1)\overline{\rho}}}}

  Here \eqn{S} is the number of strategies and
  \eqn{\overline{\rho}} is the average correlation across
  off diagonal elements and is given by

  \deqn{\overline{\rho} = \frac{2\sum_{s=1}^{S}
  \sum_{t=s+1}^{S} \rho_{S,t}}{S(S-1)}}
}
\examples{
data(edhec)
chart.BenchmarkSR(edhec,vs="strategies")
chart.BenchmarkSR(edhec,vs="sharpe")
}
\author{
  Pulkit Mehrotra
}
\references{
  Bailey, David H. and Lopez de Prado, Marcos, The Strategy
  Approval Decision: A Sharpe Ratio Indifference Curve
  Approach (January 2013). Algorithmic Finance, Vol. 2, No.
  1 (2013).
}
\seealso{
  \code{\link{BenchmarkSR}}
  \code{\link{chart.SRIndifference}}

  \code{\link{plot}}
}
\keyword{distribution}
\keyword{hplot}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

