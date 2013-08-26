\name{chart.SRIndifference}
\alias{chart.SRIndifference}
\title{Sharpe Ratio Indifference Curve}
\usage{
  chart.SRIndifference(R, reference.grid = TRUE,
    ylab = NULL, xlab = NULL,
    main = "Sharpe Ratio Indifference Curve",
    element.color = "darkgrey", lwd = 2, pch = 1, cex = 1,
    cex.axis = 0.8, cex.lab = 1, cex.main = 1, ylim = NULL,
    xlim = NULL, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{reference.grid}{if true, draws a grid aligned with
  the points on the x and y axes}

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

  \item{cex.lab}{set the cex.lab value, as in
  \code{\link{plot}}}

  \item{cex.axis}{set the cex.axis value, as in
  \code{\link{plot}}}

  \item{cex.main}{set the cex.main value, as in
  \code{\link{plot}}}

  \item{ylim}{set the ylim value, as in \code{\link{plot}}}

  \item{xlim}{set the xlim value, as in \code{\link{plot}}}

  \item{\dots}{Any other passthru variable}
}
\description{
  The trade-off between a candidate's SR and its
  correlation to the existing set of strategies, is given
  by the Sharpe ratio indifference curve. It is a plot
  between the candidate's Sharpe Ratio and candidate's
  average correlation for a given portfolio Sharpe
  Ratio.Portfolio's sharpe Ratio remains constant if any
  strategy from the Sharpe Ratio Indifference Curve is
  added.

  The equation for the candidate's average autocorrelation
  for a given sharpe Ratio is given by

  \deqn{\bar{\rho{_s+1}}=\frac{1}{2}\biggl[\frac{\bar{SR}.S+SR_{s+1}^2}{S.SR_B^2}-\frac{S+1}{S}-\bar{rho}{S-1}\biggr]}

  This is the correlation that the candidate's strategy
  should have with the portfolio for the Sharpe Ratio of
  the portfolio to remain constant if this strategy is
  added.
}
\examples{
data(edhec)
chart.SRIndifference(edhec)
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
  \code{\link{BenchmarkSR}} \code{\link{chart.BenchmarkSR}}
  \code{\link{plot}}
}
\keyword{distribution}
\keyword{hplot}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

