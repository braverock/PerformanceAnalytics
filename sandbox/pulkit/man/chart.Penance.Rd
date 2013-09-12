\name{chart.Penance}
\alias{chart.Penance}
\title{Penance vs phi plot}
\usage{
  chart.Penance(R, confidence = 0.95,
    type = c("ar", "normal"), reference.grid = TRUE,
    main = NULL, ylab = NULL, xlab = NULL,
    element.color = "darkgrey", lwd = 2, pch = 1, cex = 1,
    cex.axis = 0.8, cex.lab = 1, cex.main = 1, xlim = NULL,
    ylim = NULL, ...)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns.}

  \item{confidence}{the confidence level}

  \item{type}{The type of distribution "normal" or
  "ar"."ar" stands for Autoregressive.}

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

  \item{cex.axis}{set the cex.axis value, as in
  \code{\link{plot}}}

  \item{cex.main}{set the cex.main value, as in
  \code{\link{plot}}}

  \item{cex.lab}{set the cex.lab value, as in
  \code{\link{plot}}}

  \item{ylim}{set the ylim value, as in \code{\link{plot}}}

  \item{xlim}{set the xlim value, as in \code{\link{plot}}}

  \item{\dots}{Any other pass thru variable}
}
\description{
  A plot for Penance vs phi for the given portfolio The
  relationship between penance and phi is given by

  \deqn{penance = \frac{Maximum Time under
  water}{t_\alpha^{*}-1}}

  Penance Measures how long it takes to recover from the
  maximum drawdown as a multiple of the time it took to
  reach the bottom. Penance is smaller, the higher the
  value of \eqn{\phi(Phi)} and the higher the ratio
  \eqn{\frac{\mu}{\sigma}}. Positive serial autocorrelation
  leads to smaller Penance due to greater periods under
  water.
}
\examples{
data(edhec)
chart.Penance(edhec,0.95)
}
\author{
  Pulkit Mehrotra
}
\references{
  Bailey, David H. and Lopez de Prado,
  Marcos,Drawdown-Based Stop-Outs and the "Triple Penance"
  Rule(January 1, 2013).
}
\seealso{
  \code{\link{table.Penance}} \code{\link{MaxDD}}
  \code{\link{TuW}}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{plot}
\keyword{ts}

