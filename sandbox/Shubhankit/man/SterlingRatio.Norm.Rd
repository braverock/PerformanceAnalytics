\name{SterlingRatio.Norm}
\alias{SterlingRatio.Norm}
\title{Normalized Sterling Ratio}
\usage{
  SterlingRatio.Norm(R, tau = 1, scale = NA, excess = 0.1)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{scale}{number of periods in a year (daily scale =
  252, monthly scale = 12, quarterly scale = 4)}

  \item{excess}{for Sterling Ratio, excess amount to add to
  the max drawdown, traditionally and default .1 (10\%)}
}
\description{
  Normalized Sterling Ratio is another method of creating a
  risk-adjusted measure for ranking investments similar to
  the Sharpe Ratio.
}
\details{
  Both the Normalized Sterling and the Calmar ratio are the
  ratio of annualized return over the absolute value of the
  maximum drawdown of an investment. The Sterling ratio
  adds an \bold{excess risk} measure to the maximum
  drawdown, traditionally and defaulting to 10\%.

  \deqn{Sterling Ratio = [Return over (0,T)]/[max
  Drawdown(0,T) - 10%]} It is also \emph{traditional} to
  use a three year return series for these calculations,
  although the functions included here make no effort to
  determine the length of your series.  If you want to use
  a subset of your series, you'll need to truncate or
  subset the input data to the desired length. Malik
  Magdon-Ismail impmemented a sclaing law for different
  \eqn{\mu ,\sigma and T}
}
\examples{
data(managers)
    SterlingRatio.Norm(managers[,1,drop=FALSE])
    SterlingRatio.Norm(managers[,1:6])
}
\author{
  Brian G. Peterson , Peter Carl , Shubhankit Mohan
}
\references{
  Bacon, Carl, Magdon-Ismail, M. and Amir Atiya,\emph{
  Maximum drawdown. Risk Magazine,} 01 Oct 2004.
  \url{http://www.cs.rpi.edu/~magdon/talks/mdd_NYU04.pdf}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

