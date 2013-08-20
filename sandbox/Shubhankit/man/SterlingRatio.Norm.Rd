\name{SterlingRatio.Norm}
\alias{SterlingRatio.Norm}
\title{Normalized Sterling reward/risk ratio}
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
  Normalized Sterling and Sterling Ratios are yet another
  method of creating a risk-adjusted measure for ranking
  investments similar to the Sharpe Ratio.
}
\details{
  Both the Normalized Sterling and the Calmar ratio are the
  ratio of annualized return over the absolute value of the
  maximum drawdown of an investment. The Sterling ratio
  adds an excess risk measure to the maximum drawdown,
  traditionally and defaulting to 10%.

  It is also traditional to use a three year return series
  for these calculations, although the functions included
  here make no effort to determine the length of your
  series.  If you want to use a subset of your series,
  you'll need to truncate or subset the input data to the
  desired length.
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
  Bacon, Carl. \emph{Magdon-Ismail, M. and Amir Atiya,
  Maximum drawdown. Risk Magazine, 01 Oct 2004.
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

