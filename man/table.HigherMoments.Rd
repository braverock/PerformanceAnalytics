\name{table.HigherMoments}
\alias{table.HigherMoments}
\title{Higher Moments Summary: Statistics and Stylized Facts}
\usage{
table.HigherMoments(Ra, Rb, scale = NA, Rf = 0, digits = 4,
  method = "moment")
}
\arguments{
  \item{Ra}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{Rb}{return vector of the benchmark asset}

  \item{scale}{number of periods in a year (daily scale =
  252, monthly scale = 12, quarterly scale = 4)}

  \item{Rf}{risk free rate, in same period as your returns}

  \item{digits}{number of digits to round results to}

  \item{method}{method to use when computing
  \code{\link{kurtosis}} one of: \code{excess},
  \code{moment}, \code{fisher}}
}
\description{
Summary of the higher moements and Co-Moments of the return
distribution. Used to determine diversification potential.
Also called "systematic" moments by several papers.
}
\examples{
data(managers)
table.HigherMoments(managers[,1:3],managers[,8,drop=FALSE])
result=t(table.HigherMoments(managers[,1:6],managers[,8,drop=FALSE]))
rownames(result)=colnames(managers[,1:6])
require("Hmisc")
textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE,
         cdec=rep(3,dim(result)[2])), rmar = 0.8, cmar = 1.5,
         max.cex=.9, halign = "center", valign = "top", row.valign="center",
         wrap.rownames=5, wrap.colnames=10, mar = c(0,0,3,0)+0.1)
title(main="Higher Co-Moments with SP500 TR")
}
\author{
Peter Carl
}
\references{
Martellini L., Vaissie M., Ziemann V. Investing in Hedge
Funds: Adding Value through Active Style Allocation
Decisions. October 2005. Edhec Risk and Asset Management
Research Centre.
}
\seealso{
\code{\link{CoSkewness}} \cr \code{\link{CoKurtosis}} \cr
\code{\link{BetaCoVariance}} \cr
\code{\link{BetaCoSkewness}} \cr
\code{\link{BetaCoKurtosis}} \cr \code{\link{skewness}} \cr
\code{\link{kurtosis}}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

