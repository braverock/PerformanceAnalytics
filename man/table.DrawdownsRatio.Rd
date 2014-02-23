\name{table.DrawdownsRatio}
\alias{table.DrawdownsRatio}
\title{Drawdowns Summary: Statistics and ratios}
\usage{
table.DrawdownsRatio(R, Rf = 0, scale = NA, digits = 4)
}
\arguments{
  \item{R}{an xts, vector, matrix, data frame, timeSeries
  or zoo object of asset returns}

  \item{Rf}{risk free rate, in same period as your returns}

  \item{scale}{number of periods in a year (daily scale =
  252, monthly scale = 12, quarterly scale = 4)}

  \item{digits}{number of digits to round results to}
}
\description{
Table of Calmar ratio, Sterling ratio, Burke ratio, Pain
index, Ulcer index, Pain ratio and Martin ratio
}
\examples{
data(managers)
table.DrawdownsRatio(managers[,1:8])

require("Hmisc")
result = t(table.DrawdownsRatio(managers[,1:8], Rf=.04/12))

textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=c(3,3,1)),
rmar = 0.8, cmar = 2,  max.cex=.9, halign = "center", valign = "top",
row.valign="center", wrap.rownames=20, wrap.colnames=10,
col.rownames=c("red", rep("darkgray",5), rep("orange",2)), mar = c(0,0,3,0)+0.1)
title(main="Drawdowns ratio statistics")
}
\author{
Matthieu Lestel
}
\references{
Carl Bacon, \emph{Practical portfolio performance
measurement and attribution}, second edition 2008 p.93
}
\seealso{
\code{\link{CalmarRatio}} \cr \code{\link{BurkeRatio}} \cr
\code{\link{PainIndex}} \cr \code{\link{UlcerIndex}} \cr
\code{\link{PainRatio}} \cr \code{\link{MartinRatio}}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

