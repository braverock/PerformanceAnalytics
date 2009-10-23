\name{table.Correlation}
\alias{table.Correlation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ calculate correlalations of multicolumn data }
\description{
This is a wrapper for calculating correlation and significance against each column of the data provided.
}
\usage{
table.Correlation(Ra, Rb, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ra}{ a vector of returns to test, e.g., the asset to be examined }
  \item{Rb}{ a matrix, data.frame, or timeSeries of benchmark(s) to test the asset against. }
  \item{\dots}{ any other passthru parameters to \code{\link{cor.test}} }
}
\author{ Peter Carl }
\seealso{\code{\link{cor.test}}}
\examples{
# First we load the data
data(managers)
table.Correlation(managers[,1:6],managers[,7:8])

result=table.Correlation(managers[,1:6],managers[,8])
rownames(result)=colnames(managers[,1:6])
require("Hmisc")
textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=rep(3,dim(result)[2])), rmar = 0.8, cmar = 1.5,  max.cex=.9, halign = "center", valign = "top", row.valign="center", wrap.rownames=20, wrap.colnames=10, mar = c(0,0,3,0)+0.1)
title(main="Correlations to SP500 TR")

ctable = table.Correlation(managers[,1:6],managers[,8,drop=FALSE], conf.level=.99)
dotchart(ctable[,1],labels=rownames(ctable),xlim=c(-1,1))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
