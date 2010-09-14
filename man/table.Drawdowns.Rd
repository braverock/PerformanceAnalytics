\name{table.Drawdowns}
\alias{table.Drawdowns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Worst Drawdowns Summary: Statistics and Stylized Facts }
\description{
Creates table showing statistics for the worst drawdowns.
}
\usage{
table.Drawdowns(R, top= 5, digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{top}{ the number of drawdowns to include }
  \item{digits}{ number of digits to round results to }
}

\references{
Bacon, C. \emph{Practical Portfolio Performance Measurement and Attribution}. Wiley. 2004. p. 88 \cr

}
\author{ Peter Carl }
\seealso{
    \code{\link{DownsideDeviation}}  \cr
    \code{\link{maxDrawdown}}   \cr
    \code{\link{findDrawdowns}} \cr
    \code{\link{sortDrawdowns}} \cr
    \code{\link{chart.Drawdown}} \cr
    \code{\link{table.DownsideRisk}}
}
\examples{
data(edhec)
table.Drawdowns(edhec[,1,drop=FALSE])
table.Drawdowns(edhec[,12,drop=FALSE])
data(managers)
table.Drawdowns(managers[,8,drop=FALSE])

result=table.Drawdowns(managers[,1,drop=FALSE])

# This was really nice before Hmisc messed up 'format' from R-base
#require("Hmisc")
#textplot(Hmisc::format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=c(rep(3,4), rep(0,3))), rmar = 0.8, cmar = 1.5,  max.cex=.9, halign = "center", valign = "top", row.valign="center", wrap.rownames=5, wrap.colnames=10, mar = c(0,0,3,0)+0.1) 
# title(main="Largest Drawdowns for HAM1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
