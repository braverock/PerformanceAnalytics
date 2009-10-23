\name{table.Stats}
\alias{table.Stats}
\alias{table.MonthlyReturns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Returns Summary: Statistics and Stylized Facts }
\description{
Returns a basic set of statistics that match the period of the data passed in (e.g., monthly returns will get monthly statistics, daily will be daily stats, and so on)}
\usage{
table.Stats(R, ci = 0.95, digits = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{ci}{ confidence interval, defaults to 95\% }
  \item{digits}{ number of digits to round results to }
}
\details{
This was created as a way to display a set of related statistics together for comparison across a set of instruments or funds.  Careful consideration to missing data or unequal time series should be given when intepreting the results.
}

\author{ Peter Carl }
\examples{
data(edhec)
table.Stats(edhec[,1:3])
t(table.Stats(edhec))

result=t(table.Stats(edhec))
require("Hmisc")
textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=c(rep(1,2),rep(3,14))), rmar = 0.8, cmar = 1.5,  max.cex=.9, halign = "center", valign = "top", row.valign="center", wrap.rownames=10, wrap.colnames=10, mar = c(0,0,3,0)+0.1)
title(main="Statistics for EDHEC Indexes")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }
