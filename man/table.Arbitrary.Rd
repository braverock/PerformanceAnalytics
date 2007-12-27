\name{table.Arbitrary}
\alias{table.Arbitrary}
\alias{statsTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ wrapper function for combining arbitrary function list into a table }
\description{
This function creates a table of statistics from vectors of functions and labels passed in.  The resulting table is formatted such that metrics are calculated separately for each column of returns in the data object.

Assumes an input of period returns.  Scale arguements can be used to specify the number of observations during a year (e.g., 12 = monthly returns).
}
\usage{
table.Arbitrary(R, metrics = c("mean", "sd"), metricsNames = c("Average Return", "Standard Deviation"), ...)
statsTable(R, metrics = c("mean", "sd"), metricsNames = c("Average Return", "Standard Deviation"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{ a vector, matrix, data frame, timeSeries or zoo object of asset returns }
  \item{metrics}{ lisdt of functions to apply }
  \item{metricsNames}{ column names for each function }
  \item{\dots}{ any other passthru parameters }
}
\details{
The idea here is to be able to pass in sets of metrics and values, like:

metrics = c(DownsideDeviation(x,MAR=mean(x)), sd(subset(x,x>0)),
sd(subset(x,x<0)), DownsideDeviation(x,MAR=MAR),
DownsideDeviation(x,MAR=rf), DownsideDeviation(x,MAR=0),maxDrawdown(x))

metricsNames = c("Semi Deviation", "Gain Deviation", "Loss Deviation",
paste("Downside Deviation (MAR=",MAR*scale*100,"\%)", sep=""),
paste("Downside Deviation (rf=",rf*scale*100,"\%)", sep=""),
paste("Downside Deviation (0\%)", sep=""), "Maximum Drawdown" )

Here's how it's working right now:
> statsTable(monthlyReturns.ts,metrics=c("VaR.CornishFisher","mean"),
metricsNames=c("modVaR","mean"),p=.95)
\preformatted{
           Actual   S&P500TR
modVaR 0.04186461 0.06261451
mean   0.00945000 0.01013684
}

Passing in attributes doesn't quite work corrently.  The issue is apparent in:
> statsTable(monthlyReturns.ts,metrics=c("VaR.CornishFisher", "VaR.CornishFisher"),
  metricsNames=c("Modified VaR","Traditional VaR"), modified=c(TRUE,FALSE))
\preformatted{
                    Actual   S&P500TR
Modified VaR    0.06340849 0.09334976
Traditional VaR 0.06340849 0.09334976
}
Warning messages:
1: the condition has length > 1 and only the first element will be used in: if (modified)
2: the condition has length > 1 and only the first element will be used in: if (modified)
3: the condition has length > 1 and only the first element will be used in: if (modified)
4: the condition has length > 1 and only the first element will be used in: if (modified)
}
\value{
A table with calculated metrics for each column
}
\references{ }
\author{ Peter Carl }
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }
\keyword{ multivariate }
\keyword{ distribution }
\keyword{ models }