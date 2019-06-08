% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Level.calculate.R, R/Level.calculateV2.R
\name{Level.calculate}
\alias{Level.calculate}
\alias{isLeap}
\alias{calculateImpliedDate}
\alias{isLeap}
\alias{calculateImpliedDate}
\title{Calculate appropriate cumulative return series or asset level using xts attribute information}
\usage{
Level.calculate(R, seedValue = NULL, initial = TRUE)

isLeap(yearNumber)

calculateImpliedDate(tsObject)

isLeap(yearNumber)

calculateImpliedDate(tsObject)
}
\arguments{
\item{R}{an xts object}

\item{seedValue}{a numeric scalar indicating the (usually initial) index level or price of the series}

\item{initial}{(default TRUE) a TRUE/FALSE flag associated with 'seedValue', indicating if this value is at the begginning of the series (TRUE) or at the end of the series (FALSE)}
}
\description{
This function calculates the time varying index level over 
the entire period of available data.  It will work with arithmetic or log returns, using attribute information from an xts object.
If the first value in the left-most column is NA, it will be populated with the seedValue.  
However, if the first value in the left-most column is not NA, the previous date will be estimated
based on the periodicity of the time series, and be populated with the seedValue.
This is so that information is not lost in case levels are converted back to returns 
(where the first value would result in an NA).  Note: previous date does not consider weekdays or
holidays, it will simply calculate the previous calendar day.  
If the user has a preference, they should ensure that the first row has the appropriate time index with an NA value.  
If users run Return.calculate() from this package, this will be a non-issue.
}
\details{
Product of all the individual period returns

For arithmetic returns:
\deqn{(1+r_{1})(1+r_{2})(1+r_{3})\ldots(1+r_{n})=cumprod(1+R)}{cumprod(1+R)}

For log returns:
\deqn{exp(r_{1}+r_{2}+r_{3} + \ldots + r_{n})=exp(cumsum(R))}{exp(cumsum(R))}
}
\examples{

data(managers)
managers <- managers[,1:6] #equities only
xtsAttributes(managers) <- list(coredata_content = "discreteReturn") #append attribute
chart.CumReturns(managers) #here is baseline
managersL <- Level.calculate(R = managers)
plot(managersL-1)

#Here they are equal
Return.cumulative(managers)  #Baseline
last(managersL-1) #This function

}
\seealso{
\code{\link{Return.calculate}}
}
\author{
Erol Biceroglu
}