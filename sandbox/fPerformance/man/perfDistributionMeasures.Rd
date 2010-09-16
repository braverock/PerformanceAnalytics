\name{DistributionalPerformanceMeasures}


\alias{DistributionalPerformanceMeasures}
\alias{nSkewness}          
\alias{sampleSkewness}      
\alias{nKurtosis}            
\alias{excessKurtosis}       
\alias{sampleKurtosis}       
\alias{sampleExcessKurtosis} 
\alias{jarqueBeraStatistic}  


\title{Distributional Performance Measures}


\description{

    Returns distributional performance measures

}


\usage{
nSkewness(periodPercentReturns)           
sampleSkewness(periodPercentReturns)      
nKurtosis(periodPercentReturns)          
excessKurtosis(periodPercentReturns)      
sampleKurtosis(periodPercentReturns)      
sampleExcessKurtosis(periodPercentReturns)
jarqueBeraStatistic(periodPercentReturns)    
}


\arguments{
    
    \item{periodPercentReturns}{
        a univariate object of class \code{timeSeries} with 
        percentual periodic returns.
        }
 
}


\value{
    
    the functions return a numeric value, the performance measure.
    
}


\details{
     
    \tabular{ll}{   
    \code{nSkewness} \tab Returns moment skewness, \cr
    \code{sampleSkewness} \tab returns sample skewness, \cr
    \code{nKurtosis} \tab returns moment kurtosis, \cr
    \code{excessKurtosis} \tab returns excess kurtosis, \cr
    \code{sampleKurtosis} \tab returns sample kurtosis, \cr
    \code{sampleExcessKurtosis} \tab returns sample excess kurtosis, \cr
    \code{jarqueBeraStatistic} \tab returns Jarque Bera statistics. }
     
    For the definitions and mathematical formulas we refer to Carl Bacon's
    book (2008). 
           
}


\author{

    Diethelm Wuertz for this \R-port.
    
}


\references{

Bacon, C., (2008);
    \emph{Practical Portfolio Performance Measurement and Attribution}, 
    2nd Edtion, Wiley. 
     
}


\keyword{models}

