\name{RelativePerformanceMeasures}


\alias{RelativePerformanceMeasures}
\alias{trackingError}         
\alias{annualisedTrackingError} 
\alias{informationRatio}         
 
  
\title{Relative Performance Measures}


\description{

    Returns relative performance measures

}


\usage{
trackingError(periodPercentReturns, benchmarkReturns,
    method = c("geometric", "arithmetic"))          
annualisedTrackingError(periodPercentReturns, benchmarkReturns,
    method = c("geometric", "arithmetic"),
    scale = c("quarterly", "monthly", "weekly", "daily"))
informationRatio(periodPercentReturns, benchmarkReturns,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))       
}


\arguments{
    
    \item{periodPercentReturns}{
        a univariate object of class \code{timeSeries} with 
        percentual returns whose period is given by the argument
        \code{scale}.
        }
    \item{benchmarkReturns}{
        a univariate object of class \class{timeSeries} with 
        percentual benchmark returns whose period is given by the 
        argument \code{scale}.
        }
    \item{method}{
        a character string, describing how the returns should be
        calculated, either \code{"geometric"} or \code{"arithmetic"}.
        }
    \item{scale}{
        a character string specifying the scale or period of the
        returns. This may be one of \code{"quarterly"}, \code{"monthly"}, 
        \code{"weekly"}, \code{"daily"}.
        }

}


\value{
    
    the functions return a numeric value, the performance measure.
    
}


\details{
     
    \tabular{ll}{    
    \code{trackingError} \tab Returns tracking error, \cr                          
    \code{annualisedTrackingError} \tab returns annualised tracking error, \cr               
    \code{informationRatio} \tab returns annualised information ratio. }               
        
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

