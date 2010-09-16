\name{AdjustedPerformanceMeasures}


\alias{AdjustedPerformanceMeasures}
\alias{MS2Sortino}
\alias{omegaExcessReturn}


\title{Adjusted Performance Measures}


\description{

    Returns adjusted performance measures

}


\usage{
MS2Sortino(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))

omegaExcessReturn(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
}


\arguments{
    
    \item{periodPercentReturns}{
        a univariate object of class \code{timeSeries} with 
        percentual returns whose period is given by the argument
        \code{scale}.
        }
    \item{targetReturn}{
        a numeric value, the target or reference return. Note the scale 
        must be the same as for the argument \code{"periodPercentReturns"}. 
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
    \code{MS2Sortino} \tab Returns the M2 Sortino ratio, \cr
    \code{omegaExcessReturn} \tab returns the annualised M2 Omega excess return. }
    
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

