\name{VaRPerformanceMeasures}


\alias{VaRPerformanceMeasures}
\alias{normalVaR}         
\alias{normalVaRRatio}       
\alias{normalRewardToVaR}     
\alias{conditionalVaR}       
\alias{conditionalSharpeRatio}
\alias{modifiedVaR}           
\alias{modSharpeRatio}       


\title{Value at Risk Performance Measures}


\description{

    Returns relative performance measures

}


\usage{
normalVaR(periodPercentReturns, probability = 0.95)             
normalVaRRatio(periodPercentReturns, probability = 0.95, nAssets = 1)        
normalRewardToVaR(periodPercentReturns, targetReturn = 0, probability = 0.95, 
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"), nAssets = 1)     
conditionalVaR(periodPercentReturns, targetReturn = 0, probability = 0.95,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))        
conditionalSharpeRatio(periodPercentReturns, targetReturn = 0, probability = 0.95,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
modifiedVaR(periodPercentReturns, targetReturn = 0, probability = 0.95,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))           
modSharpeRatio(periodPercentReturns, targetReturn = 0, probability = 0.95,
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
    \item{probability}{
        a numeric value, the VaR significance level, by default 0.95.
        }
    \item{nAssets}{
        an integer value, the number of assets.
        }
    

}


\value{
    
    the functions return a numeric value, the performance measure.
    
}


\details{
     
    \tabular{ll}{       
    \code{normalVaR} \tab Returns normal VaR, \cr
    \code{normalVaRRatio} \tab returns normal VaR ratio, \cr
    \code{normalRewardToVaR} \tab returns annualised normal reward to VaR ratio, \cr
    \code{conditionalVaR} \tab returns unnualised conditional VaR, \cr
    \code{conditionalSharpeRatio} \tab rReturns conditional VaR Sharpe ratio, \cr
    \code{modifiedVaR} \tab returns annualised Cornish-Fishers modified VaR, \cr
    \code{modSharpeRatio} \tab returns annualised modified Sharpe ratio. }   
    
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

