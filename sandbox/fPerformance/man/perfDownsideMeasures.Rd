\name{DownsidePerformanceMeasures}


\alias{DownsidePerformanceMeasures}
\alias{downsideRisk}      
\alias{downsideVariance}    
\alias{downsidePotential}     
\alias{upsideRisk}            
\alias{upsideVariance}        
\alias{upsidePotential}      
\alias{downsideFrequency}     
\alias{upsideFrequency}      
\alias{omegaRatio}           
\alias{bernardoLedoitRatio}  
\alias{dRatio}               
\alias{omegaSharpeRatio}     
\alias{sortinoRatio}         
\alias{kappaRatio}          
\alias{upsidePotentialRatio} 
\alias{volatilitySkewness}   
\alias{variabilitySkewness}  
\alias{adjustedSharpeRatio}  
\alias{skewnessKurtosisRatio}
\alias{prospectRatio}        


\title{Downside Risk Performance Measures}


\description{

    Returns downside risk performance measures

}


\usage{
downsideRisk(periodPercentReturns, targetReturn = 0)          
downsideVariance(periodPercentReturns, targetReturn = 0)     
downsidePotential(periodPercentReturns, targetReturn = 0)     
upsideRisk(periodPercentReturns, targetReturn = 0)            
upsideVariance(periodPercentReturns, targetReturn = 0)        
upsidePotential(periodPercentReturns, targetReturn = 0)       
downsideFrequency(periodPercentReturns, targetReturn = 0)      
upsideFrequency(periodPercentReturns, targetReturn = 0)       
omegaRatio(periodPercentReturns, targetReturn = 0)            
bernardoLedoitRatio(periodPercentReturns)   
dRatio(periodPercentReturns)                
omegaSharpeRatio(periodPercentReturns, targetReturn = 0)     
sortinoRatio(periodPercentReturns, targetReturn = 0)          
kappaRatio(periodPercentReturns, targetReturn = 0, a = 1)            
upsidePotentialRatio(periodPercentReturns, targetReturn = 0)  
volatilitySkewness(periodPercentReturns, targetReturn = 0)    
variabilitySkewness(periodPercentReturns, targetReturn = 0)   
adjustedSharpeRatio(periodPercentReturns, riskFreeRate = 0, 
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
skewnessKurtosisRatio(periodPercentReturns)  
prospectRatio(periodPercentReturns)           
}


\arguments{
    
    \item{periodPercentReturns}{
        a univariate object of class \code{timeSeries} with 
        percentual returns whose period is given by the argument
        \code{scale}.
        }
    \item{targetReturn, riskFreeRate}{
        a numeric value, the target, or reference return, or the risk
        free rate. Note the scale must be the same as for the argument 
        \code{"periodPercentReturns"}. 
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
    \code{downsideRisk} \tab Returns downside risk, \cr          
    \code{downsideVariance} \tab returns downside variance, \cr      
    \code{downsidePotential} \tab returns downside potential, \cr     
    \code{upsideRisk} \tab returns upside risk, \cr            
    \code{upsideVariance} \tab returns upside variance, \cr        
    \code{upsidePotential} \tab returns upside potential, \cr       
    \code{downsideFrequency} \tab returns downside frequency, \cr     
    \code{upsideFrequency} \tab returns upside frequency, \cr       
    \code{omegaRatio} \tab returns Omega ratio, \cr            
    \code{bernardoLedoitRatio} \tab returns Bernardo-Ledoit ratio, \cr  
    \code{dRatio} \tab returns d ratio, \cr                
    \code{omegaSharpeRatio} \tab returns Omega Sharpe ratio, \cr     
    \code{sortinoRatio} \tab returns Sortino ratio, \cr          
    \code{kappaRatio} \tab returns kappa ratio, \cr            
    \code{upsidePotentialRatio} \tab returns upside potential ratio, \cr 
    \code{volatilitySkewness} \tab returns volatility skewness, \cr    
    \code{variabilitySkewness} \tab returns variability skewness, \cr   
    \code{adjustedSharpeRatio} \tab returns adjusted Sharpe ratio, \cr  
    \code{skewnessKurtosisRatio} \tab returns skewness kurtosis ratio, \cr
    \code{prospectRatio} \tab returns prospect ratio. }         
    
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

