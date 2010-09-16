\name{DrawdownPerformanceMeasures}


\alias{DrawdownPerformanceMeasures}
\alias{averageDrawdown}        
\alias{maximumDrawdown}          
\alias{continuousDrawdown}         
\alias{largestIndividualDrawdown}  
\alias{averageNLargestDrawdown}    
\alias{recoveryTime}              
\alias{drawdownDeviation}          
\alias{ulcerIndex}                
\alias{painIndex}                 
\alias{calmarRatio}               
\alias{sterlingRatio}             
\alias{sterlingCalmarRatio}       
\alias{burkeRatio}                
\alias{modBurkeRatio}            
\alias{martinRatio}               
\alias{painRatio}                 
\alias{lakeRatio}                 
\alias{peakRatio}                 
     

\title{Drawdown Risk Performance Measures}


\description{

    Returns drawdown risk performance measures

}


\usage{
averageDrawdown(periodPercentReturns)            
maximumDrawdown(periodPercentReturns)           
continuousDrawdown(periodPercentReturns)         
largestIndividualDrawdown(periodPercentReturns)  
averageNLargestDrawdown(periodPercentReturns, n = 1)    
recoveryTime(periodPercentReturns)               
drawdownDeviation(periodPercentReturns)        
ulcerIndex(periodPercentReturns)                
painIndex(periodPercentReturns)
                  
calmarRatio(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))              
sterlingRatio(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))            
sterlingCalmarRatio(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))      
burkeRatio(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))                 
modBurkeRatio(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))                     
martinRatio(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))                
painRatio(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))                  
lakeRatio(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))                  
peakRatio(periodPercentReturns, targetReturn = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))               
}


\arguments{
    
    \item{periodPercentReturns}{
        a univariate object of class \class{timeSeries} with 
        percentual returns whose period is gioven by the argument
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
    \item{n}{
        an integer specifying the number of the n largest drawdowns to
        be used for the calculation of the average.
        }

}


\value{
    
    the functions return a numeric value, the performance measure.
    
}


\details{
     
    \tabular{ll}{    
    \code{averageDrawdown} \tab Returns average drawdown, \cr                
    \code{maximumDrawdown} \tab returns maximum drawdown, \cr                
    \code{continuousDrawdown} \tab returns continuous drawdown, \cr             
    \code{largestIndividualDrawdown} \tab returns largest individual drawdown, \cr     
    \code{averageNLargestDrawdown} \tab returns average of N largest drawdowns, \cr  
    \code{recoveryTime} \tab returns recovery time, \cr                   
    \code{drawdownDeviation} \tab returns drawdown deviation, \cr              
    \code{ulcerIndex} \tab returns Ulcer index, \cr                     
    \code{painIndex} \tab returns Pain index, \cr                      
    \code{calmarRatio} \tab returns annualized Calmar Ratio, \cr         
    \code{sterlingRatio} \tab returns annualised Sterling ratio, \cr       
    \code{sterlingCalmarRatio} \tab returns annualised Sterling-Calmar ratio, \cr
    \code{burkeRatio} \tab returns annualised Burke ratio, \cr          
    \code{modBurkeRatio} \tab returns annualised modified Burke ratio, \cr 
    \code{martinRatio} \tab returns annualised Martin ratio, \cr         
    \code{painRatio} \tab returns annualised Pain ratio, \cr           
    \code{lakeRatio} \tab returns annualised Lake ratio, \cr           
    \code{peakRatio} \tab returns annualised peak ratio. }           
     
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

