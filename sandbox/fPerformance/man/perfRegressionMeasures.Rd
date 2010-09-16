\name{RegressionPerformanceMeasures}


\alias{RegressionPerformanceMeasures}
\alias{regressionAlpha} 
\alias{regressionBeta} 
\alias{regressionEpsilon} 
\alias{regressionCAPM}    
\alias{jensenAlpha}   
\alias{bullBeta}     
\alias{bearBeta}          
\alias{betaTimingRatio}  
\alias{nCovariance}      
\alias{nCorrelation}     
\alias{RSquared}         
\alias{systematicRisk}   
\alias{specificRisk}     
\alias{totalRisk}        
\alias{treynorRatio}     
\alias{modTreynorRatio}  
\alias{appraisalRatio}  
\alias{modJensenAlpha}   
\alias{altModJensenAlpha}
\alias{selectivity}      
\alias{betaF}            
\alias{selectivityNet}   


\title{Regression Performance Measures}


\description{

    Returns regression performance measures

}


\usage{
regressionAlpha(periodPercentageReturn, benchmarkReturn)   
regressionBeta(periodPercentageReturn, benchmarkReturn)    
regressionEpsilon(periodPercentageReturn, benchmarkReturn) 

regressionCAPM(periodPercentageReturn, benchmarkReturn, riskFreeRate = 0)
    
jensenAlpha(periodPercentReturns, benchmarkReturns, riskFreeRate = 0,
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
           
bullBeta(periodPercentReturns, benchmarkReturns, riskFreeRate = 0)        
bearBeta(periodPercentReturns, benchmarkReturns, riskFreeRate = 0)          
betaTimingRatio(periodPercentReturns, benchmarkReturns, riskFreeRate = 0)   

nCovariance(periodPercentageReturn, benchmarkReturn)        
nCorrelation(periodPercentageReturn, benchmarkReturn)       
RSquared(periodPercentageReturn, benchmarkReturn)   
        
systematicRisk(periodPercentReturns, benchmarkReturns,
    scale = c("quarterly", "monthly", "weekly", "daily"))    
specificRisk(periodPercentReturns, benchmarkReturns,
    scale = c("quarterly", "monthly", "weekly", "daily"))                  
totalRisk(periodPercentReturns, benchmarkReturns,
    scale = c("quarterly", "monthly", "weekly", "daily"))
             
treynorRatio(periodPercentReturns, benchmarkReturns, riskFreeRate = 0)      
modTreynorRatio(periodPercentReturns, benchmarkReturns, riskFreeRate = 0)
            
appraisalRatio(periodPercentReturns,  
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))    
modJensenAlpha(periodPercentReturns,  
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))    
altModJensenAlpha(periodPercentReturns,  
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
         
selectivity(periodPercentReturns,  
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))       
betaF(periodPercentReturns,  
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))             
selectivityNet(periodPercentReturns,  
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))
}


\arguments{
    
    \item{periodPercentReturns}{
        a univariate object of class \code{timeSeries} with 
        percentual returns whose period is given by the argument
        \code{scale}.
        }
    \item{riskFreeRate}{
        a numeric value, the risk free rate. Note the scale must be 
        the same as for the argument \code{"periodPercentReturns"}. 
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
    \code{regressionAlpha} \tab Returns regression alpha, \cr                    
    \code{regressionBeta} \tab returns regression beta, \cr                     
    \code{regressionEpsilon} \tab returns regression residuals, \cr                
    \code{regressionCAPM} \tab returns CAPM alpha and beta, \cr                 
    \code{jensenAlpha} \tab returns Jensen's alpha, \cr                      
    \code{bullBeta} \tab returns Bull Beta, \cr                           
    \code{bearBeta} \tab returns Bear Beta, \cr                           
    \code{betaTimingRatio} \tab returns beta timing ratio, \cr                   
    \code{nCovariance} \tab returns momentum covariance, \cr                 
    \code{nCorrelation} \tab returns momentum ccorrelation, \cr               
    \code{RSquared} \tab returns R squared, \cr                           
    \code{systematicRisk} \tab returns annualised systematic risk, \cr          
    \code{specificRisk} \tab returns annualised specific risk, \cr            
    \code{totalRisk} \tab returns annualised total risk, \cr               
    \code{treynorRatio} \tab returns Treynor ratio, \cr                       
    \code{modTreynorRatio} \tab returns modified Treynor risk, \cr               
    \code{appraisalRatio} \tab returns appraisal ratio, \cr                     
    \code{modJensenAlpha} \tab returns annualised modified Jensen alpha, \cr   
    \code{altModJensenAlpha} \tab returns alt annualised modified Jensen alpha, \cr
    \code{selectivity} \tab returns unnualised selectivity, \cr                   
    \code{betaF} \tab returns betaF diversification, \cr                
    \code{selectivityNet} \tab returns annualised selectivity net. } 
        
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

