\name{returns}


\alias{returns}
\alias{annualisedReturn}
\alias{periodicReturn}
\alias{averageReturn}


\title{Financial Returns}


\description{

    Returns financial returns

}


\usage{
annualisedReturn(periodPercentReturns, 
    method = c("geometric", "arithmetic"), 
    scale = c("quarterly", "monthly", "weekly", "daily"))    
     
periodicReturn(periodPercentReturns)        
averageReturn(periodPercentReturns)        
}


\arguments{
    
    \item{periodPercentReturns}{
        a univariate object of class \code{timeSeries} with 
        percentual returns whose period is given by the argument
        \code{scale}.
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
    
    the functions return a numeric value, the financial return.
    
}


\details{
     
    \tabular{ll}{
    \code{annualisedReturn} \tab Returns annualised return, \cr
    \code{periodicReturn} \tab returns period return, \cr
    \code{averageReturn} \tab a synonyme for the function periodReturn(). }
       
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

