#' Generate arithmetric returns and arithmetric covariance matrix given a distribution of log returns
#' 
#' @param   mu                    a numeric containing the expected logarithmic returns for each security
#' @param   sigma                 a covariance matrix of log returns
#'
#' @result                        a list containing two elements:
#' @result  arithmeticMean          a numeric containing the mean arithmetic returns
#' @result  arithmeticCovariance    a variance-covariance matrix in simple arithmetic return terms
#'
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
#' @export
linreturn <- function( mu , sigma )
{
    # formula (7) on page 5 of Appendix to "Meucci - A Common Pitfall in Mean-Variance Estimation"
    # each element of M represents the linear returns for the corresponding log-returns element in mu
    M <- exp( mu + ( diag( sigma ) / 2 ) ) - 1
    
    # prep for formula (8)
    x1 <- outer( mu, mu , "+" )
    x2 <- outer( diag( sigma ) , diag( sigma ) , "+" ) / 2
    
    # formula (8)
    S <- exp( x1 + x2 ) * ( exp( sigma ) - 1 )
    list( mean = M , vcov = S )
}

# Example experiment with two assets

    # initialize with average log returns and log-based covariance matrix
    m1 <- c( .05 , .12 , .1 )
    S1 <- matrix( c( .1 , .05 , .02 , .05 , .1 , .03 , .02 , .03 , .1 ), nrow = 3 )

    # simulate log-return draws from log-based covariance matrix assuming normal distribution
        set.seed(1001)   
        library(MASS)
        logReturns <- MASS::mvrnorm(2000000,mu=m1,Sigma=S1)
    
        # convert to arithmetic returns
        arithmeticReturn = exp( logReturns ) - 1
        colMeans( arithmeticReturn )
        # create arithmetric based covariance matrix
        var( arithmeticReturn )

    # compare simulation results with linreturn function    
    linreturn( m1, S1 )