#' Generate arithmetric returns and arithmetric covariance matrix given a distribution of log returns
#' 
#' @param   mu                    a numeric containing the expected logarithmic returns for each security
#' @param   sigma                 a covariance matrix of log returns
#'
#' @return                        a list containing two elements:
#' @return  arithmeticMean          a numeric containing the mean arithmetic returns
#' @return  arithmeticCovariance    a variance-covariance matrix in simple arithmetic return terms
#' \deqn{ M_{ \tau }^{i}  =  e^{  \mu ^{\tau} _{i} +  \frac{1}{2} \Sigma^{ii} _{\tau} },
#' \\  S^{ij} = e^{  \mu ^{\tau} _{i} + \mu ^{\tau} _{j} + \frac{1}{2} \big(\Sigma^{ii} _{\tau} + \Sigma^{jj} _{\tau}\big)  }  \big(e^{\Sigma^{ij} _{\tau}} - 1\big) }
#' @author Ram Ahluwalia \email{ram@@wingedfootcapital.com}
#' @export
#' @references
#' # formula (7) and (8) on page 5 of Appendix to "Meucci - A Common Pitfall in Mean-Variance Estimation"
#' \url{http://www.wilmott.com/pdfs/011119_meucci.pdf}
#' @export
linreturn <- function( mu , sigma )
{   
    # each element of M represents the linear returns for the corresponding log-returns element in mu
    M <- exp( mu + ( diag( sigma ) / 2 ) ) - 1
    
    # prep for formula (8)
    x1 <- outer( mu, mu , "+" )
    x2 <- outer( diag( sigma ) , diag( sigma ) , "+" ) / 2
    
    # formula (8)
    S <- exp( x1 + x2 ) * ( exp( sigma ) - 1 )
    list( mean = M , vcov = S )
}
