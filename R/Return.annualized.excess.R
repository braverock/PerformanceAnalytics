#' calculates an annualized excess return for comparing instruments with different
#' length history
#' 
#' An average annualized excess return is convenient for comparing excess 
#' returns.
#' 
#' Annualized returns are useful for comparing two assets. To do so, you must
#' scale your observations to an annual scale by raising the compound return to
#' the number of periods in a year, and taking the root to the number of total 
#' observations:
#' \deqn{prod(1+R_{a})^{\frac{scale}{n}}-1=\sqrt[n]{prod(1+R_{a})^{scale}}-
#' 1}{prod(1 + Ra)^(scale/n) - 1}
#' 
#' where scale is the number of periods in a year, and n is the total number of
#' periods for which you have observations.
#' 
#' Finally having annualized returns for portfolio and benchmark we can compute
#' annualized excess return as difference in the annualized portfolio and 
#' benchmark returns in the arithmetic case:
#' \deqn{er = R_{pa} - R_{ba}}{er = Rpa - Rba}
#' 
#' and as a geometric difference in the geometric case:
#' \deqn{er = \frac{(1 + R_{pa})}{(1 + R_{ba})} - 1}{er = (1 + Rpa) / (1 + Rba) - 1}
#' 
#' @param Rp an xts, vector, matrix, data frame, timeSeries or zoo object of
#' portfolio returns
#' @param Rb an xts, vector, matrix, data frame, timeSeries or zoo object of 
#' benchmark returns
#' @param scale number of periods in a year (daily scale = 252, monthly scale =
#' 12, quarterly scale = 4)
#' @param geometric generate geometric (TRUE) or simple (FALSE) excess returns,
#' default TRUE
#' @author Andrii Babii
#' @seealso \code{\link{Return.annualized}},
#' @references Bacon, Carl. \emph{Practical Portfolio Performance Measurement
#' and Attribution}. Wiley. 2004. p. 206-207
###keywords ts multivariate distribution models
#' @examples
#' data(managers)
#' Return.annualized.excess(Rp = managers[,1], Rb = managers[,8])
#' 
#' @export
Return.annualized.excess <- 
function (Rp, Rb, scale = NA, geometric = TRUE )
{ # @author Andrii Babii
    Rp = checkData(Rp)
    Rb = checkData(Rb)
    
    Rp = na.omit(Rp)
    Rb = na.omit(Rb)
    n = nrow(Rp)
    if(is.na(scale)) {
      freq = periodicity(Rp)
      switch(freq$scale,
             minute = {stop("Data periodicity too high")},
             hourly = {stop("Data periodicity too high")},
             daily = {scale = 252},
             eekly = {scale = 52},
             monthly = {scale = 12},
             quarterly = {scale = 4},
             yearly = {scale = 1}
             )
    }
    Rpa = apply(1 + Rp, 2, prod)^(scale/n) - 1
    Rba = apply(1 + Rb, 2, prod)^(scale/n) - 1
    if (geometric) {
      # geometric excess returns
      result = (1 + Rpa) / (1 + Rba) - 1
    } else {
      # arithmetic excess returns
      result = Rpa - Rba
    }
    dim(result) = c(1,NCOL(Rp))
    colnames(result) = colnames(Rp)
    rownames(result) = "Annualized Return"
    return(result)
}