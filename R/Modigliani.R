#' Modigliani-Modigliani measure
#' 
#' The Modigliani-Modigliani measure is the portfolio return adjusted upward
#' or downward to match the benchmark's standard deviation. This puts the 
#' portfolio return and the benchmark return on 'equal footing' from a standard
#' deviation perspective.
#' \deqn{MM_{p}=\frac{E[R_{p} - R_{f}]}{\sigma_{p}}=SR_{p} * \sigma_{b} + 
#' E[R_{f}]}{MMp = SRp * sigmab + E[Rf]}
#' where \eqn{SR_{p}}{SRp} - Sharpe ratio, \eqn{\sigma_{b}}{sigmab} - benchmark
#' standard deviation
#' 
#' This is also analogous to some approaches to 'risk parity' portfolios, which
#' use (presumably costless) leverage to increase the portfolio standard 
#' deviation to some target.
#' 
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param Rb return vector of the benchmark asset
#' @param Rf risk free rate, in same period as your returns
#' @param \dots any other passthrough parameters 
#' @author Andrii Babii, Brian G. Peterson
#' @references  J. Christopherson, D. Carino, W. Ferson. \emph{Portfolio 
#' Performance Measurement and Benchmarking}. 2009. McGraw-Hill, p. 97-99. \cr
#' Franco Modigliani and Leah Modigliani, "Risk-Adjusted Performance: How to 
#' Measure It and Why," \emph{Journal of Portfolio Management}, vol.23, no., 
#' Winter 1997, pp.45-54 \cr
#' @seealso \code{\link{SharpeRatio}}, \code{\link{TreynorRatio}}
#' @examples
#' 
#' data(managers)
#' Modigliani(managers[,1,drop=FALSE], managers[,8,drop=FALSE], Rf=.035/12)
#' Modigliani(managers[,1:6], managers[,8,drop=FALSE], managers[,8,drop=FALSE])
#' Modigliani(managers[,1:6], managers[,8:7], managers[,8,drop=FALSE])
#' 
#' @export
Modigliani <- function (Ra, Rb, Rf=0, ...)
{ # @author Andrii Babii, Brian G. Peterson
    Ra = checkData(Ra)
    Rb = checkData(Rb)
    if (!is.null(dim(Rf))) 
        Rf = checkData(Rf)
    Ra.ncols = NCOL(Ra)
    Rb.ncols = NCOL(Rb)
    pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)
    mm <- function(Ra, Rb, Rf){
        shr = SharpeRatio(Ra, Rf, FUN = "StdDev")
        MM = shr * StdDev(Rb) + mean(Rf)
        return(MM)
    }
    result = apply(pairs, 1, FUN = function(n, Ra, Rb, Rf) mm(Ra[, 
        n[1]], Rb[, n[2]], Rf), Ra = Ra, Rb = Rb, Rf = Rf)
    if (length(result) == 1) 
        return(result)
    else {
        dim(result) = c(Ra.ncols, Rb.ncols)
        colnames(result) = paste("Modigliani-Modigliani measure:", colnames(Rb))
        rownames(result) = colnames(Ra)
        return(t(result))
    }
}
