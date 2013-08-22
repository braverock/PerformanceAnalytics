#'@title
#'Modelling Drawdown using Extreme Value Theory
#'
#"@description
#'It has been shown empirically that Drawdowns can be modelled using Modified Generalized Pareto 
#'distribution(MGPD), Generalized Pareto Distribution(GPD) and other particular cases of MGPD such 
#'as weibull distribution \eqn{MGPD(\gamma,0,\psi)} and unit exponential distribution\eqn{MGPD(1,0,\psi)}
#'
#' Modified Generalized Pareto Distribution is given by the following formula
#'
#' \dqeqn{G_{\eta}(m) = \begin{array}{l} 1-(1+\eta\frac{m^\gamma}{\psi})^(-1/\eta), if \eta \neq 0 \\ 1- e^{-frac{m^\gamma}{\psi}}, if \eta = 0,\end{array}}
#'
#' Here \eqn{\gamma{\epsilon}R} is the modifying parameter. When \eqn{\gamma<1} the corresponding densities are
#' strictly decreasing with heavier tail; the GDP is recovered by setting \eqn{\gamma = 1} .\eqn{\gamma \textgreater 1}
#' 
#' The GDP is given by the following equation. \eqn{MGPD(1,\eta,\psi)}
#'
#'\deqn{G_{\eta}(m) = \begin{array}{l} 1-(1+\eta\frac{m}{\psi})^(-1/\eta), if \eta \neq 0 \\ 1- e^{-frac{m}{\psi}}, if \eta = 0,\end{array}}
#'
#' The weibull distribution is given by the following equation \eqn{MGPD(\gamma,0,\psi)}
#'
#'\deqn{G(m) =  1- e^{-frac{m^\gamma}{\psi}}}
#'
#'The unit exponential distribution is given by the following equation \eqn{MGPD(1,0,\psi)}
#'
#'\deqn{G(m) =  1- e^{-m}}
#'
#'
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset return 
#' @param type The type of distribution "gpd","pd","weibull"
#' @param threshold The threshold beyond which the drawdowns have to be modelled
#' 
#'@references
#'Mendes, Beatriz V.M. and Leal, Ricardo P.C., Maximum Drawdown: Models and Applications (November 2003). Coppead Working Paper Series No. 359. 
#'Available at SSRN: http://ssrn.com/abstract=477322 or http://dx.doi.org/10.2139/ssrn.477322.
#'
#'
DrawdownGPD<-function(R,type=c("gpd","pd","weibull"),threshold=0.90){
    x = checkData(R)
    columns = ncol(R)
    columnnames = colnames(R)
    type = type[1]
    dr = -Drawdowns(R)
    dr_sorted = sort(as.vector(dr))
    #data = dr_sorted[(0.9*nrow(R)):nrow(R)]
    if(type=="gpd"){
        gpd_fit = gpd(dr_sorted,dr_sorted[threshold*nrow(R)])
        return(gpd_fit)
    }
    if(type=="wiebull"){
        weibull = fitdistr(data,"weibull")
        return(weibull)
    }
    if(type=="pd"){
        scale = min(data)
        shape = length(data)/(sum(log(data))-length(data)*log(a))
    }
    

}









