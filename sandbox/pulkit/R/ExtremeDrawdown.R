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
#' \deqn{
#' G_{\eta}(m) = \begin{array}{l} 1-(1+\eta\frac{m^\gamma}{\psi})^(-1/\eta), if \eta \neq 0 \\ 1- e^{-frac{m^\gamma}{\psi}}, if \eta = 0,\end{array}}
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
#'In this function generalized Pareto distribution has been covered. This function can be 
#'expanded in the future to include more Extreme Value distributions as the literature on such distribution
#'matures in the future. 
#'
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset return 
#' @param threshold The threshold beyond which the drawdowns have to be modelled
#'
#'
#'@references
#'Mendes, Beatriz V.M. and Leal, Ricardo P.C., Maximum Drawdown: Models and Applications (November 2003). 
#'Coppead Working Paper Series No. 359.Available at SSRN: http://ssrn.com/abstract=477322 or http://dx.doi.org/10.2139/ssrn.477322.
#'
#'@examples
#'data(edhec)
#'DrawdownGPD(edhec)
#'data(managers)
#'DrawdownGPD(managers[,1:9],0.95)
#'
#'@export
DrawdownGPD<-function(R,threshold=0.90){
    x = checkData(R)
    columns = ncol(R)
    columnnames = colnames(R)
    gpdfit<-function(data,threshold){
            gpd_fit = gpd(as.vector(data),as.vector(threshold))
            result = list(shape = gpd_fit$param[2],scale = gpd_fit$param[1])
            return(result)
    }
    for(column in 1:columns){
        dr = -Drawdowns(R[,column])
        thresh = quantile(na.omit(dr),threshold)
        column.parameters = gpdfit(dr,thresh)
            if(column == 1){
                shape = column.parameters$shape
                scale = column.parameters$scale
            }
            else {
                scale = c(scale, column.parameters$scale) 
                shape = c(shape, column.parameters$shape)
            }
    }
    parameters = rbind(scale,shape)
    colnames(parameters) = columnnames
    parameters = reclass(parameters, x)
    rownames(parameters)=c("scale","shape")
    return(parameters)
}









