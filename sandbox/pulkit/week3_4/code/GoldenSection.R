#' @title
#' Golden Section Algorithm
#'
#' @description
#'
#' The Golden Section Search method is used to find the maximum or minimum of a unimodal
#" function. (A unimodal function contains only one minimum or maximum on the interval 
#' [a,b].) To make the discussion of the method simpler, let us assume that we are trying to find 
#' the maximum of a function. choose three points \eqn{x_l},\eqn{x_1} and \eqn{x_u} \eqn{(x_l \textless x_1 \textless x_u)}
#' along the x-axis with the corresponding values of the function \eqn{f(x_l)},\eqn{f(x_1)} and \eqn{f(x_u)}, respectively. Since
#' \eqn{f(x_1)\textgreater f(x_l)} and \eqn{f(x_1) \textgreater f(x_u)}, the maximum must lie between \eqn{x_l} and \eqn{x_u}. Now 
#' a fourth point denoted by \eqn{x_2} is chosen to be between the larger of the two intervals of \eqn{[x_l,x_1]} and \eqn{[x_1,x_u]}/
#' Assuming that the interval \eqn{[x_l,x_1]} is larger than the interval \eqn{[x_1,x_u]} we would choose \eqn{[x_l,x_1]} as the interval 
#' in which \eqn{x_2} is chosen. If \eqn{f(x_2)>f(x_1)} then the new three points would be \eqn{x_l \textless x_2 \textless x_1} else if
#' \eqn{f(x_2)<f(x_1)} then the three new points are \eqn{x_2<x_1<x_u}. This process is continued until the distance between the outer point
#' is sufficiently small.

#' @reference Bailey, David H. and Lopez de Prado, Marcos, Drawdown-Based Stop-Outs and the ‘Triple Penance’ Rule(January 1, 2013).
#' 
#'@param a initial point
#'@param b final point
#'@param minimum TRUE to calculate the minimum and FALSE to calculate the Maximum
#'@param function_name The name of the function  

golden_section<-function(a,b,minimum = TRUE,function_name,...){

    # DESCRIPTION
    # A function to perform the golden search algorithm on the provided function

    # Inputs:
    #
    # a: The starting point
    #
    # b: The end point
    #
    # minimum: If we want to calculate the minimum set minimum= TRUE(default)
    #
    # function_name: The name of the function
  
    FUN = match.fun(function_name)
    tol = 10^-9
    sign = 1 
    
    if(!minimum){
        sign = -1
    }
    N = round(ceiling(-2.078087*log(tol/abs(b-a))))
    r = 0.618033989
    c = 1.0 - r
    x1 = r*a + c*b
    x2 = c*a + r*b
    f1 = sign * FUN(x1,...)
    f2 = sign * FUN(x2,...)
    for(i in 1:N){
        if(f1>f2){
            a = x1
            x1 = x2
            f1 = f2
            x2 = c*a+r*b
            f2 = sign*FUN(x2,...)
        }
        else{
            b = x2
            x2 = x1
            f2 = f1
            x1 = r*a + c*b
            f1 = sign*FUN(x1,...)
    }
    }
    if(f1<f2){
        return(list(value=sign*f1,x=x1))
    }
    else{
        return(list(value=sign*f2,x=x2))
    }
}   
      




