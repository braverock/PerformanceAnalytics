#' @title
#' Golden Section Algorithm
#'
#' @description
#' @reference Bailey, David H. and Lopez de Prado, Marcos, Drawdown-Based Stop-Outs and the ‘Triple Penance’ Rule(January 1, 2013).


golden_section<-function(a,b,minimum = TRUE,function_name,confidence,...){

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
        return(list(min_value=sign*f1,x=x1))
    }
    else{
        return(list(min_value=sign*f2,x=x2))
    }
}   
      




