#'@title Implementation of PSR Portfolio Optimization
#'@description
#'Maximizing for PSR leads to better diversified and more balanced hedge fund allocations compared to the concentrated 
#'outcomes of Sharpe ratio maximization.We would like to find the vector of weights that maximize the expression
#'
#'\deqn{\hat{PSR}(SR^{*}) = Z\bigg[\frac{(\hat{SR}-SR^{*})\sqrt{n-1}}{\sqrt{1-\hat{\gamma_3}SR^{*} + \frac{\hat{\gamma_4}-1}{4}\hat{SR^2}}}\bigg]}
#'
#'where \eqn{\sigma = \sqrt{E[(r-\mu)^2]}} ,its standard deviation.\eqn{\gamma_3=\frac{E\biggl[(r-\mu)^3\biggr]}{\sigma^3}} its skewness,
#'\eqn{\gamma_4=\frac{E\bigg[(r-\mu)^4\bigg]}{\sigma^4}} its kurtosis and \eqn{SR = \frac{\mu}{\sigma}} its Sharpe Ratio.
#'Because \eqn{\hat{PSR}(SR^{*})=Z[\hat{Z^{*}}]} is a monotonic increasing function of 
#'\eqn{\hat{Z^{*}}} ,it suffices to compute the vector that maximizes \eqn{\hat{Z^{*}}}
#'
#'This optimal vector is invariant of the value adopted by the parameter \eqn{SR^{*}}. 
#'Gradient Ascent Logic is used to compute the weights using the Function PsrPortfolio
#'@aliases PsrPortfolio
#'
#'@useDynLib noniid.pm
#'@param R The return series
#'@param refSR The benchmark Sharpe Ratio
#'@param bounds The bounds for the weights
#'@param MaxIter The Maximum number of iterations
#'@param delta The value of delta Z
#'
#'@author Pulkit Mehrotra
#'@seealso \code{\link{ProbSharpeRatio}} \code{\link{table.PSR}} \code{\link{MinTrackRecord}}
#'@references Bailey, David H. and Lopez de Prado, Marcos, \emph{The Sharpe Ratio 
#'Efficient Frontier} (July 1, 2012). Journal of Risk, Vol. 15, No. 2, Winter
#'2012/13
#'
#'@keywords ts multivariate distribution models
#'
#'@examples
#'
#'data(edhec)
#'PsrPortfolio(edhec)
#'@export

PsrPortfolio<-function(R,refSR=0,bounds=NULL,MaxIter = 1000,delta = 0.005){
    # DESCRIPTION:
    # This function returns the weight for which Probabilistic Sharpe Ratio 
    # is maximized.
    #
    # INPUT:
    # The return series of the portfolio is taken as the input
    # refSR , bounds of the weights for each series in the portfolio , 
    # max iteration for the optimization , delta by which the z value will 
    # change is also taken as the input.
    #
    # OUTPUT:
    # The weights is given as the output.
    x = checkData(R)
    columns = ncol(x)
    n = nrow(x)
    columnnames = colnames(x)
    k<-rep(0,5)

    if(is.null(bounds)){
        message("Bounds not given assuming bounds to be (0,1) for each weight")
        bounds = matrix(rep(c(0,1),columns),nrow = columns,byrow = TRUE)
    }
    z = 0
    iter = 0
    w = rep(1,columns)
    d1z = 0
    #Optimization Function
    optimize<-function(){
        weights = w
        mean = NULL   
        for(column in 1:columns){
            mean = c(mean,get_Moments(x[,column],1))
        }
        while(TRUE){
            if(iter == MaxIter) break
            dZ = get_d1Zs(mean,weights)
            if(dZ$z>z && checkBounds(weights)==TRUE){
                z = dZ$z
                d1z = dZ$d1Z    
                w = weights
           }
            iter = iter + 1 
            weights = stepSize(weights,dZ$d1Z)
            if(is.null(weights)) break
       }
       return(w)
    }
    # To Check the bounds of the weights
    checkBounds<-function(weights){
        flag = TRUE
        for(i in 1:columns){
                         
            if(weights[i] < bounds[i,1]) flag = FALSE

            if(weights[i] > bounds[i,2]) flag = FALSE
        }
        return(flag)
    }

    #Calculate the step size to change the weights
    stepSize<-function(weights,d1Z){
        if(length(which(d1Z!=0)) == 0){
            return(NULL)        
        }
        index = which(abs(d1Z) ==max(abs(d1Z)))
        weights[index] = weights[index]+delta/d1Z[index]
        weights = weights/sum(weights)
        return(weights) 

    }
    #To get the first differentials
    get_d1Zs<-function(mean,weights){
        d1Z = numeric(columns)
        m = numeric(4)
        x_portfolio = x%*%weights

        m[1] = get_Moments(x_portfolio,1)
        for(i in 2:4){
            m[i] = get_Moments(x_portfolio,i,m[1])
        }
        stats = get_Stats(m)
        #mu = mean(x_portfolio)
        #sd = StdDev(x_portfolio)
        #sk = skewness(x_portfolio)
        #kr = kurtosis(x_portfolio)
        #stats = c(mu,sd,sk,kr)
        #m = c(stats[1],stats[2]^2,stats[3]*(stats[2]^3),stats[4]*(stats[2]^4))
        SR = get_SR(stats,n)
        meanSR = SR$meanSR
        sigmaSR = SR$sigmaSR
        if(refSR>meanSR){
            stop("The Reference Sharpe Ratio should be less than the Observred Sharpe Ratio")
        }
        for(i in 1:columns){

            d1Z[i] = get_d1Z(stats,m,meanSR,sigmaSR,mean,weights,i)
        }

        dZ = list("d1Z"=d1Z,"z"=(meanSR-refSR)/sigmaSR)

        return(dZ)
    }
    #To get the dZ/dw for each weight
    get_d1Z<-function(stats,m,meanSR,sigmaSR,mean,weights,index){
        d1Mu = get_d1Mu(mean,index)
        d1Sigma = get_d1Sigma(stats[2],mean,weights,index)
        d1Skew = get_d1Skew(d1Sigma,stats[2],mean,weights,index,m[3])
        d1Kurt = get_d1Kurt(d1Sigma,stats[2],mean,weights,index,m[4])
        d1meanSR = (d1Mu*stats[2]-d1Sigma*stats[1])/stats[2]^2
        d1sigmaSR = (d1Kurt * meanSR^2+2*meanSR*d1meanSR*(stats[4]-1))/4
        d1sigmaSR = d1sigmaSR-(d1Skew*meanSR+d1meanSR*stats[3])  
        d1sigmaSR = d1sigmaSR/(2*sigmaSR*(n-1))
        d1Z = (d1meanSR*sigmaSR-d1sigmaSR*(meanSR-refSR))/sigmaSR^2
        return(d1Z)
    }

    get_d1Mu<-function(mean,index){
        return(mean[index])
    }

    get_d1Sigma<-function(sigma,mean,weights,index){
        return(get_dnMoments(mean,weights,2,1,index)/(2*sigma))
    }

    get_d1Skew<-function(d1Sigma,sigma,mean,weights,index,m3){
        d1Skew = get_dnMoments(mean,weights,3,1,index)*sigma^3
        d1Skew = d1Skew - 3*(sigma^2)*d1Sigma*m3
        d1Skew = d1Skew/sigma^6
        return(d1Skew)
    }

    get_d1Kurt<-function(d1Sigma,sigma,mean,weights,index,m4){
        d1Kurt = get_dnMoments(mean,weights,4,1,index)*sigma^4
        d1Kurt = d1Kurt - 4*(sigma^3)*d1Sigma*m4
        d1Kurt = d1Kurt/sigma^8
        return(d1Kurt)
    }
    #To get the differential of the moments
    get_dnMoments<-function(mean,weights,mOrder,dOrder,index){
        sum = 0
        x0 = 1

        for(i in 0:(dOrder-1)){
            x0 = x0*(mOrder-i)
        }

        x_mat = as.matrix(na.omit(x))
        sum = 0
        output = .Call("sums",mat = x_mat,index,mean,dOrder,weights,mOrder,sum)
       #for(i in 1:n){
         #   x1 = 0
         #   x2 = (x_mat[i,index]-mean[index])^dOrder
            #if(index == 1){
            #    print(x2)
            #}
          #  for(j in 1:columns){
          #      x1 = x1 + weights[j]*(x_mat[i,j]-mean[j])
          #  }
       # sum = sum + x2*x1^(mOrder-dOrder)
       # }
        return(x0*(output)/n)
    }

    # TO get meanSR and sigmaSR
    get_SR<-function(stats,n){
        meanSR = stats[1]/stats[2]
        sigmaSR = ((1-meanSR*stats[3]+(meanSR^2)*(stats[4]-1)/4)/(n-1))^0.5
        SR<-list("meanSR"=meanSR,"sigmaSR"=sigmaSR)
        return(SR)
    }
    get_Stats<-function(m){
        return(c(m[1],m[2]^0.5,m[3]/(m[2]^1.5),m[4]/(m[2]^2)))
    }
    get_Moments<-function(series,order,mean = 0){
        sum = 0
        mat = na.omit(as.matrix(series))
        sum = .Call("sums_m",mat,mean,order)
       # for(i in series){
        #    sum = sum + (i-mean)^order
       # }
        return(sum/n)
    }
    weights = optimize()
    result = matrix(weights,nrow = columns)
    rownames(result) = columnnames
    colnames(result) = "weight"
    return(result)
}



