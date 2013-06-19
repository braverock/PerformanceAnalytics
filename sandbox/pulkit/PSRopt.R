#'@title Implementation of PSR Portfolio Optimization
#'@description
#'Maximizing for PSR leads to better diversified and more balanced hedge fund allocations compared to the concentrated outcomes of Sharpe ratio maximization.We would like to find the vector of weights that maximize the expression.Gradient Ascent Logic is used to compute the weights using the Function PsrPortfolio
#'@param R The return series
#'@param bounds The bounds for the weights
#'@param MaxIter The Maximum number of iterations
#'@param delta The value of delta Z

PsrPortfolio<-function(R,bounds=NULL,MaxIter = 1000,delta = 0.005){

    x = checkData(R)
    columns = ncol(x)
    n = nrow(x)
    columnnames = colnames(x)


    if(is.null(bounds)){
        bounds = matrix(rep(c(0,1),columns),nrow = columns,byrow = TRUE)
    }

    #Optimization Function
    optimize<-function(){
        weights = rep(1,columns)/columns
        d1z = 0
        z = 0
        iter = 0
        mean = NULL   
        for(column in 1:columns){
            mean = c(mean,mean(x[,column]))
        }
        flag = TRUE
        while(flag){
            if(iter == MaxIter) break
            dZ = get_d1Zs(mean,weights)
            if(dZ$z<z | checkBounds(weights)==FALSE){
                break
           }
           z = dZ$z
           d1z = dZ$d1Z
            iter = iter + 1 
            weights = stepSize(weights,d1z)
            print(z)
            if(is.null(weights)) return
       }
       return(weights)
    }
    # To Check the bounds of the weights
    checkBounds<-function(weights){
        flag = TRUE
        #for(i in 1:columns){
         #   if(weights[i] < bounds[i,0]) flag = FALSE

          #  if(weights[i] > bounds[i,1]) flag = FALSE
        #}
        return(TRUE)
    }

    #Calculate the step size to change the weights
    stepSize<-function(weights,d1Z){
        if(length(which(d1Z!=0)) == 0){
            return(NULL)        
        }
        weights[which(abs(d1Z)==max(abs(d1Z)))] = weights[which(abs(d1Z)==max(abs(d1Z)))]+delta/max(d1Z)
        # OR all the weights should be changed ?
        #weights = weights + delta/d1Z
        weights = weights/sum(weights)
        return(weights) 

    }
    #To get the first differentials
    get_d1Zs<-function(mean,weights){
        d1Z = NULL
        m = NULL
        x_portfolio = Return.portfolio(x,weights)
        mu = mean(x_portfolio)
        sd = StdDev(x_portfolio)
        sk = skewness(x_portfolio)
        kr = kurtosis(x_portfolio)
        stats = c(mu,sd,sk,kr)
        m = c(stats[1],stats[2]^2,stats[3]*(stats[2]^3),stats[4]*(stats[2]^2))
        SR = get_SR(stats,n)
        meanSR = SR$meanSR
        sigmaSR = SR$sigmaSR
        for(i in 1:columns){
            d1Z = c(d1Z,get_d1Z(stats,m,meanSR,sigmaSR,mean,weights,i))
        }
        dZ = list("d1Z"=d1Z,"z"=meanSR/sigmaSR)

        return(dZ)
    }

    get_d1Z<-function(stats,m,meanSR,sigmaSR,mean,weights,index){
        d1Mu = get_d1Mu(mean,index)
        d1Sigma = get_d1Sigma(stats[2],mean,weights,index)
        d1Skew = get_d1Skew(d1Sigma,stats[2],mean,weights,index,m[2])
        d1Kurt = get_d1Kurt(d1Sigma,stats[2],mean,weights,index,m[3])
        d1meanSR = (d1Mu*stats[2]-d1Sigma*stats[1])/stats[2]^2
        d1sigmaSR = (d1Kurt * meanSR^2+2*meanSR*d1meanSR*(stats[4]-1))/4
        d1sigmaSR = d1sigmaSR - d1Skew*meanSR+d1meanSR*stats[3]    
        d1sigmaSR = (d1sigmaSR/2)*sigmaSR*(n-1)
        d1Z = (d1meanSR*sigmaSR-d1sigmaSR*meanSR)/sigmaSR^2
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
        d1Skew = d1Skew - 3*sigma^2*d1Sigma*m3
        d1Skew = d1Skew/sigma^6
        return(d1Skew)
    }

    get_d1Kurt<-function(d1Sigma,sigma,mean,weights,index,m4){
        d1Kurt = get_dnMoments(mean,weights,4,1,index)*sigma^4
        d1Kurt = d1Kurt - 4*sigma^3*d1Sigma*m4
        d1Kurt = d1Kurt/sigma^8
        return(d1Kurt)
    }

    get_dnMoments<-function(mean,weights,mOrder,dOrder,index){
        sum = 0
        x0 = 1
        for(i in 1:dOrder){
            x0 = x0*(mOrder-i)
        }
        x_mat = as.matrix(na.omit(x))
        for(i in 1:n){
            x1 = 0
            x2 = (x_mat[i,index]-mean[index])^dOrder
            for(j in 1:columns){
                x1 = x1 + weights[j]*(x_mat[i,j]-mean[j])
            }
        sum = sum + x2*x1^(mOrder-dOrder)
        }
        return(x0*sum/n)
    }

    # TO get meanSR and sigmaSR
    get_SR<-function(stats,n){
        meanSR = stats[1]/stats[2]
        sigmaSR = ((1-meanSR*stats[3]+(meanSR^2)*(stats[4]-1)/4)/(n-1))^0.5
        SR<-list("meanSR"=meanSR,"sigmaSR"=sigmaSR)
        return(SR)
    }

weights = optimize()
return(weights)
}





