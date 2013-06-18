PsrPortfolio<-function(R,bounds=NULL,MaxIter = 1000,delta = 0.05){

    x = checkData(R)
    columns = ncol(x)
    n = nrow(x)
    columnnames = colnames(x)

    weights = rep(1,columns)/columns

    if(is.null(bounds)){
        bounds = matrix(rep(c(0,1),columns),nrow = columns,byrow = TRUE)
    }


     

    optimize<-function(){
    
    mean = NULL   
    for(column in 1:columns){
        mean = c(mean,get_Moments(x[,column],1))
    }
    while(TRUE){
        if(iter == MaxIter) break
        c(d1z,z) = get_d1zs(mean,weights)
        
    }


    checkBounds<-function(w){
    }

    stepSize<-function(w,d1Z){
    }

    get_d1Zs(mean,w){
        d1Z = rep(0,columns)
        m = NULL
        x = Return.portfolio(x,weights)
        m[1] = get_Moments(x,1)
        for(i in 1:4){
            m = c(m,get_Moments(x,i+1,m[0])) 
        }
        stats = get_Stats(m)
        c(meanSR,sigmaSR) = get_SR(stats,n)
        for(i in 1:columns){
            d1Z[i] = get_d1Z(stats,m,meanSR,sigmaSR,mean,weights,index)
        }
    }

    get_d1Z<-function(stats,m,meanSR,sigmaSR,mean,w,index){
        d1Mu = get_d1Mu(mean,index)
        


    }

    get_d1Mu<-function(mean,index){
    }

    get_d1Sigma<-function(sigma,mean,w,index){
    }

    get_d1Skew<-function(d1Sigma,sigma,mean,w,index,m3){
    }

    get_d1Kurt<-function(d1Sigma,sigma,mean,w,index,m4){
    }

    get_dnMoments<-function(mean,w,mOrder,dOrder,index){
    }

    get_SR<-function(stats,n){
        meanSR = stats[0]/stats[1]
        sigmaSR = ((1-meanSR*stats[2]+(meanSR^2)*(stats[3]-1)/4)/(n-1))^0.5
        return(meanSR,sigmaSR)
    }

    get_Stats<-function(m){
        stats = c(m[0],m[1]^(0.5),(m[2]/m[1])^(3/2),(m[3]/m[1])^(0.5))
        return(stats)
    }

    get_Moments<-function(x,order,mean = 0){

        sum = 0 
        for(i in 1:n){
        sum = sum + (x[i]-mean)^order
        }
        moment = sum/n
        return(moment)
    }

}




