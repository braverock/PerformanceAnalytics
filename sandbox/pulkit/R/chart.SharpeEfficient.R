chart.SharpeEfficientFrontier<-function(R){

    x = checkData(R)
    x = na.omit(x)
    columns = ncol(x)
    weights<-matrix(ncol=ncol(x))
    for(i in 1:20000){
       weights<-rbind(weights,sample(1:ncol(x))/ncol(x))
    }
    y_axis<-NULL
    x_axis<-NULL
    print(weights)
    for(i in 1:20000){
        x_portfolio = Return.portfolio(x,weights[i,])
        print(x_portfolio)
        sr<-SharpeRatio(x_portfolio,FUN="StdDev")
        sk<-skewness(x_portfolio)
        kr<-kurtosis(x_portfolio)
        sd<-StdDev(x_portfolio)
        sigma_sr<-((1-sk*sr+(sr^2)*(kr-1)/4)/(length(x_portfolio)-1))^0.5
        y_axis<-c(y_axis,as.vector(sr))
        x_axis<-c(x_axis,as.vector(sigma_sr))
    }
    plot(x_axis,y_axis)

}
    
