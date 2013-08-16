chart.SharpeEfficientFrontier<-function(R){

    x = checkData(R)
    columns = ncol(x)

    mat<-NULL
    subset_sum<-function(numbers,target,partial){
        s = sum(partial)
        print(s)
        if(s==target){
            mat = rbind(mat,partial)
        }
      
        x<-NULL
        for(i in 1:length(numbers)){
            n = numbers[i]
            remaining = numbers[(i+1):length(numbers)]
            subset_sum(remaining,target,c(partial,n))
        }
    }
    subset_sum(c(1:10),10,0)
}
    
