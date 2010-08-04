
dbenford <- function(d, n=1, base=10) {
    if(n<=1){
        result = log(1+(1/d),base=base)
    }
    else {
        k = seq(from=10^(n-2), to=(10^(n-1)-1))
        result=vector(length=length(d))
        for(i in 1:length(d)) result[i] = sum(log(1+1/(10*k+d[i]),base=base))
    }
    return(result)
}

# > dbenford(1:9)
# [1] 0.30103000 0.17609126 0.12493874 0.09691001 0.07918125 0.06694679 0.05799195
# [8] 0.05115252 0.04575749

which.digit <- function(x, n=1) {
    as.numeric(strsplit(as.character(x),"")[[1]][n])
}

# > sapply(as.vector(abs(edhec[,1])*100),which.digit)
#   [1] 1 1 0 0 1 2 1 1 1 1 0 0 1 1 1 1 0 0 0 3 1 2 2 1 2 0 1 2 1 1 1 0 0 0 1 1 2
#  [38] 2 2 2 1 1 0 1 1 0 0 0 3 1 1 1 0 0 0 1 0 1 0 0 1 0 0 0 0 0 1 0 1 1 2 1 2 1
#  [75] 0 1 1 0 0 0 1 1 0 0 1 0 0 0 1 1 0 0 0 0 0 0 0 0 1 3 1 1 1 0 1 0 0 0 2 1 1
# [112] 0 0 0 0 0 0 0 0 1 1 1 0 0 1 0 0 1 1 1 1 0 0 0 3 0 1 0 1 0 1 1 2 1 4 1 2 5
# [149] 5 2 6 3

# > cbind(table(sapply(as.vector(abs(edhec[,1])*100),which.digit)))
#   [,1]
# 0   66
# 1   62
# 2   15
# 3    5
# 4    1
# 5    2
# 6    1
# dig.count=cbind(table(sapply(as.vector(abs(which(edhec[,1]>0))*100),which.digit,n=1)))
#chisq.test(x=dig.count/sum(dig.count),y=sapply(1:9,dbenford, n=1))

table.Benford <- 
function(R=NULL, method=c("percentage", "count"), n=1, obs=100){
    # n: the nth significant digit to test
    if(!is.null(R))
        R = checkData(R)
    method=method[1]
    columns = NCOL(R)
    result=NA
    if(is.null(R)){
        # Calculate theoretical values
        for(sign.digit in n){
            digit.perc = cbind(dbenford(0:9,n=sign.digit))
            if(sign.digit==1) digit.perc[1] = NA
            if(sign.digit==n[1])
                result=digit.perc
            else
                result = cbind(result, digit.perc)
        }
        colnames(result)=n
        rownames(result)=0:9
        if(method=="count")
            result = round(result*obs,0)
            
    }
    else {
        # Calculate actual values
        for(column in 1:columns){ # Return a list of tables for each columns
            for(sign.digit in n){
                digit.count = cbind(table(sapply(as.vector(abs(R[which(R[,column]>0),column])*10000), which.digit, n=sign.digit)))
                fill = data.frame(rep(NA,10),row.names=0:9)
                tmp = merge(digit.count,fill,by=0,all=TRUE)
                count.col=as.data.frame(tmp[,2],row.names=tmp[,1])
                if(sign.digit==n[1]) 
                    result = count.col
                else
                    result = cbind(result, count.col)
            }
            colnames(result)=n
            if(NROW(result)==10)
                rownames(result)=0:9
            if(method=="percentage"){
                result[ is.na(result) ] <- 0
                result=t(t(result)/colSums(result))
            }
        }

    }
    return(result)
}

chart.Benford <-
function(R, n=1, p=.95, method=c("actual", "differences"), ...) {

    obs=NROW(R)
    # Calculate the theoretical line
    theo = table.Benford(method="percentage", n=n[1])
    # Calculate digit counts
    actual = table.Benford(R, method="percentage", n=n[1])
    # Calculate z-score lines
#     ztests = (actual-theo)/(sqrt(theo*(1-theo)/obs)
    zUpper =
    zLower = 



    plot(theo, type="b", axes=FALSE)
    # TODO add axes, labels
    return(invisible(result))
}
