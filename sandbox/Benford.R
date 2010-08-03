
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
dig.count=cbind(table(sapply(as.vector(abs(which(edhec[,1]>0))*100),which.digit,n=1)))
#chisq.test(x=dig.count/sum(dig.count),y=sapply(1:9,dbenford, n=1))