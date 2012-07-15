DrawdownPeak <- function (R, ...)
{
   R0 <- R
   R = checkData(R, method="matrix")

    if (ncol(R)==1 || is.null(R) || is.vector(R)) {
	calcul = FALSE
        for (i in (1:length(R))) {
     	     if (!is.na(R[i])) {
     	    	calcul = TRUE
	     }
        }		      
        if (!calcul) {
	result = NaN
	}
	else {
        R = na.omit(R)
	drawdownpeak = c()
	length(drawdownpeak) = length(R)
	peak = 0
	for(i in (1:length(R))) {
	      val = 1
	      borne = peak+1
	      for(j in (borne:i)) {
	      	    val = val*(1+R[j]/100)
	      }
	      if (val > 1) {
	      	 peak = i
		 drawdownpeak[i] = 0
	      }
	      else {
	      	   drawdownpeak[i] = (val-1)*100
	      }
	}
	result = drawdownpeak
	}
       	reclass(result, R0)	
	return(result)
}
    else {
        R = checkData(R)
        result = apply(R, MARGIN = 2, DrawdownPeak, ...)
        result<-t(result)
        colnames(result) = colnames(R)
        rownames(result) = paste("DrawdownPeak", sep="")
        return(result)
    }
}