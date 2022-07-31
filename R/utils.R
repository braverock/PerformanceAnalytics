
excessReturns <- function(Ra, Rb, Rf){
  Ra = checkData(Ra)
  Rb = checkData(Rb)
  if(!is.null(dim(Rf)))
    Rf = checkData(Rf)
  
  xRa = Return.excess(Ra, Rf)
  xRb = Return.excess(Rb, Rf)
  return (list(xRa, xRb));
}

getResults <- function(xRa, xRb, Ra.ncols, Rb.ncols, ..., subset=TRUE){
  pairs = expand.grid(1:Ra.ncols, 1:Rb.ncols)
  result = apply(
    pairs, 1, 
    FUN = function(n, xRa, xRb, subset, ..., method, family){
      if (subset=="Bull"){
        subset <- (xRb[,n[2]]>0);
      }
      else if (subset=="Bear"){
        subset <- (xRb[,n[2]]<0);
      }
      else{
        subset <- TRUE;
      }
      SFM.coefficients(xRa[,n[1]], xRb[,n[2]], subset=subset, ..., method = method, family = family, Model = T)
    }, 
    xRa = xRa, xRb = xRb, subset=subset, ...
  )
  return (result)
}

processResults <- function(result.all, attribute, Ra.ncols, Rb.ncols, 
                           Ra.colnames, Rb.colnames, method, attribute.alias=NULL,
                           digits=3, benchmarkCols=T){
  if (method!="Both"){
    result = matrix(ncol=Ra.ncols, nrow=Rb.ncols);
  }
  else{
    result.LS  = matrix(ncol=Ra.ncols, nrow=Rb.ncols);
    result.Rob = matrix(ncol=Ra.ncols, nrow=Rb.ncols);
  }
  i = 1
  j = 1
  attribute.alias = ifelse(is.null(attribute.alias), attribute, attribute.alias);
  for (res in result.all) {
    if (method!="Both"){
      result[i,j] <- get(attribute,res)
    }
    else{
      result.LS[i,j] <- get(attribute, res$LS);
      result.Rob[i, j] <- get(attribute, res$robust);
    }
    
    j = j+1
    if (j>Ra.ncols){i=i+1; j=1}
  }
  if (method!="Both"){
    if(length(result) ==1)
      return(result[[1]])
    else {
      dim(result) = c(Rb.ncols, Ra.ncols)
      colnames(result) = Ra.colnames
      rownames(result) = paste(paste(attribute.alias,":"), Rb.colnames)
      if(benchmarkCols){
        result = t(result);
      }
      if (!is.null(digits)){
        result = round(result, digits);
      }
      return(result)
    }
  }
  else{
    dim(result.LS) = c(Rb.ncols, Ra.ncols)
    dim(result.Rob) = c(Rb.ncols, Ra.ncols)
    
    colnames(result.LS) <- Ra.colnames
    rownames(result.LS) = paste("[LS]", attribute.alias, ":", Rb.colnames)
    colnames(result.Rob) <- Ra.colnames
    rownames(result.Rob) = paste("[Rob]", attribute.alias, ":", Rb.colnames)
    
    if(benchmarkCols){
      result = cbind(t(result.LS), t(result.Rob))
    }
    else{
      result = rbind(result.LS, result.Rob)
    }
    if (!is.null(digits)){
      result = round(result, digits);
    }
    return(result)
  }
  
}
