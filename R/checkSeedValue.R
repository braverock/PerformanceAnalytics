#' Check 'seedValue' to ensure it is compatible with coredata_content attribute of 'R' (an xts object)
#' 
#' @param R an xts object
#' @param seedValue a numeric scalar indicating the (usually initial) index level or price of the series
#' @author Erol Biceroglu
#' 

checkSeedValue <- function(R, seedValue){
  #Begin ERROR CHECKING
  #if user somehow converts to zoo then runs checkData, it'll lose the coredata_content
  if(length(attributes(R)$coredata_content) == 0){stop("object is missing coredata_content attribute")}
  
  #Error check initial value before starting, so if no seedValue passed, set to 1 if it's not 'difference'
  if(is.null(seedValue) & (attributes(R)$coredata_content == "difference")){stop("When calculating levels using 'difference'(s), a seedValue must be provided")
  } else{
    if(is.null(seedValue) & !(attributes(R)$coredata_content == "difference")){seedValue <- 1}
  }
  
  #Now, if userpassed a value, make sure to default to first element
  #if(seedValue != 1){
  if(!is.null(seedValue)){
    if(length(seedValue) > 1){
      seedValue <- seedValue[1]
      warning("seedValue length greater than 1, using first element only")
      
    }
    if(!is.numeric(seedValue)){
      stop("Require a numeric value for seedValue")
    }
  }
  
  #if passed an xts object, convert to scalar
  if(inherits(seedValue, "xts")){seedValue <- as.vector(zoo::coredata(seedValue))}
  
  return(seedValue)
  
}