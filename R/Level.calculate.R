#' Calculate appropriate compounded return using xts attribute information
#' 
#' This is a useful function for calculating cumulative return over a period of
#' time, say a calendar year.  Can produce simple or geometric return.
#' 
#' product of all the individual period returns
#' 
#' \deqn{(1+r_{1})(1+r_{2})(1+r_{3})\ldots(1+r_{n})-1=prod(1+R)-1}{prod(1+R)-1}
#' 
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param geometric utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE) to aggregate returns,
#' default TRUE
#' @author Peter Carl
#' @seealso \code{\link{Return.annualized}}
#' @references Bacon, Carl. \emph{Practical Portfolio Performance Measurement
#' and Attribution}. Wiley. 2004. p. 6
###keywords ts multivariate distribution models
#' @examples
#' 
#' data(managers)
#' Return.cumulative(managers[,1,drop=FALSE])
#' Return.cumulative(managers[,1:8])
#' Return.cumulative(managers[,1:8],geometric=FALSE)
#' 
#' @export
Level.calculate <-
  function(R, seedValue = 1, initial = TRUE)  #maybe value = 1, initial = TRUE #???
  { # @author Erol Biceroglu

    
#Begin ERROR CHECKING
  #Error check initial value before starting
    if(seedValue != 1){
      if(length(seedValue) > 1){
        seedValue <- seedValue[1]
        warning("seedValue length greater than 1, using first element only")
      }
      if(!is.numeric(seedValue)){
        stop("Require a numeric value for seedValue")
      }
    }
    
    #if user somehow converts to zoo then runs checkData, it'll lose the coredata_content
    if(length(attributes(R)$coredata_content) == 0){stop("object is missing coredata_content attribute")}
    
    #clean up NAs
    if(is.na(R[1])){firstNAflag <- TRUE}else{firstNAflag <- FALSE} #may be used later if initial=FALSE
    R <- zoo::na.fill(object = R,fill = 0)
#End ERROR CHECKING    
    
#calculate  result
if(is.xts(R)){
  if(attributes(R)$coredata_content %in% c("discreteReturn","logReturn")){
    if(initial){
      result <- 
        switch(attributes(R)$coredata_content
               , discreteReturn = cumprod(1+R)
               , logReturn = exp(cumsum(R))   #this is faster than cumprod(exp(R))
        )
    }else{
      step1 <-  switch(attributes(R)$coredata_content
                       , discreteReturn = (1+R)
                       , logReturn = exp(R)
      )
      step2 <- 1/step1
      step3 <- unclass(step2)
      step4a <- rev(cumprod(rev(step3)))  #need to reverse the order to reconstruct from last to first
      if(firstNAflag){step4b <- step4a[-1]}  #from NA cleanup
      step4 <- xts(x = c(step4b,1), order.by = index(step1))
      xts::xtsAttributes(step4) <- xts::xtsAttributes(step3)
      result <- step4
      colnames(result) <- colnames(R)
      rm(step1,step2,step3,step4,step4a,step4b)
    }

  } else {
    stop("Unknown coredata_content attribute, unable to calculate")
  }
} else {
  stop("Must pass an xts object")
}

    
result <- result * seedValue

    #set attributes
    attributes(result)$coredata_content <- "level"
    attributes(result)$ret_type <- NULL
    
    return(result)
}
