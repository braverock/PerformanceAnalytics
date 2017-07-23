#' Calculate appropriate cumulative return series or asset level using xts attribute information
#' 
#' This function calculates the time varying index level over 
#' the entire period of available data.  It will work with arithmetic or log returns, using attribute information from an xts object.
#' If the first value in the left-most column is NA, it will be populated with the seedValue.  
#' However, if the first value in the left-most column is not NA, the previous date will be estimated
#' based on the periodicity of the time series, and be populated with the seedValue.
#' This is so that information is not lost in case levels are converted back to returns 
#' (where the first value would result in an NA).  Note: previous date does not consider weekdays or
#' holidays, it will simply calculate the previous calendar day.  
#' If the user has a preference, they should ensure that the first row has the appropriate time index with an NA value.  
#' If users run Return.calculate() from this package, this will be a non-issue.
#' 
#' Product of all the individual period returns
#' 
#' For arithmetic returns:
#' \deqn{(1+r_{1})(1+r_{2})(1+r_{3})\ldots(1+r_{n})=cumprod(1+R)}{cumprod(1+R)}
#' 
#' For log returns:
#' \deqn{exp(r_{1}+r_{2}+r_{3} + \ldots + r_{n})=exp(cumsum(R))}{exp(cumsum(R))}
#' 
#' @param R an xts object
#' @param seedValue a numeric scalar indicating the (usually initial) index level or price of the series
#' @param initial (default TRUE) a TRUE/FALSE flag associated with 'seedValue', indicating if this value is at the begginning of the series (TRUE) or at the end of the series (FALSE)
#' @author Erol Biceroglu
#' @seealso \code{\link{Return.calculate}}
#' 
#' @examples
#' 
#' data(managers)
#' managers <- managers[,1:6] #equities only
#' xtsAttributes(managers) <- list(coredata_content = "discreteReturn") #append attribute
#' chart.CumReturns(managers) #here is baseline
#' managersL <- Level.calculate(R = managers)
#' plot(managersL-1)
#' 
#' #Here they are equal
#' Return.cumulative(managers)  #Baseline
#' last(managersL-1) #This function
#' 
#' @export
Level.calculate <-
  function(R, seedValue = NULL, initial = TRUE)
  { # @author Erol Biceroglu

     
#Begin ERROR CHECKING
seedValue <- checkSeedValue(R = R, seedValue = seedValue)
        
  #clean up NAs
    #append estimated last date if first value not NA
    if(!is.na(R[1])){
      warning("Estimated start date/time based on periodicity of time series")
      estimatedPreviousDateStamp <- calculateImpliedDate(R)
      tempName <- colnames(R)
      tempR <- R
      R <- rbind(  xts(x = t(rep(0,ncol(R))), order.by = estimatedPreviousDateStamp)
                   , R
      )
      colnames(R) <- tempName
      R <- reclass(R,match.to = tempR)
      xtsAttributes(R)$coredata_content <- xtsAttributes(tempR)$coredata_content
      rm(tempR, tempName)
    }
    
    R <- zoo::na.fill(object = R,fill = 0)
#End ERROR CHECKING    

#calculate  result
if(is.xts(R)){
  if(attributes(R)$coredata_content %in% c("discreteReturn","logReturn", "difference")){
    if(initial){
      result <- 
        switch(attributes(R)$coredata_content
               , discreteReturn = cumprod(1+R)
               , logReturn = exp(cumsum(R))   #this is faster than cumprod(exp(R))
               , difference = cumsum(R) #+1
        )
    }else{
      #This could be refactored, but it's easier to follow like this
      step1 <-  switch(attributes(R)$coredata_content
                       , discreteReturn = (1+R)
                       , logReturn = exp(R)
                       , difference = exp(-R)  #rev(cumsum(rev(-R)))
      )
      
      #difference is handled differently
      if(attributes(R)$coredata_content != "difference"){
      step2 <- 1/step1
      } else{
        step2 <- step1
      }
      
      step3 <- unclass(step2)
      step4a <- rev(cumprod(rev(step3)))  #need to reverse the order to reconstruct from last to first
      
      #since we're using cumprod(), transform it back into cumsum()
      if(attributes(R)$coredata_content == "difference"){step4a <- log(step4a)} #+ 1}
      
        step4b <- step4a[-1]
        step4 <- xts(x = c(step4b,1), order.by = zoo::index(step1))

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

#Here, the seedValue is appended to get "prices" or index levels
#result <- result * seedValue
if(attributes(R)$coredata_content != "difference"){
  result <- result * seedValue
} else{
  result <- result + seedValue #(seedValue - 1)
}




    #set attributes
    attributes(result)$coredata_content <- "level"
    attributes(result)$ret_type <- NULL
    
    return(result)
}


#' @rdname Level.calculate
#'
isLeap <- function(yearNumber){
  #pulled formula from this link: https://support.microsoft.com/en-sg/help/214019/method-to-determine-whether-a-year-is-a-leap-year
  ifelse(
    ((yearNumber + 1900) %% 400 == 0) |
      ((yearNumber + 1900) %% 4 == 0) & ((yearNumber + 1900) %% 100 != 0)
    ,TRUE
    ,FALSE
  )
}

#' @rdname Level.calculate
#'
calculateImpliedDate <- 
  function(tsObject){

    switch( xts::periodicity(tsObject)$scale
            , daily = {impliedDate <- zoo::index(tsObject)[1] - 1}
            , weekly = {impliedDate <- zoo::index(tsObject)[1] - 7}
            , monthly = {
              if(attributes(tsObject)$.indexCLASS == "yearmon"){
                
                impliedDate <- zoo::index(tsObject)[1] - 1/12 #subtract a month
                
              } else {
                #year
                impliedYear <-
                  xts::.indexyear(tsObject)[1] - ifelse(xts::.indexmon(tsObject)[1] == 0,1,0) + 1900
                
                #month
                impliedMonth <-
                  ifelse((xts::.indexmon(tsObject)[1] + 1) == 1, 12 ,(xts::.indexmon(tsObject)[1] + 1) - 1)
                
                #day
                if(impliedMonth %in% c(1,3,5,7,8,10,12) #JMMJAOD
                ){
                  impliedDay <- 31
                }
                if(impliedMonth %in% c(4,6,9,11) #AJSN
                ){
                  impliedDay <- 30
                }
                if(impliedMonth == 2 #F
                ){
                  impliedDay <- ifelse(isLeap(impliedYear),29,28)
                }
                
                impliedDate <- base::as.Date(paste(impliedYear,impliedMonth,impliedDay,sep = "-"))
                
              }
            }
            , quarterly = {impliedDate <- zoo::index(tsObject)[1] - 1/4}
            , yearly = {
              #year
              impliedYear <-
                xts::.indexyear(tsObject)[1] - 1 + 1900
              
              #month
              impliedMonth <- (xts::.indexmon(tsObject)[1] + 1)
              
              #day
              impliedDay <- xts::.indexmday(tsObject)[1]
              #from leap to non-leap
              if((isLeap(impliedYear + 1) == TRUE) & (isLeap(impliedYear) == FALSE)){
                impliedDay <- ifelse(impliedMonth == 2 & impliedDay == 29,28,impliedDay)
              }
              
              
              #from non-leap to leap
              if((isLeap(impliedYear + 1) == FALSE) & (isLeap(impliedYear) == TRUE)){
                impliedDay <- ifelse(impliedMonth == 2 & impliedDay == 28,29,impliedDay)
              }
              
              impliedDate <- base::as.Date(paste(impliedYear,impliedMonth,impliedDay,sep = "-"))
              
            }
            , stop("Unknown periodicity, unable to compute implied date")
            
    )
    
    return(impliedDate)
    
  }