#' Convert coredata content from one type of return to another
#' 
#' This function takes an xts object, and using its attribute information, will convert 
#' information in the object into the desired output, selected by the user.  For example, all 
#' combinations of moving from one of 'discrete', 'log', 'difference' and 'level', to another different
#' data type (from the same list) are permissible.
#' 
#'  
#' @param R an xts object
#' @param destinationType one of 'discrete', 'log', 'difference' or 'level'
#' @param seedValue a numeric scalar indicating the (usually initial) index level or price of the series
#' @param initial (default TRUE) a TRUE/FALSE flag associated with 'seedValue', indicating if this value is at the begginning of the series (TRUE) or at the end of the series (FALSE)
#' @author Erol Biceroglu
#' @seealso \code{\link{Return.calculate}}
###keywords ts multivariate distribution models
#' @examples
#' # TBD
#' 
Return.convert <- function(R
                           , destinationType = c("discrete","log","difference", "level")
                           , seedValue = NULL, initial = TRUE
                           ){
  
  if(is.null(seedValue)){seedValuePassed <- FALSE}else{seedValuePassed <- TRUE}
  seedValue <- checkSeedValue(R = R, seedValue = seedValue)
  
  #Error-check here for multiple types, matching the options presented in Return.calculate()
  if(length(destinationType)>1){stop("Must select a single destinationType, one of 'discrete','log','difference' or 'level'")}
  #End Error-check
  
#Declare warnings and error messages (these can show up multiple times)
  sameTypeWarning <- "destinationType is same as coredata_content"
  usingLevelCalculateWarning <- "Using Level.calculate() with default seedValue = 1 and initial = TRUE.  Use function directly to change default values."
  unknownDestTypeError <- "Unknown destinationType"
  unknownCoreDataError <- "Unknown, unsupported or missing coredata_content attribute"
  noSeedValueWarning <- "No seedValue detected for type 'difference', user should validate that results are accurate"
  
  #Now convert the returns, using nested-switch statements  
  switch(attributes(R)$coredata_content
         , discreteReturn = { #done
                                switch(destinationType
                                       , discrete = {warning(sameTypeWarning); return(R);}
                                       , log = {result <- log(1 + R);  attributes(result)$coredata_content <- "logReturn";}
                                       , difference = {result <- R*xts::lag.xts(Level.calculate(R = R, seedValue = seedValue, initial = initial)); attributes(result)$coredata_content <- "difference";if(!seedValuePassed){warning(noSeedValueWarning)};}
                                       , level = {warning(usingLevelCalculateWarning); result <- Level.calculate(R = R, seedValue = seedValue, initial = initial);}
                                       , {stop(unknownDestTypeError)}
                                      )
                            }
         , logReturn = { #done
                                switch(destinationType
                                      , discrete = {result <- exp(R) - 1;  attributes(result)$coredata_content <- "discreteReturn";}
                                      , log = {warning(sameTypeWarning); return(R);}
                                      , difference = {result <- (exp(R)-1)*xts::lag.xts(Level.calculate(R = R, seedValue = seedValue, initial = initial)); attributes(result)$coredata_content <- "difference";if(!seedValuePassed){warning(noSeedValueWarning)};}
                                      , level = {warning(usingLevelCalculateWarning); result <- Level.calculate(R = R, seedValue = seedValue, initial = initial);}
                                      , {stop(unknownDestTypeError)}
                                      )
                        }
         , difference = {#done
                                switch(destinationType
                                      , discrete = {result <- R/xts::lag.xts(Level.calculate(R = R, seedValue = seedValue, initial = initial));  attributes(result)$coredata_content <- "discreteReturn";}
                                      , log = {result <- log(R/xts::lag.xts(Level.calculate(R = R, seedValue = seedValue, initial = initial)) + 1); attributes(result)$coredata_content <- "logReturn";}
                                      , difference = {warning(sameTypeWarning); return(R);}
                                      , level = {warning(usingLevelCalculateWarning); result <- Level.calculate(R = R, seedValue = seedValue, initial = initial);}
                                      , {stop(unknownDestTypeError)}
                                      )
                          }
         , level = {
                               switch(destinationType
                                    , discrete = {result <- Return.calculate(prices = R, method = "discrete")}
                                    , log = {result <- Return.calculate(prices = R, method = "log")}
                                    , difference = {result <- Return.calculate(prices = R, method = "difference");if(!seedValuePassed){warning(noSeedValueWarning)};}
                                    , level = {warning(sameTypeWarning); return(R);}
                                    , {stop(unknownDestTypeError)}
                                      )
           
         }
         , {stop(unknownCoreDataError)}
  )
  
  return(result)
  
}