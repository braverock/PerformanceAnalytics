#' @rdname CAPM.coefficients
#' @export SFM.coefficients CAPM.coefficients
CAPM.coefficients <- SFM.coefficients <- .coefficients <- 
function(xRa, xRb, subset, ..., method ="LS", family="mOpt"){
  # subset is assumed to be a logical vector
  if(missing(subset))
    subset <-TRUE
  # check columns
  if(NCOL(xRa)!= 1L || NCOL(xRb)!= 1L || NCOL(subset)!= 1L)
    stop("all arguments must have only one column")
  # merge, drop NA
  merged <- as.data.frame(na.omit(cbind(xRa, xRb, subset)))
  # return NA if no non-NA values
  if(NROW(merged)== 0)
    return (NA)
  # add column names and convert subset back to logical
  colnames(merged) <- c("xRa", "xRb", "subset")
  merged$subset <- as.logical(merged$subset)
  switch(method,
    LS = {
      model.lm = lm(xRa ~ xRb, data=merged, subset=subset)
      return (list(intercept=coef(model.lm)[[1]],
                   beta=coef(model.lm)[[2]],
                   model= model.lm))
      },
    mOpt = {
      switch(family,
        mOpt = {
          model.rob.lm = lmrobdetMM(xRa ~ xRb, data=merged, 
                                                subset=subset)
        }
      )
      return (list(intercept=coef(model.rob.lm)[[1]],
                   beta=coef(model.rob.lm)[[2]],
                   model= model.rob.lm))
      },
    Both = {
      model.rob.lm = lmrobdetMM(xRa ~ xRb, data=merged,
                                subset=subset)
      model.lm = lm(xRa ~ xRb, data=merged, subset=subset)
      return (list(robust=(list(intercept=coef(model.rob.lm)[[1]],
                                beta=coef(model.rob.lm)[[2]],
                                model=model.rob.lm
                                )
                           ),
                   ordinary=(list(intercept=coef(model.lm)[[1]],
                                  beta=coef(model.lm)[[2]],
                                  model=model.lm
                                  )
                             )
                   )
      )
      }
  )
}