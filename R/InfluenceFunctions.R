#### Collection of IF function computations

#' Influence Function of Mean
#'
#' @param data Vector of the data
#'
#' @return IF of Mean
#' @export
#' @author Xin Chen
#'
#' @examples
#' mu.IF(rnorm(10))
mu.IF=function(data){
  mu.hat=mean(data)
  return(data-mu.hat)
}

#' Influence Function of Standard Deviation
#'
#' @param data Vector of the data
#'
#' @return IF of SD
#' @export
#'
#' @examples
#' SD.IF(rnorm(10))
SD.IF=function(data){
  mu.hat=mean(data)
  sd.hat=sd(data)
  return((data-mu.hat)^2-sd.hat^2)
}

#' Compute the influence function of value-at-risk
#'
#' @param data The vector of data
#' @param alpha The tail probability
#'
#' @return Influence Function of VaR
#' @export
#'
#' @examples
#' VaR.IF(rnorm(10))
VaR.IF=function(data,alpha=0.1){
  pdf.fit <- approxfun(density(data))
  qa=quantile(data,alpha)
  tmp=((data<=qa)-alpha)/pdf.fit(qa)
  return(tmp)
}

#' Influence Function of Expected Shortfall (ES)
#'
#' @param data Vector of the data
#' @param alpha Tail Probability
#'
#' @return IF of ES
#' @export
#'
#' @examples
#' ES.IF(rnorm(10))
ES.IF=function(data,alpha=0.1){
  pdf.fit <- approxfun(density(data))
  qa=quantile(data,alpha)
  ESa=-mean(data[data<=qa])
  tmp=1/alpha*((-data+qa)*(data<=qa))-qa-ESa
  return(tmp)
}

#' Compute the Influence Function value of Sharpe Ratio
#'
#' @param data vector of data
#' @param rf risk free interest rate
#'
#' @return vector of influence function
#' @export
#'
#' @examples
#' SR.IF(rnorm(10))
SR.IF=function(data,rf=0){
  mu.hat=mean(data)
  sd.hat=sd(data)
  IF=1/sd.hat*(data-mu.hat)-1/2*mu.hat/sd.hat^3*((data-mu.hat)^2-sd.hat^2)
  return(IF)
}


#' Influence Function of Sortino Ratio with Mean Return Threshold
#'
#' @param data Vector of data
#' @param rf Risk-Free interest rate
#'
#' @return IF of SoR
#' @export
#'
#' @examples
#' SoR.IF(rnorm(10))
SoR.IF=function(data,rf=0){
  mu.hat=mean(data)
  sigma.hat=sqrt(mean((data-mu.hat)^2))
  sigma.minus.hat=sqrt(mean((data-mu.hat)^2*(data<=mu.hat)))
  SoR.hat=(mu.hat-rf)/sigma.minus.hat
  mu1.minus.hat=mean((data-mu.hat)*(data<=mu.hat))
  tmp=-SoR.hat/2/sigma.minus.hat^2*(data-mu.hat-rf)^2*(data<=mu.hat)+
    (1/sigma.minus.hat+SoR.hat*mu1.minus.hat/sigma.minus.hat^2)*(data-mu.hat-rf)+
    SoR.hat/2
  return(tmp)
}

#' Influence Function of STARR Ratio
#'
#' @param data Vector of data
#' @param alpha Tail Probability
#' @param rf risk free interest rate
#'
#' @return IF of STARR
#' @export
#'
#' @examples
#' STARR.IF(rnorm(10))
STARR.IF=function(data,alpha=0.1,rf=0){
  mu.hat=mean(data)
  sigma.hat=mean((data-mu.hat)^2)
  VaR.hat=-quantile(data,alpha)
  qa=-VaR.hat
  ES.hat=-mean(data[data<=-VaR.hat])
  STARR.hat=(mu.hat-rf)/ES.hat
  tmp=(data-mu.hat-rf)/ES.hat-STARR.hat/ES.hat*(1/alpha*((-data-VaR.hat)*(data<=qa))+VaR.hat-ES.hat)
  return(tmp)
}






