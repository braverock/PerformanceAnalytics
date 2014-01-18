#' @title Summary of Expected Drawdown using Brownian Motion Assumptions and Return-Volatility 
#' 
#' @description  Works on the model specified by Maddon-Ismail which investigates the behavior of this statistic for a Brownian motion 
#' with drift.
#' @details If X(t) is a random process on [0, T ], the maximum drawdown at time T , D(T), is defined by
#' where \deqn{D(T) = sup [X(s) - X(t)]} where s belongs to [0,t] and s belongs to [0,T]
#'Informally, this is the largest drop from a peak to a bottom. In this paper, we investigate the
#'behavior of this statistic for a Brownian motion with drift. In particular, we give an infinite 
#'series representation of its distribution, and consider its expected value. When the drift is zero,
#'we give an analytic expression for the expected value, and for non-zero drift, we give an infinite
#'series representation. For all cases, we compute the limiting \bold{(\eqn{T tends to \infty})} behavior, which can be
#'logarithmic (\eqn{\mu} > 0), square root (\eqn{\mu} = 0), or linear (\eqn{\mu} < 0).
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of  asset returns
#' @param digits significant number
#' @author Shubhankit Mohan
#' @keywords Expected Drawdown Using Brownian Motion Assumptions
#' @references Magdon-Ismail, M., Atiya, A., Pratap, A., and Yaser S. Abu-Mostafa: On the Maximum Drawdown of a Browninan Motion, Journal of Applied Probability 41, pp. 147-161, 2004 \url{http://alumnus.caltech.edu/~amir/drawdown-jrnl.pdf}
#' @keywords Drawdown models Brownian Motion Assumptions
#' @examples
#' 
#'library(PerformanceAnalytics)
#' data(edhec)
#' table.EMaxDDGBM(edhec)
#' @rdname table.EMaxDDGBM
#' @export 
table.EMaxDDGBM <-
  function (R,digits =4)
  {# @author 
    
    # DESCRIPTION:
    # Downside Risk Summary: Statistics and Stylized Facts
    
    # Inputs:
    # R: a regular timeseries of returns (rather than prices)
    # Output: Table of Estimated Drawdowns 
    
    y = checkData(R, method = "xts")
    columns = ncol(y)
    rows = nrow(y)
    columnnames = colnames(y)
    rownames = rownames(y)
    T= nyears(y);
    
    # for each column, do the following:
    for(column in 1:columns) {
      x = y[,column]
      mu = Return.annualized(x, scale = NA, geometric = TRUE)
      sig=StdDev(x)
      gamma<-sqrt(pi/8)
      
      if(mu==0){
        
        Ed<-2*gamma*sig*sqrt(T)
        
      }
      
      else{
        
        alpha<-mu*sqrt(T/(2*sig^2))
        
        x<-alpha^2
        
        if(mu>0){
          
          mQp<-matrix(c(
            
            0.0005, 0.0010, 0.0015, 0.0020, 0.0025, 0.0050, 0.0075, 0.0100, 0.0125,
            
            0.0150, 0.0175, 0.0200, 0.0225, 0.0250, 0.0275, 0.0300, 0.0325, 0.0350,
            
            0.0375, 0.0400, 0.0425, 0.0450, 0.0500, 0.0600, 0.0700, 0.0800, 0.0900,
            
            0.1000, 0.2000, 0.3000, 0.4000, 0.5000, 1.5000, 2.5000, 3.5000, 4.5000,
            
            10, 20, 30, 40, 50, 150, 250, 350, 450, 1000, 2000, 3000, 4000, 5000, 0.019690,
            
            0.027694, 0.033789, 0.038896, 0.043372, 0.060721, 0.073808, 0.084693, 0.094171,
            
            0.102651, 0.110375, 0.117503, 0.124142, 0.130374, 0.136259, 0.141842, 0.147162,
            
            0.152249, 0.157127, 0.161817, 0.166337, 0.170702, 0.179015, 0.194248, 0.207999,
            
            0.220581, 0.232212, 0.243050, 0.325071, 0.382016, 0.426452, 0.463159, 0.668992,
            
            0.775976, 0.849298, 0.905305, 1.088998, 1.253794, 1.351794, 1.421860, 1.476457,
            
            1.747485, 1.874323, 1.958037, 2.020630, 2.219765, 2.392826, 2.494109, 2.565985,
            
            2.621743),ncol=2)
          
          
          
          if(x<0.0005){
            
            Qp<-gamma*sqrt(2*x)
            
          }
          
          if(x>0.0005 & x<5000){
            
            Qp<-spline(log(mQp[,1]),mQp[,2],n=1,xmin=log(x),xmax=log(x))$y
            
          }
          
          if(x>5000){
            
            Qp<-0.25*log(x)+0.49088
            
          }
          
          Ed<-(2*sig^2/mu)*Qp
          
        }
        
        if(mu<0){
          
          mQn<-matrix(c(
            
            0.0005, 0.0010, 0.0015, 0.0020, 0.0025, 0.0050, 0.0075, 0.0100, 0.0125, 0.0150,
            
            0.0175, 0.0200, 0.0225, 0.0250, 0.0275, 0.0300, 0.0325, 0.0350, 0.0375, 0.0400,
            
            0.0425, 0.0450, 0.0475, 0.0500, 0.0550, 0.0600, 0.0650, 0.0700, 0.0750, 0.0800,
            
            0.0850, 0.0900, 0.0950, 0.1000, 0.1500, 0.2000, 0.2500, 0.3000, 0.3500, 0.4000,
            
            0.5000, 1.0000, 1.5000, 2.0000, 2.5000, 3.0000, 3.5000, 4.0000, 4.5000, 5.0000,
            
            0.019965, 0.028394, 0.034874, 0.040369, 0.045256, 0.064633, 0.079746, 0.092708,
            
            0.104259, 0.114814, 0.124608, 0.133772, 0.142429, 0.150739, 0.158565, 0.166229,
            
            0.173756, 0.180793, 0.187739, 0.194489, 0.201094, 0.207572, 0.213877, 0.220056,
            
            0.231797, 0.243374, 0.254585, 0.265472, 0.276070, 0.286406, 0.296507, 0.306393,
            
            0.316066, 0.325586, 0.413136, 0.491599, 0.564333, 0.633007, 0.698849, 0.762455,
            
            0.884593, 1.445520, 1.970740, 2.483960, 2.990940, 3.492520, 3.995190, 4.492380,
            
            4.990430, 5.498820),ncol=2)
          
          
          
          
          
          if(x<0.0005){
            
            Qn<-gamma*sqrt(2*x)
            
          }
          
          if(x>0.0005 & x<5000){
            
            Qn<-spline(mQn[,1],mQn[,2],n=1,xmin=x,xmax=x)$y
            
          }
          
          if(x>5000){
            
            Qn<-x+0.50
            
          }
          
          Ed<-(2*sig^2/mu)*(-Qn)
          
        }
        
      }
      
    #  return(Ed)
      
      z = c((mu*100),
            (sig*100),
            (Ed*100))
      znames = c(
        "Annual Returns in %",
        "Std Devetions in %",
        "Expected Drawdown in %"
      )
      if(column == 1) {
        resultingtable = data.frame(Value = z, row.names = znames)
      }
      else {
        nextcolumn = data.frame(Value = z, row.names = znames)
        resultingtable = cbind(resultingtable, nextcolumn)
      }
    }
    colnames(resultingtable) = columnnames
    ans = base::round(resultingtable, digits)
    ans
    
    
  }

###############################################################################
################################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2014 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: EmaxDDGBM.R 2271 2012-09-02 01:56:23Z braverock $
#
###############################################################################
