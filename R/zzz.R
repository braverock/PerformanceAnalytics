.onLoad <- function(lib, pkg)
{   
    # Startup Mesage and Desription:
    MSG <- if(getRversion() >= "2.5") packageStartupMessage else message
    dsc <- packageDescription(pkg)
    if(interactive() || getOption("verbose")) { 
        # not in test scripts
        MSG(paste("\nPackage ", pkg, " (",dsc$Version,") loaded.\n",
            dsc$Title, "\n", dsc$Copyright, " ", dsc$Author, ". License: ", dsc$License, "\n", dsc$URL,
            "\n", sep=""))
    }
}

even <- function (x) x%%2==0

odd  <- function (x) x%%2==1

mean.xts <- function(x,...) {
    if(is.vector(x) ||is.null(ncol(x))  || ncol(x)==1){
        x<-as.numeric(x)
        mean(x,...)
    } else apply(x,2,mean.xts,...)
} 
mean.matrix <- function(x,...) {apply(x,2,mean,...)} 

sd.xts <- function(x,na.rm=FALSE) {
    if(is.vector(x) || is.null(ncol(x)) || ncol(x)==1){
        x<-as.numeric(x)
        sd(x,na.rm=na.rm)
    } else apply(x,2,sd,na.rm=na.rm)
}
sd.matrix <- function(x,na.rm=FALSE) {apply(x,2,sd,na.rm=na.rm)}

rollapply.xts <- xts:::rollapply.xts
###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2012 Peter Carl and Brian G. Peterson
#
# This R package is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id$
#
###############################################################################