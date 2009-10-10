`Return.clean` <-
function(R, method = c("none","boudt","geltner"), alpha=.01, ...)
{ # @author Peter Carl

    # DESCRIPTION:
    # A wrapper for selecting the method by which return data is 'cleaned'

    # Inputs:
    # R: a matrix, data frame, or timeSeries of returns

    # Outputs:
    # A timeseries of the 'cleaned' series

    # FUNCTION:
    method = method[1]

    # Transform input data to a timeseries (xts) object
    orig = R
    R = checkData(R, method="xts")

    #result.zoo = zoo(NA, order.by=time(R))

    # Get dimensions and labels
    columns = ncol(R)
    columnnames = colnames(R)

    for(column in 1:columns) { # for each asset passed in as R
        #R.clean = zoo(NA, order.by=time(R))

        switch(method,
            none = {
		R.clean = R[,column]
	    },
	    boudt = {
                R.clean = clean.boudt(na.omit(R[ , column, drop=FALSE]),alpha=alpha,...)[[1]]
            },
	    geltner = {
		R.clean = Return.Geltner(R[,column])
	    }
        )

        if(column == 1) {
            result = R.clean
        }
        else {
            result = cbind(result, R.clean)
        }
    }

    # RESULTS:
    result=reclass(result,match.to=orig)
    return(result)
}

`clean.boudt` <-
function(R, alpha=.01 , trim=1e-3)
{# @author Kris Boudt, Brian Peterson

   # set up by loading robustbase library
   stopifnot("package:robustbase" %in% search() || require("robustbase",quietly=TRUE))

   # Function used to bound the effect of the most extreme returns on the downside
   # risk prediction.

   R=checkData(R,method="zoo") # modified to create a zoo object in the cleaneddata slot of the list

   T=dim(R)[1]; date=c(1:T)
   N=dim(R)[2];
   MCD = covMcd(as.matrix(R),alpha=1-alpha)
   mu = as.matrix(MCD$raw.center) #no reweighting
   sigma = MCD$raw.cov
   invSigma = solve(sigma);
   vd2t = c();
   cleaneddata = R
   outlierdate = c()

   # 1. Sort the data in function of their extremeness
   # Extremeness is proxied by the robustly estimated squared Mahalanbobis distance

   for(t in c(1:T) )
   {
      d2t = as.matrix(R[t,]-mu)%*%invSigma%*%t(as.matrix(R[t,]-mu));
      vd2t = c(vd2t,d2t);
   }
   out = sort(vd2t,index.return=TRUE)
   sortvd2t = out$x;
   sortt = out$ix;

   # 2. Outlier detection
   # empricical 1-alpha quantile

   empirical.threshold = sortvd2t[floor((1-alpha)*T)];

   # 2.1. Only the alpha most extreme observations can be qualified as outliers

   T.alpha = floor(T * (1-alpha))+1
   # print(c("empirical quantile=",vd2t[sortt[T.alpha-1]],"chi-squared quantile",qchisq(1-trim,N)))

   cleanedt=sortt[c(T.alpha:T)]

   # 2.2. multivariate winsorization (Khan et al, 2007) :
   # bound the MD of the most exteme observations to a quantile of the chi squared distribution, N d
   for(t in cleanedt ){
        if(vd2t[t]>qchisq(1-trim,N)){
              # print(c("Observation",as.character(date[t]),"is detected as outlier and cleaned") );
               cleaneddata[t,] = sqrt( max(empirical.threshold,qchisq(1-trim,N))/vd2t[t])*R[t,];
               outlierdate = c(outlierdate,date[t]) } }

   return(list(cleaneddata,outlierdate))

}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2009 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: Return.clean.R,v 1.9 2009-10-10 12:40:08 brian Exp $
#
###############################################################################
# $Log: not supported by cvs2svn $
# Revision 1.8  2009-09-02 14:11:12  brian
# - add alpha and dots as passthru arguments to clean.boudt
# - TODO: vectorize and na.skip
#
# Revision 1.7  2009-09-02 12:23:39  brian
# - convert to use xts internally
# - add reclass
# - add 'none' and 'geltner' as methods
#
# Revision 1.6  2009-09-01 21:40:07  brian
# - change to use xts internally
#
# Revision 1.5  2008-08-13 18:05:22  brian
# - add copyright, licence, and CVS log
#
# Revision 1.4 2008-08-12 17:56:13 brian
# - add library check for package robustbase
#
# Revision 1.3 2008-08-11 08:58:42 peter
# - moved prior functionality into 'clean.boudt'
# - made Return.clean a wrapper focused on data handling, multiple methods
#
# Revision 1.2 2008-08-08 23:16:09 peter
# - separated out the cleaning function and added column handling
#
# Revision 1.1 2008-08-07 16:39:33 brian
# - initial revision of robust data cleaning function and documentation