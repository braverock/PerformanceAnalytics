Return.clean = function(R, alpha=.01 , trim=1e-3)
{# @author Kris Boudt, Brian Peterson

   # Function used to bound the effect of the most extreme returns on the downside
   # risk prediction.

   R=checkData(R,method="matrix")

   T=dim(R)[1]; date=c(1:T)
   N=dim(R)[2];
   MCD = covMcd(R,alpha=1-alpha)
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
