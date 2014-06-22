#' Uncertainty measure of Variance Estimator
#' This function returns the standard error of the three estimator of Variance.
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param methods the estimation methods for Variance estimator, default is normal
#' @author Douglass Martin, Kirk Li 
#' @references TBA
#' @keywords variance estimation, bootstrap
#' @examples
#' data(edhec)
#' var.se(edhec[,1],methods="normal")
#' @export 
#' @rdname UncertaintyMeasure
var.se <- function(R,methods=c("normal","non-normal","bootstrap")){
	R <- checkData(R, method="xts")
	columns=colnames(R)
	
	methods <- match.arg(methods,c("normal","non-normal","bootstrap"))
	
	switch(methods, 
			"normal" = {		var.std.error = sapply(R, function(x) sqrt(2/(length(x)-1)* var(x,na.rm=TRUE)^2))
			},
			"non-normal" = {var.std.error = sapply(R, function(x) sqrt((length(x)/(length(x)-1))^2 * 
											(
												(mean((x-mean(x,na.rm=TRUE))^4,na.rm=TRUE)-var(x,na.rm=TRUE)^2)/length(x)
												
												-2*(mean((x-mean(x,na.rm=TRUE))^4)-2*var(x,na.rm=TRUE)^2)/length(x)^2
												
												+(mean((x-mean(x,na.rm=TRUE))^4,na.rm=TRUE)-3*var(x,na.rm=TRUE)^2)/length(x)^3
												)
							))},
			"bootstrap" = {	require("boot")
				
				boot.sd.var <- function(X,idx) var(X[idx],na.rm=TRUE)
				boot.sd <- function(X) 
				{
					boot.res = boot(X, statistic=boot.sd.var,  R=10*length(X))
					sd(as.vector(boot.res$t))
				}
				
				var.std.error = sapply(R,boot.sd)}
	)
	var.std.error
} 


