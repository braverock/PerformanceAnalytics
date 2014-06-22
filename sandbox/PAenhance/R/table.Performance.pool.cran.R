#' Print metrics from R CRAN PerformanceAnalytics that compatible with table.Performance
#' 
#' @details use \code{table.Performance.pool} to check available metrics. recoded SharpeRatio 
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @seealso \code{\link{table.Performance}}, \code{\link{table.Performance.pool}}
#' @keywords table metrics performance measure
#' @examples
#' table.Performance.pool.cran()
#' @export
table.Performance.pool.cran <-
function(...){
	c(		"AdjustedSharpeRatio", 
			"AverageDrawdown", 
			"BernardoLedoitRatio", 
			"BurkeRatio", 
			"CalmarRatio", 
			"CVaR", 
			"DownsideDeviation", 
			"DownsideFrequency", 
			"DownsidePotential", 
			"DRatio", 
			"DrawdownDeviation", 
			"ES", 
			"ETL", 
			"Frequency", 
#			"KellyRatio", problem occurs due to only one method in KellyRatio, will fix
			"kurtosis", 
			"MartinRatio", 
			"maxDrawdown", 
			"mean.geometric", 
			"mean.LCL", 
			"mean.stderr", 
			"mean.UCL", 
			"MeanAbsoluteDeviation", 
			"Omega", 
			"OmegaSharpeRatio", 
			"PainIndex", 
			"PainRatio", 
			"Return.annualized", 
			"Return.cumulative", 
			"sd.annualized", 
			"sd.multiperiod", 
			"SemiDeviation", 
			"SemiVariance", 
			"SharpeRatio",
			"SharpeRatio.annualized", 
			"skewness", 
			"SkewnessKurtosisRatio", 
			"SmoothingIndex", 
			"SortinoRatio", 
			"StdDev", 
			"StdDev.annualized", 
			"SterlingRatio", 
			"UPR", 
			"UpsideFrequency", 
			"UpsidePotentialRatio", 
			"UpsideRisk", 
			"VaR", 
			"VolatilitySkewness")
	
}
