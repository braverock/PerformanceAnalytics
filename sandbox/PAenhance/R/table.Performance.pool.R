
#' Print metrics from R-forge PerformanceAnalytics that compatible with table.Performance
#' @param NULL
#' @details use \code{table.Performance.pool} to check available metrics. recoded SharpeRatio 
#' @author Kirk Li  \email{kirkli@@stat.washington.edu} 
#' @seealso \code{\link{table.Performance}}, \code{\link{table.Performance.pool.cran}}
#' @keywords table metrics performance measure
#' @examples
#' table.Performance.pool()
#' @export
table.Performance.pool <- function(...){
	c(
			"AdjustedSharpeRatio", 
			"AverageDrawdown", 
			"AverageRecovery", 
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
			"HurstIndex", 
#			"KellyRatio", something is wrong with this metric, will fix later
			"kurtosis", 
			"lpm", 
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
			"UlcerIndex", 
			"UpsideFrequency", 
			"UpsidePotentialRatio", 
			"UpsideRisk", 
			"VaR", 
			"VolatilitySkewness")
}
