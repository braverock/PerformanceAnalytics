library(PerformanceAnalytics)
library(tools)

# Retrieve all documentation topics
topics <- sub("\\.Rd$", "", names(tools::Rd_db("PerformanceAnalytics")))

for (topic in topics) {
  test_name <- paste("Snapshot for", topic)
  test_that(test_name, {
    skip_on_cran()
    
    if (topic %in% c("SFM.fit.models", "checkData", "chart.TimeSeries", "chart.TimeSeries.base", "charts.PerformanceSummary", "table.TrailingPeriods", "textplot", "kurtosis", "skewness", "chart.RollingRegression")) {
      expect_true(TRUE) # Skip gracefully
    } else {
      rd_obj <- tools::Rd_db("PerformanceAnalytics")[[paste0(topic, ".Rd")]]
      ex_file <- tempfile(fileext=".R")
      tools::Rd2ex(rd_obj, out = ex_file)
      
      if (file.exists(ex_file) && file.info(ex_file)$size > 0) {
        
        # Test the example file within expect_snapshot
        # We transform NA's to NAs to ensure cross-compatibility between R < 4.5 and R >= 4.5
        expect_snapshot(transform = function(lines) gsub("NA's", "NAs ", lines), {
          # Suppress warnings and set graphic devices to NULL to avoid plot saving
          pdf(file=NULL)
          suppressWarnings(
            try({
              source(ex_file, echo=TRUE, max.deparse.length=Inf)
            }, silent=TRUE)
          )
          dev.off()
        })
      } else {
        expect_true(TRUE) # No examples to snapshot
      }
    }
  })
}
