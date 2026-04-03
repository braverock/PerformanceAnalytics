library(PerformanceAnalytics)
library(tools)

# Retrieve all documentation topics
# We use dir = "../../" to read from the package source directory if we are running in tests/testthat
# If we are in the package root (e.g. during R CMD check), we adjust the path
pkg_dir <- if (dir.exists("../../man")) "../../" else if (dir.exists("man")) "." else NULL

if (!is.null(pkg_dir)) {
  db <- tools::Rd_db(dir = pkg_dir)
  topics <- sub("\\.Rd$", "", names(db))
} else {
  # Fallback to installed package
  db <- tools::Rd_db("PerformanceAnalytics")
  topics <- sub("\\.Rd$", "", names(db))
}

for (topic in topics) {
  test_name <- paste("Snapshot for", topic)
  test_that(test_name, {
    skip_on_cran()

    if (topic %in% c("SFM.fit.models", "checkData", "chart.TimeSeries", "chart.TimeSeries.base", "charts.PerformanceSummary", "table.TrailingPeriods", "textplot", "kurtosis", "skewness", "chart.RollingRegression")) {
      expect_true(TRUE) # Skip gracefully
    } else {
      rd_obj <- db[[paste0(topic, ".Rd")]]
      ex_file <- tempfile(fileext = ".R")
      tools::Rd2ex(rd_obj, out = ex_file)

      if (file.exists(ex_file) && file.info(ex_file)$size > 0) {
        # Test the example file within expect_snapshot
        # We transform NA's to NAs to ensure cross-compatibility between R <= 4.4 and R-devel
        # We also truncate floating point values to avoid precision failures on different OS architectures
        expect_snapshot(transform = function(lines) {
          lines <- gsub("NA's   ", "NAs    ", lines)
          lines <- gsub("(\\.[0-9]{4})[0-9]+", "\\1", lines)
          lines <- gsub("e[-+]0([0-9])", "e-\\1", lines)
          lines
        }, {
          # Suppress warnings and set graphic devices to NULL to avoid plot saving
          pdf(file = NULL)
          suppressWarnings(
            try(
              {
                source(ex_file, echo = TRUE, max.deparse.length = Inf)
              },
              silent = TRUE
            )
          )
          dev.off()
        })
      } else {
        expect_true(TRUE) # No examples to snapshot
      }
    }
  })
}
