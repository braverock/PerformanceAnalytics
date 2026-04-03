library(PerformanceAnalytics)
library(xts)

test_that("UpDownRatios mathematically toggles between arithmetic and geometric compound capture ratios (fixes #59)", {
  skip_on_cran()
  
  dates <- as.Date("2020-01-01") + 0:4
  Ra <- xts(c(0.10, -0.05, 0.05, -0.10, 0.02), order.by=dates)
  Rb <- xts(c(0.05, 0.02, -0.01, -0.05, 0.05), order.by=dates)
  
  # Manual subsets
  UpRa <- Ra[Rb > 0]
  UpRb <- Rb[Rb > 0]
  DnRa <- Ra[Rb <= 0]
  DnRb <- Rb[Rb <= 0]
  
  # A. Legacy Arithmetic evaluation
  man_arith_up <- sum(UpRa) / sum(UpRb)
  res_arith_up <- UpDownRatios(Ra, Rb, side="Up", method="Capture", geometric=FALSE)
  expect_equal(as.numeric(res_arith_up), man_arith_up)
  
  man_arith_dn <- sum(DnRa) / sum(DnRb)
  res_arith_dn <- UpDownRatios(Ra, Rb, side="Down", method="Capture", geometric=FALSE)
  expect_equal(as.numeric(res_arith_dn), man_arith_dn)
  
  # B. New Geometric compounding evaluation
  man_geom_up <- Return.cumulative(UpRa) / Return.cumulative(UpRb)
  res_geom_up <- UpDownRatios(Ra, Rb, side="Up", method="Capture", geometric=TRUE)
  expect_equal(as.numeric(res_geom_up), as.numeric(man_geom_up))
  
  man_geom_dn <- Return.cumulative(DnRa) / Return.cumulative(DnRb)
  res_geom_dn <- UpDownRatios(Ra, Rb, side="Down", method="Capture", geometric=TRUE)
  expect_equal(as.numeric(res_geom_dn), as.numeric(man_geom_dn))
  
  # Assert they are mathematically divergent 
  expect_false(isTRUE(all.equal(as.numeric(res_arith_up), as.numeric(res_geom_up))))
})

test_that("table.CaptureRatios and table.UpDownRatios correctly evaluate the geometric parameter", {
  skip_on_cran()
  
  dates <- as.Date("2020-01-01") + 0:4
  Ra <- xts(matrix(c(0.10, -0.05, 0.05, -0.10, 0.02), ncol=1), order.by=dates)
  Rb <- xts(matrix(c(0.05, 0.02, -0.01, -0.05, 0.05), ncol=1), order.by=dates)
  colnames(Ra) <- "AssetA"
  colnames(Rb) <- "BenchmarkB"
  
  # A. table.CaptureRatios
  res_arith_cap <- table.CaptureRatios(Ra, Rb, geometric=FALSE)
  res_geom_cap <- table.CaptureRatios(Ra, Rb, geometric=TRUE)
  
  expect_false(isTRUE(all.equal(as.numeric(res_arith_cap[1, ]), as.numeric(res_geom_cap[1, ]))))
  
  # Base calculation verifies the pass-through exactly matches raw UpDownRatios outputs
  expect_equal(as.numeric(res_arith_cap[1, "Up Capture"]), 
               as.numeric(round(UpDownRatios(Ra, Rb, side="Up", method="Capture", geometric=FALSE), 4)))
  
  expect_equal(as.numeric(res_geom_cap[1, "Up Capture"]), 
               as.numeric(round(UpDownRatios(Ra, Rb, side="Up", method="Capture", geometric=TRUE), 4)))
               
  # B. table.UpDownRatios
  res_arith_updn <- table.UpDownRatios(Ra, Rb, geometric=FALSE)
  res_geom_updn <- table.UpDownRatios(Ra, Rb, geometric=TRUE)
  
  # The first two columns are Up Capture and Down Capture
  expect_false(isTRUE(all.equal(as.numeric(res_arith_updn[1, 1:2]), as.numeric(res_geom_updn[1, 1:2]))))
  
  # Verify pass-through logic 
  expect_equal(as.numeric(res_arith_updn[1, "Up Capture"]), 
               as.numeric(round(UpDownRatios(Ra, Rb, side="Up", method="Capture", geometric=FALSE), 4)))
               
  expect_equal(as.numeric(res_geom_updn[1, "Up Capture"]), 
               as.numeric(round(UpDownRatios(Ra, Rb, side="Up", method="Capture", geometric=TRUE), 4)))
})
