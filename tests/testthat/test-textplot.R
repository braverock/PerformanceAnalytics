library(PerformanceAnalytics)

test_that("textplot variants work", {
  skip_on_cran()
  
  # Set up a null graphics device
  pdf(file = NULL)
  
  # textplot.character
  expect_error(textplot("Hello World\nNext line"), NA)
  
  # textplot.matrix
  m <- matrix(1:6, ncol=2)
  rownames(m) <- c("R1", "R2", "R3")
  colnames(m) <- c("C1", "C2")
  expect_error(textplot(m), NA)
  
  # test with different alignments
  expect_error(textplot(m, halign="left", valign="top"), NA)
  expect_error(textplot(m, halign="right", valign="bottom"), NA)
  
  # textplot.data.frame
  df <- as.data.frame(m)
  expect_error(textplot(df), NA)
  
  # textplot.default (object without specific method)
  y <- 1:3
  x <- 1:3
  expect_error(textplot(lm(y ~ x)), NA)
  
  dev.off()
})

test_that("replaceTabs works", {
  skip_on_cran()
  
  if (exists("replaceTabs", asNamespace("PerformanceAnalytics"))) {
    txt <- "A\tB\tC"
    res <- PerformanceAnalytics:::replaceTabs(txt)
    expect_true(is.character(res))
    expect_false(grepl("\t", res))
  }
})
