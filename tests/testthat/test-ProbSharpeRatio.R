library(PerformanceAnalytics)

test_that("ProbSharpeRatio handles refSR correctly across dimensions", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:3]
  
  # 1. Provide matrix with identical dims as sr (1xN)
  refSR_matrix <- matrix(c(0.1, 0.1, 0.1), nrow=1)
  res_mat <- ProbSharpeRatio(R, refSR=refSR_matrix)
  expect_true(is.matrix(res_mat$sr_prob))
  expect_equal(dim(res_mat$sr_prob), c(1, 3))
  expect_equal(dim(res_mat$sr_confidence_interval), c(3, 3))
  
  # 2. Scalar refSR correctly broadcast
  res_scalar <- ProbSharpeRatio(R, refSR=0.1)
  expect_equal(dim(res_scalar$sr_prob), c(1, 3))
  expect_equal(dim(res_scalar$sr_confidence_interval), c(3, 3))
  
  # 3. Vector refSR
  res_vec <- ProbSharpeRatio(R, refSR=c(0.1, 0.1, 0.1))
  expect_equal(res_vec$sr_prob, res_mat$sr_prob, ignore_attr = TRUE)
  
  # 4. Trigger the warning/subsetting logic when refSR > sr
  # CTA Global SR is ~0.29, make it fail by giving it 0.4
  suppressWarnings({
    res_subset <- ProbSharpeRatio(R, refSR=c(0.1, 0.4, 0.1))
  })
  
  # Ensure the dims reflect dropping exactly 1 column
  expect_true(is.matrix(res_subset$sr_prob))
  expect_equal(dim(res_subset$sr_prob), c(1, 2))
  expect_equal(dim(res_subset$sr_confidence_interval), c(2, 3))
  
  # 5. Check NA logic
  expect_false(any(is.na(res_subset$sr_confidence_interval)))
  
  # 6. Manual inputs (R=NULL) correctly produce dimensions
  res_manual <- ProbSharpeRatio(R=NULL, refSR=0.1, n=50, sr=c(0.2, 0.3, 0.4), sk=c(0,0,0), kr=c(3,3,3))
  expect_true(is.matrix(res_manual$sr_prob))
  expect_equal(dim(res_manual$sr_prob), c(1, 3))
  expect_equal(dim(res_manual$sr_confidence_interval), c(3, 3))
})
