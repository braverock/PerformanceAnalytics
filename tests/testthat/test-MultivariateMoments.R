library(PerformanceAnalytics)

test_that("MultivariateMoments shrinkage functions work", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  f <- rowSums(R)
  
  targets <- c(1, 2, 3)
  
  # M2
  m2_sh <- M2.shrink(R, targets = targets, f = f)
  expect_true(is.list(m2_sh))
  expect_true(!is.null(m2_sh$M2sh))
  
  # M3
  m3_sh <- M3.shrink(R, targets = targets, f = f, as.mat = TRUE)
  expect_true(is.list(m3_sh))
  expect_true(is.matrix(m3_sh$M3sh))
  
  m3_sh_vec <- M3.shrink(R, targets = targets, f = f, as.mat = FALSE)
  expect_true(is.vector(m3_sh_vec$M3sh))
  
  # M4
  m4_sh <- M4.shrink(R, targets = targets, f = f, as.mat = TRUE)
  expect_true(is.list(m4_sh))
  expect_true(is.matrix(m4_sh$M4sh))
  
  m4_sh_vec <- M4.shrink(R, targets = targets, f = f, as.mat = FALSE)
  expect_true(is.vector(m4_sh_vec$M4sh))
})

test_that("MultivariateMoments struct functions work", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  f <- rowSums(R)
  
  structs <- c("Indep", "IndepId", "observedfactor", "CC")
  
  for (struct in structs) {
    if (struct == "observedfactor") {
      m2 <- M2.struct(R, struct, f)
      m3 <- M3.struct(R, struct, f)
      m4 <- M4.struct(R, struct, f)
    } else {
      m2 <- M2.struct(R, struct)
      m3 <- M3.struct(R, struct)
      m4 <- M4.struct(R, struct)
    }
    expect_true(is.matrix(m2))
    expect_true(is.matrix(m3))
    expect_true(is.matrix(m4))
  }
})

test_that("MultivariateMoments EWMA functions work", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  m2 <- M2.ewma(R, 0.94)
  m3 <- M3.ewma(R, 0.94)
  m4 <- M4.ewma(R, 0.94)
  
  expect_true(is.matrix(m2))
  expect_true(is.matrix(m3))
  expect_true(is.matrix(m4))
})

test_that("MultivariateMoments MCA functions work", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  m3 <- M3.MCA(R, k = 2, as.mat = TRUE)
  expect_true(is.list(m3))
  expect_true(is.matrix(m3$M3mca))
  
  m4 <- M4.MCA(R, k = 2, as.mat = TRUE)
  expect_true(is.list(m4))
  expect_true(is.matrix(m4$M4mca))
})
