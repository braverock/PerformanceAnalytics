library(PerformanceAnalytics)

test_that("M3 and M4 moment estimation works", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:50, 1:4]
  
  expect_true(is.vector(M3.MM(R, as.mat=FALSE)))
  expect_true(is.matrix(M3.MM(R, as.mat=TRUE)))
  
  expect_true(is.vector(M4.MM(R, as.mat=FALSE)))
  expect_true(is.matrix(M4.MM(R, as.mat=TRUE)))
  
  # Conversion tests
  vec <- M3.MM(R, as.mat=FALSE)
  mat <- M3.vec2mat(vec, ncol(R))
  expect_true(is.matrix(mat))
  
  mat2 <- M3.MM(R, as.mat=TRUE)
  vec2 <- M3.mat2vec(mat2)
  expect_true(is.vector(vec2))
  
  vec4 <- M4.MM(R, as.mat=FALSE)
  mat4 <- M4.vec2mat(vec4, ncol(R))
  expect_true(is.matrix(mat4))
  
  mat42 <- M4.MM(R, as.mat=TRUE)
  vec42 <- M4.mat2vec(mat42)
  expect_true(is.vector(vec42))
})
