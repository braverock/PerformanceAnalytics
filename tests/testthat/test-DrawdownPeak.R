# context: DrawdownPeak and PainIndex regression tests --------------------------------

test_that("DrawdownPeak returns expected drawdowns for decimal returns", {
  returns_xts <- xts::xts(
    c(0.02, -0.01, 0.015, -0.03, 0.01),
    order.by = as.Date("2020-01-01") + 0:4
  )

  wealth <- cumprod(1 + as.numeric(returns_xts))
  running_peak <- cummax(wealth)
  expected <- xts::xts(
    wealth / running_peak - 1,
    order.by = zoo::index(returns_xts)
  )

  actual <- DrawdownPeak(returns_xts)

  expect_s3_class(actual, "xts")
  expect_equal(actual, expected, tolerance = 1e-10)
})

test_that("DrawdownPeak handles vector input consistently", {
  returns_vec <- c(0.02, -0.01, 0.015, -0.03, 0.01)
  wealth <- cumprod(1 + returns_vec)
  expected <- wealth / cummax(wealth) - 1

  actual <- DrawdownPeak(returns_vec)

  expect_type(actual, "double")
  expect_equal(actual, expected, tolerance = 1e-10)
})

test_that("PainIndex computes mean absolute drawdown using decimal returns", {
  returns_xts <- xts::xts(
    c(0.02, -0.01, 0.015, -0.03, 0.01),
    order.by = as.Date("2020-01-01") + 0:4
  )

  drawdowns <- DrawdownPeak(returns_xts)
  drawdowns_numeric <- as.numeric(drawdowns)
  expected_pi <- mean(abs(drawdowns_numeric))

  actual_pi <- as.numeric(PainIndex(returns_xts))

  expect_equal(actual_pi, expected_pi, tolerance = 1e-10)
})
