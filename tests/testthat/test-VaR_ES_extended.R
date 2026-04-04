library(PerformanceAnalytics)

test_that("Extended VaR and ES methods function correctly (fixes #139)", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:100, 1:3]
  w <- c(0.3, 0.4, 0.3)

  # Lognormal method tests
  expect_error(res_ln_var <- VaR(R, method = "lognormal"), NA)
  expect_equal(dim(res_ln_var), c(1, 3))
  expect_error(res_ln_es <- ES(R, method = "lognormal"), NA)
  expect_equal(dim(res_ln_es), c(1, 3))

  # Check portfolio weighting scalar applies properly across columns
  res_ln_w <- VaR(R, method = "lognormal", weights = w)
  expect_equal(as.numeric(res_ln_w)[1], as.numeric(res_ln_var)[1] * 0.3)

  # GPD method tests
  # GPD optimization can fail silently or return NA if tail lacks elements, which is acceptable
  expect_error(res_gpd_var <- VaR(R, method = "gpd"), NA)
  expect_equal(dim(res_gpd_var), c(1, 3))
  expect_error(res_gpd_es <- ES(R, method = "gpd"), NA)
  expect_equal(dim(res_gpd_es), c(1, 3))

  # Explicitly test GPD specific arguments
  expect_error(res_gpd_var_args <- VaR(R, method = "gpd", p.tr = 0.90, init = c(1.5, 0.2)), NA)
  expect_equal(dim(res_gpd_var_args), c(1, 3))
  # Ensure they evaluate differently with different tail thresholds
  expect_false(isTRUE(all.equal(as.numeric(res_gpd_var), as.numeric(res_gpd_var_args))))

  # Test GPD specific SE bounds
  expect_error(res_gpd_se_var <- suppressWarnings(VaR(R[, 1], method = "gpd", SE = TRUE)), NA)
  expect_equal(dim(res_gpd_se_var), c(3, 1)) # Should be VaR, LCL, UCL bounds

  # VaR should be above LCL when valid, but could fail and return NA under test simulation
  if (!is.na(res_gpd_se_var[1, 1]) && !is.na(res_gpd_se_var[2, 1])) {
    expect_true(res_gpd_se_var[1, 1] >= res_gpd_se_var[2, 1])
  }

  expect_error(res_gpd_se_es <- suppressWarnings(ES(R[, 1], method = "gpd", SE = TRUE)), NA)
  expect_equal(dim(res_gpd_se_es), c(3, 1))

  # Monte Carlo method tests
  set.seed(42)
  expect_error(res_mc_var <- VaR(R, method = "montecarlo", nsim = 100), NA)
  expect_equal(dim(res_mc_var), c(1, 3))
  expect_error(res_mc_es <- ES(R, method = "montecarlo", nsim = 100), NA)
  expect_equal(dim(res_mc_es), c(1, 3))

  # Explicitly test Monte Carlo nsim arguments
  set.seed(42)
  expect_error(res_mc_var_nsim <- VaR(R, method = "montecarlo", nsim = 200), NA)
  expect_equal(dim(res_mc_var_nsim), c(1, 3))
  # With a different nsim, the estimated quantile should differ
  expect_false(isTRUE(all.equal(as.numeric(res_mc_var), as.numeric(res_mc_var_nsim))))

  # Multivariate evaluation checking
  # Assert component/marginal unsupported exceptions fire
  expect_error(VaR(R, method = "lognormal", portfolio_method = "component"), "no component method defined")
  expect_error(VaR(R, method = "gpd", portfolio_method = "marginal"), "no marginal method defined")

  # Assert Monte Carlo component methods evaluate correctly
  set.seed(42)
  expect_error(res_mc_comp_var <- VaR(R, method = "montecarlo", portfolio_method = "component", weights = w, nsim = 100), NA)
  expect_true(is.list(res_mc_comp_var))
  expect_equal(length(res_mc_comp_var), 3)

  set.seed(42)
  expect_error(res_mc_comp_es <- ES(R, method = "montecarlo", portfolio_method = "component", weights = w, nsim = 100), NA)
  expect_true(is.list(res_mc_comp_es))
  expect_equal(length(res_mc_comp_es), 3)
})
