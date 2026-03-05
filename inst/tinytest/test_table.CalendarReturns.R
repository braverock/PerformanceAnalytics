library(tinytest)
library(PerformanceAnalytics)
library(xts)

# ------------------------------------------------------------------------------
# Test 1: table.CalendarReturns preserves identical behavior for monthly inputs
# ------------------------------------------------------------------------------
data(managers)
# Managers dataset contains monthly periodicity returns
res_monthly <- table.CalendarReturns(managers[, 1, drop = FALSE])

expect_inherits(res_monthly, "data.frame", 
                info = "Output should remain a data.frame")
expect_equal(ncol(res_monthly), 13, 
             info = "Output should have 12 months + 1 annual summary column")
expect_equal(colnames(res_monthly)[13], colnames(managers)[1], 
             info = "The 13th column should match the asset name")

# ------------------------------------------------------------------------------
# Test 2: table.CalendarReturns successfully converts and outputs daily returns
# ------------------------------------------------------------------------------
# Mock 2 years of daily data
dates <- seq(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day")
dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]

set.seed(123)
daily_rets <- xts(rnorm(length(dates), mean = 0.001, sd = 0.01), order.by = dates)
colnames(daily_rets) <- "Asset_A"

res_daily <- table.CalendarReturns(daily_rets, digits = 2)

# Check dimension shapes and conversions
expect_inherits(res_daily, "data.frame", 
                info = "Output should be a data.frame")
expect_equal(ncol(res_daily), 13, 
             info = "Output should have 13 columns")
expect_equal(nrow(res_daily), 2, 
             info = "Output should collapse 2 years of daily data into 2 rows")
expect_equal(rownames(res_daily), c("2020", "2021"), 
             info = "Rownames should correctly identify the years")
expect_true(!all(is.na(res_daily)), 
            info = "Output should contain calculated numeric values, not just NAs")

# ------------------------------------------------------------------------------
# Test 3: respects geometric vs arithmetic accumulation on daily inputs
# ------------------------------------------------------------------------------
# Setup 1 full month of exactly 1% returns per business day
dates_jan <- seq(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day")
dates_jan <- dates_jan[!weekdays(dates_jan) %in% c("Saturday", "Sunday")]
daily_rets_jan <- xts(rep(0.01, length(dates_jan)), order.by = dates_jan)
colnames(daily_rets_jan) <- "Asset_A"

res_geom <- table.CalendarReturns(daily_rets_jan, geometric = TRUE, as.perc = FALSE, digits = 4)
res_arith <- table.CalendarReturns(daily_rets_jan, geometric = FALSE, as.perc = FALSE, digits = 4)

# Total days in Jan 2020: 23 weekdays
n_days <- length(dates_jan)

# Arithmetic calculation (0.01 * 23)
expected_arith <- round(0.01 * n_days, 4)

# Geometric calculation ((1 + 0.01)^23 - 1)
expected_geom <- round((1.01 ^ n_days) - 1, 4)

expect_equal(res_arith["2020", "Jan"], expected_arith, 
             info = "Should correctly calculate arithmetic sums")
expect_equal(res_geom["2020", "Jan"], expected_geom, 
             info = "Should correctly calculate geometric compounding")