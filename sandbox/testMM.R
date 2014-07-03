library(microbenchmark)

# Test for equality
set.seed(123)
x <- matrix(rnorm(1000 * 5), 1000, 5)

all.equal(PerformanceAnalytics:::M3.MM(x), PerformanceAnalytics:::M3.MM.old(x))
all.equal(PerformanceAnalytics:::M4.MM(x), PerformanceAnalytics:::M4.MM.old(x))

data(edhec)
all.equal(PerformanceAnalytics:::M3.MM(edhec), PerformanceAnalytics:::M3.MM.old(edhec))
all.equal(PerformanceAnalytics:::M4.MM(edhec), PerformanceAnalytics:::M4.MM.old(edhec))

# Do benchmarks with bigger data set
m <- 100000
n <- 10
set.seed(21)
r <- matrix(rnorm(m * n), m, n)

benchM3 <- microbenchmark(
  PerformanceAnalytics:::M3.MM(r), 
  PerformanceAnalytics:::M3.MM.old(r),
  times=10
)

benchM3
plot(benchM3, main="M3 Benchmarks")

benchM4 <- microbenchmark(
  PerformanceAnalytics:::M4.MM(r), 
  PerformanceAnalytics:::M4.MM.old(r),
  times=10
)

benchM4
plot(benchM4, main="M4 Benchmarks")
