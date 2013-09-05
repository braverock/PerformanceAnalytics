data("RealInt")
#OLS-based CUSUM test with quadratic spectral kernel HAC estimate:
  ocus <- gefp(RealInt ~ 1, fit = lm, vcov = kernHAC)
plot(ocus, aggregate = FALSE)
sctest(ocus)
#supF test with quadratic spectral kernel HAC estimate:
  fs <- Fstats(RealInt ~ 1, vcov = kernHAC)
plot(fs)
sctest(fs)
#Breakpoint estimation and conﬁdence intervals with quadratic spectral kernel HAC estimate:
  bp <- breakpoints(RealInt ~ 1)
confint(bp, vcov = kernHAC)
plot(bp)
#Visualization:
  plot(RealInt, ylab = "Real interest rate")
lines(ts(fitted(bp), start = start(RealInt), freq = 4), col = 4)
lines(confint(bp, vcov = kernHAC))