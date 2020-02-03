
# Test environments passing
* Local: R version 3.6.2 (2019-12-12), x86_64-pc-linux-gnu (64-bit), 
Ubuntu 18.04.3 LTS
* Local: R version 3.6.2 (2019-12-12), x86_64-pc-linux-gnu (64-bit), 
Ubuntu 19.10
* travis-ci: R version 3.6.2 (2017-01-27), Platform: x86_64-pc-linux-gnu (64-bit), 
Ubuntu 14.04.5 LTS
* R-hub: using R version 3.6.2 (2019-12-12), x86_64-apple-darwin15.6.0 (64-bit), 
macOS 10.11 El Capitan, R-release (experimental)
* R-hub: using R version 3.6.2 (2019-12-12), x86_64-w64-mingw32 (64-bit)-devel, 
Windows Server 2008 R2 SP1, R-devel, 32/64 bit 
* R-hub: using R version 3.6.2 (2019-12-12), x86_64-w64-mingw32 (64-bit)-release, 
Windows Server 2008 R2 SP1, R-release, 32/64 bit
* R-hub: using R version 4.0.0 Under development (Testing Rtools) (2019-09-30 r77236), 
x86_64-w64-mingw32 (64-bit) 
Windows Server 2012, R-devel, Rtools4.0, 32/64 bit (experimental)
* Win-builder R-devel ATC (alternative toolchain): using R version 4.0.0 Under development (unstable) (2020-01-27 r77730),
x86_64-w64-mingw32 (64-bit)


## Local 18.03.3 LTS R CMD check --as-cran results
R CMD check results
Status: OK
R CMD check succeeded

## Local 19.10 R CMD check --as-cran results
R CMD check results
Status: OK
R CMD check succeeded

## travis-ci
Done. Your build exited with 0.

## R-hub x86_64-apple-darwin15.6.0 (64-bit)
Status: 1 NOTE
checking Rd cross-references ... NOTE
Packages unavailable to check Rd xrefs: 'timeSeries', 'tseries', 'car'

## R-hub x86_64-w64-mingw32 (64-bit)-devel
Status: 1 NOTE
checking Rd cross-references ... NOTE
Packages unavailable to check Rd xrefs: 'timeSeries', 'tseries', 'car'

## R-hub x86_64-w64-mingw32 (64-bit)-release
Status: 1 NOTE
checking Rd cross-references ... NOTE
Packages unavailable to check Rd xrefs: 'timeSeries', 'tseries', 'car'

## R-hub windows-x86_64-devel-rtools4
Status: 1 NOTE
checking Rd cross-references ... NOTE
Packages unavailable to check Rd xrefs: 'timeSeries', 'tseries', 'car'

## * Win-builder R-devel ATC (alternative toolchain):
Status: 2 NOTEs

** running examples for arch 'i386' ... [191s] NOTE
Examples with CPU (user + system) or elapsed time > 10s
                           user system elapsed
chart.RollingPerformance  25.60   0.02   25.73
charts.RollingPerformance 19.21   0.01   19.31
ES                        11.79   0.02   11.99
VaR                       10.95   0.02   10.98
StdDev                    10.44   0.02   10.63

** running examples for arch 'x64' ... [204s] NOTE
Examples with CPU (user + system) or elapsed time > 10s
                           user system elapsed
chart.RollingPerformance  23.94   0.02   24.15
charts.RollingPerformance 21.23   0.05   21.29
StdDev                    13.62   0.01   13.68
ES                        13.60   0.01   14.65
VaR                       13.32   0.02   13.31
