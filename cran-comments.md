
# Test environments passing
* Local: R version 3.6.2 (2018-07-02), x86_64-pc-linux-gnu (64-bit), Ubuntu 18.04.3 LTS
* travis-ci: R version 3.6.2 (2017-01-27), Platform: x86_64-pc-linux-gnu (64-bit), Ubuntu 14.04.5 LTS
* R-hub: using R version 3.6.2 (2019-12-12), x86_64-apple-darwin15.6.0 (64-bit), macOS 10.11 El Capitan, R-release (experimental)
* R-hub: using R version 3.6.2 (2019-12-12), x86_64-w64-mingw32 (64-bit)-devel, Windows Server 2008 R2 SP1, R-devel, 32/64 bit 
* R-hub: using R version 3.6.2 (2019-12-12), x86_64-w64-mingw32 (64-bit)-release, Windows Server 2008 R2 SP1, R-release, 32/64 bit


## Local R CMD check results
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
