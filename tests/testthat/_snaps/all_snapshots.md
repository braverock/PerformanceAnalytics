# Snapshot for ActivePremium

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > ActivePremium(managers[, "HAM1", drop = FALSE], managers[, 
      +     "SP500 TR", drop = FALSE])
      [1] 0.0407
      
      > ActivePremium(managers[, 1, drop = FALSE], managers[, 
      +     8, drop = FALSE])
      [1] 0.0407
      
      > ActivePremium(managers[, 1:6], managers[, 8, drop = FALSE])
                                     HAM1       HAM2       HAM3       HAM4       HAM5
      Active Premium: SP500 TR 0.0407 0.0775 0.0544 0.0247 0.0218
                                     HAM6
      Active Premium: SP500 TR 0.0758
      
      > ActivePremium(managers[, 1:6], managers[, 8:7, drop = FALSE])
                                        HAM1       HAM2       HAM3        HAM4
      Active Premium: SP500 TR    0.0407 0.0775 0.0544  0.0247
      Active Premium: EDHEC LS EQ 0.0196 0.0377 0.0104 -0.0046
                                         HAM5       HAM6
      Active Premium: SP500 TR     0.0218 0.0758
      Active Premium: EDHEC LS EQ -0.0323 0.0546
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for AdjustedSharpeRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(AdjustedSharpeRatio(portfolio_bacon[, 1]))
                                              portfolio.monthly.return....
      Annualized Sharpe Ratio (Rf=0%, p=95%):                    0.7913
      
      > data(managers)
      
      > print(AdjustedSharpeRatio(managers["1996"]))
                                                HAM1     HAM2   HAM3     HAM4 HAM5
      Adjusted Sharpe ratio (Risk free = 0) 1.9617 8.6297 1.1666 1.7707   NA
                                            HAM6 EDHEC LS EQ SP500 TR  US 10Y TR
      Adjusted Sharpe ratio (Risk free = 0)   NA          NA 1.8602 0.0384
                                             US 3m TR
      Adjusted Sharpe ratio (Risk free = 0) -543.8279
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for AppraisalRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(AppraisalRatio(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2], method = "appraisal"))
      [1] -0.4302
      
      > print(AppraisalRatio(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2], method = "modified"))
      [1] -0.0141
      
      > print(AppraisalRatio(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2], method = "alternative"))
      [1] -0.1066
      
      > data(managers)
      
      > print(AppraisalRatio(managers["1996", 1], managers["1996", 
      +     8]))
      [1] 1.6230
      
      > print(AppraisalRatio(managers["1996", 1:5], managers["1996", 
      +     8]))
                                          HAM1 HAM2     HAM3      HAM4 HAM5
      Appraisal ratio (Risk free = 0) 1.6230   NA 3.5277 0.7070   NA
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for BernardoLedoitRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(BernardoLedoitRatio(portfolio_bacon[, 1]))
      [1] 1.7797
      
      > data(managers)
      
      > print(BernardoLedoitRatio(managers["1996"]))
                                    HAM1 HAM2     HAM3     HAM4 HAM5 HAM6 EDHEC LS EQ
      Bernardo and Ledoit ratio 4.5983 2375 6.4828 3.6150  NaN  NaN         NaN
                                SP500 TR US 10Y TR US 3m TR
      Bernardo and Ledoit ratio 4.3406  1.0282     -Inf
      
      > print(BernardoLedoitRatio(managers["1996", 1]))
      [1] 4.5983
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for BetaCoMoments

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > BetaCoVariance(managers[, "HAM2", drop = FALSE], managers[, 
      +     "SP500 TR", drop = FALSE])
      [1] 0.3431
      
      > BetaCoSkewness(managers[, "HAM2", drop = FALSE], managers[, 
      +     "SP500 TR", drop = FALSE])
      [1] 0.0454
      
      > BetaCoKurtosis(managers[, "HAM2", drop = FALSE], managers[, 
      +     "SP500 TR", drop = FALSE])
      [1] 0.1988
      
      > BetaCoKurtosis(managers[, 1:6], managers[, 8, drop = FALSE])
                                     HAM1      HAM2     HAM3      HAM4      HAM5
      Beta Cokurtosis: SP500 TR 0.4814 0.1988 0.5068 0.8483 0.2738
                                     HAM6
      Beta Cokurtosis: SP500 TR 0.1541
      
      > BetaCoKurtosis(managers[, 1:6], managers[, 8:7])
                                        HAM1      HAM2     HAM3      HAM4      HAM5
      Beta Cokurtosis: SP500 TR    0.4814 0.1988 0.5068 0.8483 0.2738
      Beta Cokurtosis: EDHEC LS EQ 0.7100 1.2676 1.4266 1.4533 1.2831
                                        HAM6
      Beta Cokurtosis: SP500 TR    0.1541
      Beta Cokurtosis: EDHEC LS EQ 0.8618
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for BurkeRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(BurkeRatio(portfolio_bacon[, 1]))
      [1] 0.7447
      
      > print(BurkeRatio(portfolio_bacon[, 1], modified = TRUE))
      [1] 3.6484
      
      > data(managers)
      
      > print(BurkeRatio(managers["1996"]))
                                      HAM1 HAM2     HAM3     HAM4 HAM5 HAM6
      Burke ratio (Risk free = 0) 4.7797  Inf 6.3404 4.0481   NA   NA
                                  EDHEC LS EQ SP500 TR   US 10Y TR US 3m TR
      Burke ratio (Risk free = 0)          NA 4.7398 0.0061      Inf
      
      > print(BurkeRatio(managers["1996", 1]))
      [1] 4.7797
      
      > print(BurkeRatio(managers["1996"], modified = TRUE))
                                               HAM1 HAM2     HAM3     HAM4 HAM5 HAM6
      Modified Burke ratio (Risk free = 0) 16.5575  Inf 21.9640 14.0232   NA   NA
                                           EDHEC LS EQ SP500 TR  US 10Y TR US 3m TR
      Modified Burke ratio (Risk free = 0)          NA 16.4192 0.0212      Inf
      
      > print(BurkeRatio(managers["1996", 1], modified = TRUE))
      [1] 16.5575
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for CAPM.RiskPremium

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > CAPM.CML.slope(managers[, "SP500 TR", drop = FALSE], 
      +     managers[, 10, drop = FALSE])
                                           SP500 TR
      Capital Market Line Slope: SP500 TR 0.1257
      
      > CAPM.CML(managers[, "HAM1", drop = FALSE], managers[, 
      +     "SP500 TR", drop = FALSE], Rf = 0)
      [1] 0.0022
      
      > CAPM.RiskPremium(managers[, "SP500 TR", drop = FALSE], 
      +     Rf = 0)
                              SP500 TR
      Risk Premium (Rf=0%) 0.0086
      
      > CAPM.RiskPremium(managers[, "HAM1", drop = FALSE], 
      +     Rf = 0)
                                 HAM1
      Risk Premium (Rf=0%) 0.0111
      
      > CAPM.SML.slope(managers[, "SP500 TR", drop = FALSE], 
      +     Rf = 0)
                                     SP500 TR
      Security Market Line: SP500 TR 115.4023
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for CAPM.dynamic

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > CAPM.dynamic(managers[, 1, drop = FALSE], managers[, 
      +     8, drop = FALSE], Rf = 0.035/12, Z = managers[, 9:10])
                       Average alpha US 10Y TR alpha at t - 1 US 3m TR alpha at t - 1
      HAM1 to SP500 TR     0.0070                -0.1963               0.1665
                       Average beta US 10Y TR beta at t - 1 US 3m TR beta at t - 1
      HAM1 to SP500 TR    0.3248                3.4933              -63.7481
      
      > CAPM.dynamic(managers[80:120, 1:6], managers[80:120, 
      +     7, drop = FALSE], Rf = managers[80:120, 10, drop = FALSE], 
      +     Z = managers[80:120, 9:10])
                          Average alpha US 10Y TR alpha at t - 1
      HAM1 to EDHEC LS EQ -0.0001              -0.2389
      HAM2 to EDHEC LS EQ -0.0027              -0.0663
      HAM3 to EDHEC LS EQ  0.0062              -0.2173
      HAM4 to EDHEC LS EQ -0.0033               0.1613
      HAM5 to EDHEC LS EQ  0.0043               0.2688
      HAM6 to EDHEC LS EQ -0.0053               0.0500
                          US 3m TR alpha at t - 1 Average beta
      HAM1 to EDHEC LS EQ              -0.4385    1.1793
      HAM2 to EDHEC LS EQ              -4.0176    0.7067
      HAM3 to EDHEC LS EQ               7.6804    0.4260
      HAM4 to EDHEC LS EQ              -0.2091    1.6367
      HAM5 to EDHEC LS EQ               3.8497    1.2224
      HAM6 to EDHEC LS EQ              -3.0664    1.6281
                          US 10Y TR beta at t - 1 US 3m TR beta at t - 1
      HAM1 to EDHEC LS EQ                3.8612              -51.0140
      HAM2 to EDHEC LS EQ                5.6820              171.1665
      HAM3 to EDHEC LS EQ                1.5079             -705.2035
      HAM4 to EDHEC LS EQ               -7.6221             -565.8519
      HAM5 to EDHEC LS EQ                7.0839               39.7035
      HAM6 to EDHEC LS EQ              -11.0351              343.5289
      
      > CAPM.dynamic(managers[80:120, 1:6], managers[80:120, 
      +     8:7], managers[80:120, 10, drop = FALSE], Z = managers[80:120, 
      +     9:10])
                          Average alpha US 10Y TR alpha at t - 1
      HAM1 to SP500 TR     0.0036              -0.0353
      HAM2 to SP500 TR     0.0016              -0.0548
      HAM3 to SP500 TR     0.0072              -0.0597
      HAM4 to SP500 TR    -0.0015               0.4131
      HAM5 to SP500 TR     0.0083               0.3530
      HAM6 to SP500 TR     0.0012               0.0352
      HAM1 to EDHEC LS EQ -0.0001              -0.2389
      HAM2 to EDHEC LS EQ -0.0027              -0.0663
      HAM3 to EDHEC LS EQ  0.0062              -0.2173
      HAM4 to EDHEC LS EQ -0.0033               0.1613
      HAM5 to EDHEC LS EQ  0.0043               0.2688
      HAM6 to EDHEC LS EQ -0.0053               0.0500
                          US 3m TR alpha at t - 1 Average beta
      HAM1 to SP500 TR                 0.0850   0.5186
      HAM2 to SP500 TR                -2.9183   0.0515
      HAM3 to SP500 TR                 4.1023   0.1772
      HAM4 to SP500 TR                -6.0409   1.2056
      HAM5 to SP500 TR                 1.5669   0.5721
      HAM6 to SP500 TR                -1.7231   0.5961
      HAM1 to EDHEC LS EQ             -0.4385   1.1793
      HAM2 to EDHEC LS EQ             -4.0176   0.7067
      HAM3 to EDHEC LS EQ              7.6804   0.4260
      HAM4 to EDHEC LS EQ             -0.2091   1.6367
      HAM5 to EDHEC LS EQ              3.8497   1.2224
      HAM6 to EDHEC LS EQ             -3.0664   1.6281
                          US 10Y TR beta at t - 1 US 3m TR beta at t - 1
      HAM1 to SP500 TR                  -1.1810              -65.7367
      HAM2 to SP500 TR                   2.0755              -23.7998
      HAM3 to SP500 TR                   1.0633             -256.1934
      HAM4 to SP500 TR                  -1.8122              162.0345
      HAM5 to SP500 TR                   4.2773              183.0620
      HAM6 to SP500 TR                  -5.1063              189.5137
      HAM1 to EDHEC LS EQ                3.8612              -51.0140
      HAM2 to EDHEC LS EQ                5.6820              171.1665
      HAM3 to EDHEC LS EQ                1.5079             -705.2035
      HAM4 to EDHEC LS EQ               -7.6221             -565.8519
      HAM5 to EDHEC LS EQ                7.0839               39.7035
      HAM6 to EDHEC LS EQ              -11.0351              343.5289
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for CAPM.epsilon

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(SFM.epsilon(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2]))
      [1] -0.0131
      
      > data(managers)
      
      > print(SFM.epsilon(managers["1996", 1], managers["1996", 
      +     8]))
      [1] 0.0742
      
      > print(SFM.epsilon(managers["1996", 1:5], managers["1996", 
      +     8]))
                                               HAM1      HAM2      HAM3       HAM4
      Regression epsilon (Risk free = 0) 0.0742 0.5399 0.2048 0.0557
                                         HAM5
      Regression epsilon (Risk free = 0)   NA
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for CAPM.jensenAlpha

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(SFM.jensenAlpha(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2]))
      [1] -0.0141
      
      > data(managers)
      
      > print(SFM.jensenAlpha(managers["1996", 1], managers["1996", 
      +     8]))
      [1] 0.0807
      
      > print(SFM.jensenAlpha(managers["1996", 1:5], managers["1996", 
      +     8]))
                                            HAM1      HAM2      HAM3       HAM4 HAM5
      Jensen's Alpha (Risk free = 0%) 0.0807 0.5636 0.2196 0.0606   NA
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for CDD

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > t(round(CDD(edhec), 4))
                             Conditional Drawdown 95%
      Convertible Arbitrage                    0.1487
      CTA Global                               0.1087
      Distressed Securities                    0.1760
      Emerging Markets                         0.2910
      Equity Market Neutral                    0.0692
      Event Driven                             0.1537
      Fixed Income Arbitrage                   0.1151
      Global Macro                             0.0528
      Long/Short Equity                        0.1455
      Merger Arbitrage                         0.0583
      Relative Value                           0.0928
      Short Selling                            0.7687
      Funds of Funds                           0.1224
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for CDaR.alpha

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > CDaR.alpha(edhec[, 1], edhec[, 2])
      [1] 0.1005
      
      > CDaR.alpha(edhec[, 1], edhec[, 2], type = "max")
            max 
      0.1114 
      
      > CDaR.alpha(edhec[, 1], edhec[, 2], type = "average")
        average 
      0.1217 
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for CDaR.beta

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > CDaR.beta(edhec[, 1], edhec[, 2])
      [1] -0.5417
      
      > CDaR.beta(edhec[, 1], edhec[, 2], type = "max")
             max 
      -0.7480 
      
      > CDaR.beta(edhec[, 1], edhec[, 2], type = "average")
         average 
      -0.9412 
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for CalmarRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > CalmarRatio(managers[, 1, drop = FALSE])
                        HAM1
      Calmar Ratio 0.9061
      
      > CalmarRatio(managers[, 1:6])
                        HAM1     HAM2      HAM3      HAM4      HAM5     HAM6
      Calmar Ratio 0.9061 0.7280 0.5225 0.4227 0.1095 1.7425
      
      > SterlingRatio(managers[, 1, drop = FALSE])
                                         HAM1
      Sterling Ratio (Excess = 10%) 0.5462
      
      > SterlingRatio(managers[, 1:6])
                                         HAM1      HAM2      HAM3      HAM4
      Sterling Ratio (Excess = 10%) 0.5462 0.5138 0.3883 0.3136
                                          HAM5      HAM6
      Sterling Ratio (Excess = 10%) 0.0847 0.7678
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for CoMoments

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > CoVariance(managers[, "HAM2", drop = FALSE], managers[, 
      +     "SP500 TR", drop = FALSE])
      [1] 0.0006
      
      > CoSkewness(managers[, "HAM2", drop = FALSE], managers[, 
      +     "SP500 TR", drop = FALSE])
      [1] -2.1012e-6
      
      > CoKurtosis(managers[, "HAM2", drop = FALSE], managers[, 
      +     "SP500 TR", drop = FALSE])
      [1] 2.5790e-6
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for DRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(DRatio(portfolio_bacon[, 1]))
      [1] 0.4013
      
      > data(managers)
      
      > print(DRatio(managers["1996"]))
                    HAM1         HAM2       HAM3      HAM4 HAM5 HAM6 EDHEC LS EQ
      d ratio 0.0724 0.0001 0.0308 0.1383  NaN  NaN         NaN
                SP500 TR US 10Y TR US 3m TR
      d ratio 0.0460  1.3615        0
      
      > print(DRatio(managers["1996", 1]))
      [1] 0.0724
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for DownsideDeviation

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > MAR = 0.005
      
      > DownsideDeviation(portfolio_bacon[, 1], MAR)
                 [,1]
      [1,] 0.0255
      
      > DownsidePotential(portfolio_bacon[, 1], MAR)
                 [,1]
      [1,] 0.0137
      
      > data(managers)
      
      > apply(managers[, 1:6], 2, sd, na.rm = TRUE)
            HAM1       HAM2       HAM3       HAM4       HAM5       HAM6 
      0.0256 0.0367 0.0365 0.0531 0.0457 0.0238 
      
      > DownsideDeviation(managers[, 1:6])
                                          HAM1      HAM2       HAM3       HAM4
      Downside Deviation (MAR = 0%) 0.0145 0.0115 0.0173 0.0340
                                         HAM5       HAM6
      Downside Deviation (MAR = 0%) 0.0304 0.0121
      
      > DownsideDeviation(managers[, 1:6], MAR = 0.04/12)
                                         HAM1       HAM2       HAM3       HAM4
      Downside Deviation (MAR = 0%) 0.0157 0.0134 0.0189 0.0356
                                         HAM5       HAM6
      Downside Deviation (MAR = 0%) 0.0320 0.0136
      
      > SemiDeviation(managers[, 1, drop = FALSE])
                          HAM1
      Semi-Deviation 0.0190
      
      > SemiDeviation(managers[, 1:6])
                          HAM1       HAM2       HAM3       HAM4       HAM5       HAM6
      Semi-Deviation 0.0190 0.0201 0.0236 0.0395 0.0324 0.0175
      
      > SemiVariance(managers[, 1, drop = FALSE])
                            HAM1
      Semi-Variance 0.0007
      
      > SemiVariance(managers[, 1:6])
                            HAM1         HAM2        HAM3        HAM4        HAM5
      Semi-Variance 0.0007 0.0006 0.0010 0.0033 0.0020
                           HAM6
      Semi-Variance 0.0006
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for DownsideFrequency

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > MAR = 0.005
      
      > print(DownsideFrequency(portfolio_bacon[, 1], MAR))
      [1] 0.4583
      
      > data(managers)
      
      > print(DownsideFrequency(managers["1996"]))
                                    HAM1 HAM2      HAM3      HAM4 HAM5 HAM6
      Downside Frequency (MAR = 0%) 0.25  0.2 0.1666 0.3333  NaN  NaN
                                    EDHEC LS EQ  SP500 TR US 10Y TR US 3m TR
      Downside Frequency (MAR = 0%)         NaN 0.1666 0.5833        0
      
      > print(DownsideFrequency(managers["1996", 1]))
      [1] 0.25
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for DownsideSharpeRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec, package = "PerformanceAnalytics")
      
      > class(edhec)
      [1] "xts" "zoo"
      
      > names(edhec) = c("CA", "CTA", "DIS", "EM", "EMN", 
      +     "ED", "FIA", "GM", "LS", "MA", "RV", "SS", "FOF")
      
      > DownsideSharpeRatio(edhec)
                                   CA       CTA       DIS        EM       EMN
      Downside Sharpe Ratio 0.3001 0.1951 0.3315 0.1866 0.4720
                                   ED       FIA       GM        LS        MA
      Downside Sharpe Ratio 0.3074 0.3155 0.4262 0.3055 0.4428
                                   RV          SS       FOF
      Downside Sharpe Ratio 0.4168 -0.0301 0.2668
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for ES

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > ES(edhec, p = 0.95, method = "historical")
         Convertible Arbitrage CTA Global Distressed Securities Emerging Markets
      ES              -0.0387   -0.0406              -0.0413      -0.0754
         Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      ES              -0.0175  -0.0444            -0.0290  -0.0210
         Long/Short Equity Merger Arbitrage Relative Value Short Selling
      ES       -0.0448      -0.0233    -0.0271   -0.0948
         Funds of Funds
      ES    -0.0356
      
      > ES(edhec, p = 0.95, method = "gaussian")
         Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      ES           -0.0287 -0.0426            -0.0305      -0.0606
         Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      ES           -0.0125  -0.0325            -0.0191  -0.0245
         Long/Short Equity Merger Arbitrage Relative Value Short Selling
      ES       -0.0363      -0.0180    -0.0187   -0.0949
         Funds of Funds
      ES    -0.0286
      
      > ES(edhec, p = 0.95, method = "modified")
         Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      ES           -0.0894 -0.0403           -0.0673       -0.1157
         Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      ES           -0.0367  -0.0811            -0.0530  -0.0169
         Long/Short Equity Merger Arbitrage Relative Value Short Selling
      ES       -0.0485      -0.0513    -0.0475   -0.0675
         Funds of Funds
      ES    -0.0459
      
      > ES(edhec, p = 0.99)
         Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      ES           -0.0953 -0.0520           -0.0709       -0.1261
         Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      ES           -0.0387  -0.0843            -0.0603  -0.0230
         Long/Short Equity Merger Arbitrage Relative Value Short Selling
      ES       -0.0658      -0.0576    -0.0488    -0.1941
         Funds of Funds
      ES    -0.0542
      
      > ES(edhec, p = 0.01)
         Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      ES           -0.0953 -0.0520           -0.0709       -0.1261
         Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      ES           -0.0387  -0.0843            -0.0603  -0.0230
         Long/Short Equity Merger Arbitrage Relative Value Short Selling
      ES       -0.0658      -0.0576    -0.0488    -0.1941
         Funds of Funds
      ES    -0.0542
      
      > if (requireNamespace("robustbase", quietly = TRUE)) {
      +     ES(edhec, clean = "boudt")
      +     ES(edhec, clean = "boudt", portfolio_method = "component")
      + }
    Message
      no weights passed in, assuming equal weighted portfolio
    Output
      $MES
      [1] 0.0231
      
      $contribution
       Convertible Arbitrage             CTA Global  Distressed Securities 
                0.0027          -0.0014           0.0029 
            Emerging Markets  Equity Market Neutral           Event Driven 
                0.0052           0.0009           0.0033 
      Fixed Income Arbitrage           Global Macro      Long/Short Equity 
                0.0022           0.0007           0.0030 
            Merger Arbitrage         Relative Value          Short Selling 
                0.0015           0.0020          -0.0034 
              Funds of Funds 
                0.0031 
      
      $pct_contrib_MES
       Convertible Arbitrage             CTA Global  Distressed Securities 
                  0.1203            -0.0638             0.1267 
            Emerging Markets  Equity Market Neutral           Event Driven 
                  0.2278             0.0396             0.1468 
      Fixed Income Arbitrage           Global Macro      Long/Short Equity 
                  0.0983             0.0341             0.1314 
            Merger Arbitrage         Relative Value          Short Selling 
                  0.0658             0.0868            -0.1483 
              Funds of Funds 
                  0.1342 
      
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for EWMAMoments

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > sigma <- M2.ewma(edhec, 0.94)
      
      > m3 <- M3.ewma(edhec, 0.94)
      
      > m4 <- M4.ewma(edhec, 0.94)
      
      > mu <- colMeans(edhec)
      
      > p <- length(mu)
      
      > ES(p = 0.95, portfolio_method = "component", weights = rep(1/p, 
      +     p), mu = mu, sigma = sigma, m3 = m3, m4 = m4)
      $MES
      [1] 0.0354
      
      $contribution
       [1]  3.8395e-3 -2.2235e-3  6.6104e-3  6.5060e-3  8.6532e-4
       [6]  7.2763e-3  2.3245e-3 -7.8425e-5  3.1280e-3  3.9323e-3
      [11]  3.6308e-3 -3.9056e-3  3.5883e-3
      
      $pct_contrib_MES
       [1]  0.1081 -0.0626  0.1862  0.1832  0.0243
       [6]  0.2050  0.0654 -0.0022  0.0881  0.1107
      [11]  0.1022 -0.1100  0.1010
      
      
      > sigma <- cov(edhec)
      
      > m3 <- M3.MM(edhec)
      
      > m4 <- M4.MM(edhec)
      
      > ES(p = 0.95, portfolio_method = "component", weights = rep(1/p, 
      +     p), mu = mu, sigma = sigma, m3 = m3, m4 = m4)
      $MES
      [1] 0.0362
      
      $contribution
       [1]  0.0059 -0.0028  0.0052  0.0062  0.0009
       [6]  0.0058  0.0034  0.0001  0.0036  0.0034
      [11]  0.0037 -0.0031  0.0034
      
      $pct_contrib_MES
       [1]  0.1648 -0.0774  0.1456  0.1724  0.0250
       [6]  0.1626  0.0955  0.0036  0.1000  0.0954
      [11]  0.1042 -0.0869  0.0948
      
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for FamaBeta

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(FamaBeta(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2]))
                                   portfolio.monthly.return....
      portfolio.monthly.return....                     1.0303
      
      > data(managers)
      
      > print(FamaBeta(managers["1996", 1], managers["1996", 
      +     8]))
                HAM1
      HAM1 0.5351
      
      > print(FamaBeta(managers["1996", 1:5], managers["1996", 
      +     8]))
                      HAM1 HAM2     HAM3     HAM4 HAM5
      Fama Beta  0.5351   NA 1.0070 1.0376   NA
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for Frequency

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(Frequency(portfolio_bacon[, 1]))
      [1] 12
      
      > data(managers)
      
      > print(Frequency(managers["1996", 1:5]))
                HAM1 HAM2 HAM3 HAM4 HAM5
      Frequency   12   12   12   12   NA
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for InformationRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > InformationRatio(managers[, "HAM1", drop = FALSE], 
      +     managers[, "SP500 TR", drop = FALSE])
      [1] 0.3604
      
      > InformationRatio(managers[, 1:6], managers[, 8, drop = FALSE])
                                       HAM1      HAM2      HAM3     HAM4      HAM5
      Information Ratio: SP500 TR 0.3604 0.5059 0.4701 0.1549 0.1212
                                       HAM6
      Information Ratio: SP500 TR 0.6722
      
      > InformationRatio(managers[, 1:6], managers[, 8:7])
                                          HAM1      HAM2      HAM3       HAM4
      Information Ratio: SP500 TR    0.3604 0.5059 0.4701  0.1549
      Information Ratio: EDHEC LS EQ 0.2593 0.4162 0.1279 -0.0294
                                           HAM5      HAM6
      Information Ratio: SP500 TR     0.1212 0.6722
      Information Ratio: EDHEC LS EQ -0.2277 0.9667
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for Kappa

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > l = 2
      
      > data(portfolio_bacon)
      
      > MAR = 0.005
      
      > print(Kappa(portfolio_bacon[, 1], MAR, l))
      [1] 0.1566
      
      > data(managers)
      
      > MAR = 0
      
      > print(Kappa(managers["1996"], MAR, l))
                           HAM1     HAM2     HAM3    HAM4 HAM5 HAM6 EDHEC LS EQ
      kappa (MAR = 0%) 1.4920 1061.685 2.2351 1.1418  NaN  NaN         NaN
                       SP500 TR  US 10Y TR US 3m TR
      kappa (MAR = 0%) 1.2743 0.0167      Inf
      
      > print(Kappa(managers["1996", 1], MAR, l))
      [1] 1.4920
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for KellyRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > KellyRatio(managers[, 1, drop = FALSE], Rf = 0.04/12)
                      HAM1
      Kelly Ratio 5.9294
      
      > KellyRatio(managers[, 1, drop = FALSE], Rf = managers[, 
      +     10, drop = FALSE])
                      HAM1
      Kelly Ratio 6.0108
      
      > KellyRatio(managers[, 1:6], Rf = managers[, 10, drop = FALSE])
                      HAM1     HAM2     HAM3     HAM4      HAM5     HAM6
      Kelly Ratio 6.0108 4.0698 3.4581 1.3763 0.3876 7.9482
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for Level.calculate

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(prices)
      
      > ret <- Return.calculate(as.xts(prices), method = "discrete")
      
      > prices_recovered <- Level.calculate(ret, seedValue = 100)
      
      > head(prices_recovered)
                 AdjClose
      1999-01-04 100.0000
      1999-01-05 103.6218
      1999-01-06 103.1356
      1999-01-07 103.9256
      1999-01-08 102.4915
      1999-01-11 103.4152
      
      > data(managers)
      
      > mgr_eq <- managers[, 1:6]
      
      > xtsAttributes(mgr_eq) <- list(coredata_content = "discreteReturn")
      
      > mgr_level <- Level.calculate(mgr_eq)
      
      > Return.cumulative(mgr_eq)
                            HAM1     HAM2     HAM3    HAM4      HAM5      HAM6
      Cumulative Return 3.1266 4.3485 3.7067 2.5294 0.2650 0.9858
      
      > tail(mgr_level - 1, 1)
                     HAM1     HAM2     HAM3    HAM4      HAM5      HAM6
      2006-12-31 3.1266 4.3485 3.7067 2.5294 0.2650 0.9858
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for M2Sortino

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > MAR = 0.005
      
      > print(M2Sortino(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2], MAR))
                                 portfolio.monthly.return....
      Sortino Ratio (MAR = 0.5%)                    0.1034
      
      > data(managers)
      
      > MAR = 0
      
      > print(MSquaredExcess(managers["1996", 1], managers["1996", 
      +     8], MAR))
                 SP500 TR
      SP500 TR 0.0202
      
      > print(MSquaredExcess(managers["1996", 1:5], managers["1996", 
      +     8], MAR))
                                           HAM1 HAM2      HAM3        HAM4 HAM5
      MSquaredExcess (Risk free = 0) 0.0202   NA 0.1409 -0.0254   NA
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for MCA

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > M3mca <- M3.MCA(edhec, k = 2)$M3mca
      
      > M3dist <- M4dist <- rep(NA, ncol(edhec))
      
      > M3S <- M3.MM(edhec)
      
      > M4S <- M4.MM(edhec)
      
      > for (k in 1:ncol(edhec)) {
      +     M3MCA_list <- M3.MCA(edhec, k)
      +     M4MCA_list <- M4.MCA(edhec, k)
      +     M3dist[k] <- sqrt(sum((M3S - M3MCA_list$M3mca)^2))
      +     M4dist[k] <- sqrt(sum((M4S - M4MCA_list$M4mca)^2))
      + }
      
      > par(mfrow = c(2, 1))
      
      > plot(1:ncol(edhec), M3dist)
      
      > plot(1:ncol(edhec), M4dist)
      
      > par(mfrow = c(1, 1))
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for MSquared

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(MSquared(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2]))
                           benchmark.return....
      benchmark.return....              0.1006
      
      > data(managers)
      
      > print(MSquared(managers["1996", 1], managers["1996", 
      +     8]))
                SP500 TR
      SP500 TR 0.2544
      
      > print(MSquared(managers["1996", 1:5], managers["1996", 
      +     8]))
                                    HAM1 HAM2      HAM3      HAM4 HAM5
      MSquared (Risk free = 0) 0.2544   NA 0.4028 0.1982   NA
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for MSquaredExcess

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > MSquaredExcess(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2])
                           benchmark.return....
      benchmark.return....          -0.0155
      
      > MSquaredExcess(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2], Method = "arithmetic")
                           benchmark.return....
      benchmark.return....          -0.0173
      
      > data(managers)
      
      > MSquaredExcess(managers["1996", 1], managers["1996", 
      +     8])
                 SP500 TR
      SP500 TR 0.0202
      
      > MSquaredExcess(managers["1996", 1:5], managers["1996", 
      +     8])
                                           HAM1 HAM2      HAM3        HAM4 HAM5
      MSquaredExcess (Risk free = 0) 0.0202   NA 0.1409 -0.0254   NA
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for MarketTiming

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > MarketTiming(managers[, 1], managers[, 8], Rf = 0.035/12, 
      +     method = "HM")
                             Alpha      Beta      Gamma
      HAM1 to SP500 TR 0.0082 0.3211 -0.1344
      
      > MarketTiming(managers[80:120, 1:6], managers[80:120, 
      +     7], managers[80:120, 10])
                                  Alpha      Beta     Gamma
      HAM1 to EDHEC LS EQ -0.0005 1.3121 -0.4051
      HAM2 to EDHEC LS EQ -0.0003 0.4370  8.5206
      HAM3 to EDHEC LS EQ -0.0058 1.1898 11.9137
      HAM4 to EDHEC LS EQ -0.0055 2.0616 18.7973
      HAM5 to EDHEC LS EQ  0.0005 1.0703 -5.0778
      HAM6 to EDHEC LS EQ  0.0003 1.2711 -7.4434
      
      > MarketTiming(managers[80:120, 1:6], managers[80:120, 
      +     8:7], managers[80:120, 10], method = "TM")
                                  Alpha      Beta      Gamma
      HAM1 to SP500 TR     0.0048 0.5970 -0.2801
      HAM2 to SP500 TR     0.0050 0.1190 -0.5000
      HAM3 to SP500 TR     0.0032 0.5272 -0.6645
      HAM4 to SP500 TR     0.0094 0.8779 -0.8155
      HAM5 to SP500 TR     0.0087 0.2869 -2.7728
      HAM6 to SP500 TR     0.0048 0.2902  0.6910
      HAM1 to EDHEC LS EQ -0.0005 1.3121 -0.4051
      HAM2 to EDHEC LS EQ -0.0003 0.4370  8.5206
      HAM3 to EDHEC LS EQ -0.0058 1.1898 11.9137
      HAM4 to EDHEC LS EQ -0.0055 2.0616 18.7973
      HAM5 to EDHEC LS EQ  0.0005 1.0703 -5.0778
      HAM6 to EDHEC LS EQ  0.0003 1.2711 -7.4434
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for MartinRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(MartinRatio(portfolio_bacon[, 1]))
                  portfolio.monthly.return....
      Ulcer Index                     1.6945
      
      > data(managers)
      
      > print(MartinRatio(managers["1996"]))
                                HAM1     HAM2  HAM3    HAM4 HAM5 HAM6 EDHEC LS EQ
      Martin Ratio (Rf = 0) 15.5212 16390.63 19.35 13.5996   NA   NA          NA
                            SP500 TR  US 10Y TR US 3m TR
      Martin Ratio (Rf = 0) 14.7080 0.0101      Inf
      
      > print(MartinRatio(managers["1996", 1]))
                      HAM1
      Ulcer Index 15.5212
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for MeanAbsoluteDeviation

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(MeanAbsoluteDeviation(portfolio_bacon[, 1]))
      [1] 0.0310
      
      > data(managers)
      
      > print(MeanAbsoluteDeviation(managers["1996"]))
                                   HAM1     HAM2       HAM3       HAM4 HAM5 HAM6
      Mean absolute deviation 0.0125 0.0315 0.0222 0.0254  NaN  NaN
                              EDHEC LS EQ SP500 TR  US 10Y TR US 3m TR
      Mean absolute deviation         NaN  0.0222 0.0161  0.0002
      
      > print(MeanAbsoluteDeviation(managers["1996", 1]))
      [1] 0.0125
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for MinTrackRecord

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > MinTrackRecord(edhec[, 1], refSR = 0.23)
      $min_TRL
                                             Convertible Arbitrage (SR > 0.23 )
      Minimum Track Record Length (p= 95 %):                           397.5894
      
      $IS_SR_SIGNIFICANT
      [1] FALSE
      
      $num_of_extra_obs_needed
      [1] 105
      
      
      > MinTrackRecord(refSR = 1/12^0.5, Rf = 0, p = 0.95, 
      +     sr = 2/12^0.5, sk = -0.72, kr = 5.78, n = 59)
      $min_TRL
      [1] 52.3736
      
      $IS_SR_SIGNIFICANT
      [1] TRUE
      
      $num_of_extra_obs_needed
      [1] 0
      
      
      > MinTrackRecord(edhec[, 1:2], refSR = c(0.28, 0.24), 
      +     ignore_skewness = FALSE, ignore_kurtosis = FALSE)
      $min_TRL
      [1] 1583.033
      
      $IS_SR_SIGNIFICANT
      [1] FALSE
      
      $num_of_extra_obs_needed
      [1] 1291
      
      
      > MinTrackRecord(edhec[, 1:2], refSR = c(0.28, 0.24), 
      +     ignore_skewness = FALSE, ignore_kurtosis = TRUE)
      $min_TRL
      [1] 1233.385
      
      $IS_SR_SIGNIFICANT
      [1] FALSE
      
      $num_of_extra_obs_needed
      [1] 941
      
      
      > MinTrackRecord(edhec[, 1:2], refSR = c(0.28, 0.24), 
      +     ignore_skewness = TRUE, ignore_kurtosis = TRUE)
      $min_TRL
      [1] 668.2947
      
      $IS_SR_SIGNIFICANT
      [1] FALSE
      
      $num_of_extra_obs_needed
      [1] 376
      
      
      > MinTrackRecord(edhec[, 1:2], refSR = 0.26, weights = c(0.5, 
      +     0.5), ignore_skewness = FALSE, ignore_kurtosis = FALSE)
      $min_TRL
                                             portfolio.returns (SR > 0.26 )
      Minimum Track Record Length (p= 95 %):                       275.0956
      
      $IS_SR_SIGNIFICANT
      [1] TRUE
      
      $num_of_extra_obs_needed
      [1] 0
      
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for Modigliani

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > Modigliani(managers[, 1, drop = FALSE], managers[, 
      +     8, drop = FALSE], Rf = 0.035/12)
      [1] 0.0167
      
      > Modigliani(managers[, 1:6], managers[, 8, drop = FALSE], 
      +     managers[, 8, drop = FALSE])
                                                    HAM1       HAM2       HAM3
      Modigliani-Modigliani measure: SP500 TR 0.0119 0.0139 0.0135
                                                    HAM4       HAM5       HAM6
      Modigliani-Modigliani measure: SP500 TR 0.0108 0.0103 0.0158
      
      > Modigliani(managers[, 1:6], managers[, 8:7], managers[, 
      +     8, drop = FALSE])
                                                       HAM1       HAM2       HAM3
      Modigliani-Modigliani measure: SP500 TR    0.0119 0.0139 0.0135
      Modigliani-Modigliani measure: EDHEC LS EQ 0.0102 0.0111 0.0109
                                                        HAM4        HAM5       HAM6
      Modigliani-Modigliani measure: SP500 TR    0.0108 0.0103 0.0158
      Modigliani-Modigliani measure: EDHEC LS EQ 0.0097 0.0094 0.0120
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for NCE

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
      dev.off()
    Output
      pdf 
        2 

# Snapshot for NetSelectivity

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(NetSelectivity(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2]))
                                   portfolio.monthly.return....
      portfolio.monthly.return....                   -0.0178
      
      > data(managers)
      
      > print(NetSelectivity(managers["1996", 1], managers["1996", 
      +     8]))
                 HAM1
      HAM1 0.0133
      
      > print(NetSelectivity(managers["1996", 1:5], managers["1996", 
      +     8]))
                                            HAM1 HAM2      HAM3        HAM4 HAM5
      Net Selectivity (Risk free = 0) 0.0133   NA 0.1745 -0.0324   NA
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for Omega

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > Omega(edhec)
                     Convertible Arbitrage CTA Global Distressed Securities
      Omega (L = 0%)              2.8484   1.6185              2.7565
                     Emerging Markets Equity Market Neutral Event Driven
      Omega (L = 0%)         1.7529              4.2917     2.6301
                     Fixed Income Arbitrage Global Macro Long/Short Equity
      Omega (L = 0%)               3.3690      2.8979          2.3144
                     Merger Arbitrage Relative Value Short Selling Funds of Funds
      Omega (L = 0%)         3.9553       3.6620     0.9247       2.1856
      
      > if (requireNamespace("Hmisc", quietly = TRUE)) {
      +     Omega(edhec[, 13], method = "interp", output = "point")
      +     Omega(edhec[, 13], method = "interp", output = "full")
      + }
             Funds of Funds
        [1,]            Inf
        [2,]            Inf
        [3,]   1.4828e-3
        [4,]   1.4238e-3
        [5,]   9.8326e-2
        [6,]   5.1231e-1
        [7,]   5.0187e-1
        [8,]   4.9051e-1
        [9,]   4.8261e-1
       [10,]   4.7414e-1
       [11,]   4.3142e-1
       [12,]   3.2583e-1
       [13,]   2.7847e-1
       [14,]   2.7071e-1
       [15,]   2.4584e-1
       [16,]   2.1041e-1
       [17,]   1.8523e-1
       [18,]   1.7276e-1
       [19,]   1.6093e-1
       [20,]   1.5928e-1
       [21,]   1.4951e-1
       [22,]   1.4791e-1
       [23,]   1.4628e-1
       [24,]   1.4298e-1
       [25,]   1.3488e-1
       [26,]   1.3328e-1
       [27,]   1.2548e-1
       [28,]   1.2396e-1
       [29,]   1.1799e-1
       [30,]   1.1368e-1
       [31,]   1.0948e-1
       [32,]   9.9015e-0
       [33,]   9.4153e-0
       [34,]   8.8379e-0
       [35,]   8.3966e-0
       [36,]   8.1814e-0
       [37,]   7.7647e-0
       [38,]   7.1772e-0
       [39,]   7.0832e-0
       [40,]   6.8062e-0
       [41,]   6.6264e-0
       [42,]   6.3637e-0
       [43,]   6.1934e-0
       [44,]   6.1089e-0
       [45,]   6.0245e-0
       [46,]   5.9407e-0
       [47,]   5.8563e-0
       [48,]   5.4447e-0
       [49,]   5.3654e-0
       [50,]   5.1338e-0
       [51,]   4.7698e-0
       [52,]   4.4320e-0
       [53,]   4.1170e-0
       [54,]   3.8794e-0
       [55,]   3.7078e-0
       [56,]   3.6515e-0
       [57,]   3.5407e-0
       [58,]   3.4860e-0
       [59,]   3.3780e-0
       [60,]   3.2221e-0
       [61,]   3.1716e-0
       [62,]   3.0728e-0
       [63,]   2.9299e-0
       [64,]   2.8835e-0
       [65,]   2.7928e-0
       [66,]   2.7484e-0
       [67,]   2.6192e-0
       [68,]   2.4958e-0
       [69,]   2.4167e-0
       [70,]   2.3780e-0
       [71,]   2.3021e-0
       [72,]   2.2649e-0
       [73,]   2.2282e-0
       [74,]   2.1920e-0
       [75,]   2.1563e-0
       [76,]   2.1211e-0
       [77,]   2.0187e-0
       [78,]   1.9211e-0
       [79,]   1.8584e-0
       [80,]   1.7975e-0
       [81,]   1.7677e-0
       [82,]   1.6530e-0
       [83,]   1.5981e-0
       [84,]   1.5449e-0
       [85,]   1.5189e-0
       [86,]   1.4932e-0
       [87,]   1.4431e-0
       [88,]   1.4185e-0
       [89,]   1.3704e-0
       [90,]   1.3469e-0
       [91,]   1.3237e-0
       [92,]   1.2785e-0
       [93,]   1.2347e-0
       [94,]   1.2134e-0
       [95,]   1.1923e-0
       [96,]   1.1716e-0
       [97,]   1.1513e-0
       [98,]   1.1312e-0
       [99,]   1.0921e-0
      [100,]   1.0542e-0
      [101,]   1.0358e-0
      [102,]   1.0176e-0
      [103,]   9.8226e-1
      [104,]   9.3137e-1
      [105,]   8.6736e-1
      [106,]   8.5201e-1
      [107,]   8.3692e-1
      [108,]   8.2209e-1
      [109,]   7.6515e-1
      [110,]   7.5149e-1
      [111,]   7.3806e-1
      [112,]   7.2485e-1
      [113,]   6.7411e-1
      [114,]   6.4994e-1
      [115,]   6.3816e-1
      [116,]   6.2660e-1
      [117,]   6.1526e-1
      [118,]   6.0412e-1
      [119,]   5.9319e-1
      [120,]   5.8245e-1
      [121,]   5.7191e-1
      [122,]   5.5141e-1
      [123,]   5.4143e-1
      [124,]   5.3165e-1
      [125,]   5.2203e-1
      [126,]   5.1259e-1
      [127,]   5.0334e-1
      [128,]   4.8538e-1
      [129,]   4.7665e-1
      [130,]   4.5967e-1
      [131,]   4.5140e-1
      [132,]   4.3529e-1
      [133,]   4.2744e-1
      [134,]   4.1976e-1
      [135,]   4.1225e-1
      [136,]   4.0490e-1
      [137,]   3.9769e-1
      [138,]   3.9061e-1
      [139,]   3.8367e-1
      [140,]   3.7689e-1
      [141,]   3.7027e-1
      [142,]   3.5747e-1
      [143,]   3.2732e-1
      [144,]   3.1596e-1
      [145,]   3.0500e-1
      [146,]   2.9968e-1
      [147,]   2.8935e-1
      [148,]   2.7939e-1
      [149,]   2.7457e-1
      [150,]   2.5167e-1
      [151,]   2.4303e-1
      [152,]   2.2665e-1
      [153,]   2.2274e-1
      [154,]   2.1890e-1
      [155,]   2.0777e-1
      [156,]   2.0418e-1
      [157,]   1.9720e-1
      [158,]   1.9383e-1
      [159,]   1.8731e-1
      [160,]   1.8415e-1
      [161,]   1.8104e-1
      [162,]   1.7801e-1
      [163,]   1.7503e-1
      [164,]   1.6925e-1
      [165,]   1.6092e-1
      [166,]   1.5561e-1
      [167,]   1.5305e-1
      [168,]   1.4319e-1
      [169,]   1.4083e-1
      [170,]   1.3397e-1
      [171,]   1.3177e-1
      [172,]   1.2538e-1
      [173,]   1.1932e-1
      [174,]   1.1738e-1
      [175,]   1.0813e-1
      [176,]   1.0468e-1
      [177,]   1.0305e-1
      [178,]   9.8308e-2
      [179,]   8.8002e-2
      [180,]   8.3889e-2
      [181,]   7.8690e-2
      [182,]   7.6213e-2
      [183,]   6.6969e-2
      [184,]   6.3753e-2
      [185,]   6.2722e-2
      [186,]   6.1719e-2
      [187,]   6.0755e-2
      [188,]   5.9828e-2
      [189,]   5.7157e-2
      [190,]   5.3791e-2
      [191,]   5.1406e-2
      [192,]   5.0644e-2
      [193,]   4.9178e-2
      [194,]   4.8473e-2
      [195,]   4.7117e-2
      [196,]   4.6466e-2
      [197,]   4.5217e-2
      [198,]   4.0585e-2
      [199,]   3.4925e-2
      [200,]   2.9529e-2
      [201,]   2.5202e-2
      [202,]   2.2742e-2
      [203,]   2.2416e-2
      [204,]   2.0298e-2
      [205,]   1.9192e-2
      [206,]   1.5062e-2
      [207,]   1.3387e-2
      [208,]   1.3196e-2
      [209,]   1.3019e-2
      [210,]   1.2373e-2
      [211,]   1.0008e-2
      [212,]   9.3036e-3
      [213,]   6.2044e-3
      [214,]   5.4092e-3
      [215,]   4.4889e-3
      [216,]   3.0345e-3
      [217,]   1.7806e-3
      [218,]   1.2943e-4
      [219,]   0.0000e-0
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for OmegaExcessReturn

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > MAR = 0.005
      
      > print(OmegaExcessReturn(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2], MAR))
                 [,1]
      [1,] 0.0805
      
      > data(managers)
      
      > MAR = 0
      
      > print(OmegaExcessReturn(managers["1996", 1], managers["1996", 
      +     8], MAR))
                [,1]
      [1,] 0.1325
      
      > print(OmegaExcessReturn(managers["1996", 1:5], managers["1996", 
      +     8], MAR))
                                         HAM1 HAM2      HAM3      HAM4 HAM5
      Omega Excess Return (MAR = 0) 0.1325   NA 0.3991 0.1985   NA
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for OmegaSharpeRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > MAR = 0.005
      
      > print(OmegaSharpeRatio(portfolio_bacon[, 1], MAR))
                [,1]
      [1,] 0.2917
      
      > MAR = 0
      
      > data(managers)
      
      > print(OmegaSharpeRatio(managers["1996"], MAR))
                                      HAM1 HAM2     HAM3     HAM4 HAM5 HAM6
      OmegaSharpeRatio (MAR = 0%) 3.5983 2374 5.4828 2.6150   NA   NA
                                  EDHEC LS EQ SP500 TR  US 10Y TR US 3m TR
      OmegaSharpeRatio (MAR = 0%)          NA 3.3406 0.0282      Inf
      
      > print(OmegaSharpeRatio(managers["1996", 1], MAR))
               [,1]
      [1,] 3.5983
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for PainIndex

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(PainIndex(portfolio_bacon[, 1]))
                 portfolio.monthly.return....
      Pain Index                   0.0399
      
      > data(managers)
      
      > print(PainIndex(100 * managers["1996"]))
                     HAM1  HAM2   HAM3     HAM4 HAM5 HAM6 EDHEC LS EQ SP500 TR
      Pain Index 6.2299 0.002 0.2525 1.4502  NaN  NaN         NaN 218.6989
                 US 10Y TR US 3m TR
      Pain Index   1.3891        0
      
      > print(PainIndex(100 * managers["1996", 1]))
                     HAM1
      Pain Index 6.2299
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for PainRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(PainRatio(portfolio_bacon[, 1]))
                 portfolio.monthly.return....
      Pain Index                     2.5926
      
      > data(managers)
      
      > print(PainRatio(managers["1996"]))
                             HAM1     HAM2     HAM3     HAM4 HAM5 HAM6 EDHEC LS EQ
      Pain Ratio (Rf = 0) 36.6663 36650.56 43.0626 27.7174   NA   NA          NA
                          SP500 TR  US 10Y TR US 3m TR
      Pain Ratio (Rf = 0) 31.2920 0.0119      Inf
      
      > print(PainRatio(managers["1996", 1]))
                    HAM1
      Pain Index 36.6663
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for ProbSharpeRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > ProbSharpeRatio(edhec[, 1], refSR = 0.23)
      $sr_prob
                                           Convertible Arbitrage (SR > 0.23 )
      Probabilistic Sharpe Ratio(p= 95 %):                          0.9209
      
      $sr_confidence_interval
                            Lower Bound Sharpe Ratio Upper Bound
      Convertible Arbitrage      0.2109       0.3455      0.4802
      
      
      > ProbSharpeRatio(refSR = 1/12^0.5, Rf = 0, p = 0.95, 
      +     sr = 2/12^0.5, sk = -0.72, kr = 5.78, n = 59)
      $sr_prob
                                           Series 1 (SR > 0.29 )
      Probabilistic Sharpe Ratio(p= 95 %):             0.9597
      
      $sr_confidence_interval
        Lower Bound Sharpe Ratio Upper Bound
      1      0.3057       0.5774       0.849
      
      
      > ProbSharpeRatio(edhec[, 1:2], refSR = c(0.28, 0.24), 
      +     ignore_skewness = FALSE, ignore_kurtosis = FALSE)
      $sr_prob
                                           Convertible Arbitrage (SR > 0.28 )
      Probabilistic Sharpe Ratio(p= 95 %):                          0.7601
      
      $sr_confidence_interval
                            Lower Bound Sharpe Ratio Upper Bound
      Convertible Arbitrage       0.193       0.3455      0.4981
      
      
      > ProbSharpeRatio(edhec[, 1:2], refSR = c(0.28, 0.24), 
      +     ignore_skewness = FALSE, ignore_kurtosis = TRUE)
      $sr_prob
                                           Convertible Arbitrage (SR > 0.28 )
      Probabilistic Sharpe Ratio(p= 95 %):                          0.7883
      
      $sr_confidence_interval
                            Lower Bound Sharpe Ratio Upper Bound
      Convertible Arbitrage      0.2109       0.3455      0.4802
      
      
      > ProbSharpeRatio(edhec[, 1:2], refSR = c(0.28, 0.24), 
      +     ignore_skewness = TRUE, ignore_kurtosis = TRUE)
      $sr_prob
                                           Convertible Arbitrage (SR > 0.28 )
      Probabilistic Sharpe Ratio(p= 95 %):                          0.8617
      
      $sr_confidence_interval
                            Lower Bound Sharpe Ratio Upper Bound
      Convertible Arbitrage      0.2465       0.3455      0.4446
      
      
      > ProbSharpeRatio(edhec[, 1:2], refSR = 0.26, weights = c(0.5, 
      +     0.5), ignore_skewness = FALSE, ignore_kurtosis = FALSE)
      $sr_prob
                                           portfolio.returns (SR > 0.26 )
      Probabilistic Sharpe Ratio(p= 95 %):                      0.9552
      
      $sr_confidence_interval
                        Lower Bound Sharpe Ratio Upper Bound
      portfolio.returns      0.2635       0.3735      0.4835
      
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for ProspectRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > MAR = 0.05
      
      > print(ProspectRatio(portfolio_bacon[, 1], MAR))
                 [,1]
      [1,] -0.1347
      
      > data(managers)
      
      > MAR = 0
      
      > print(ProspectRatio(managers["1996"], MAR))
                                     HAM1     HAM2     HAM3      HAM4 HAM5 HAM6
      Prospect ratio (MAR = 0%) 0.9737 442.1359 1.7256 0.5960   NA   NA
                                EDHEC LS EQ  SP500 TR  US 10Y TR US 3m TR
      Prospect ratio (MAR = 0%)          NA 0.7975 -0.7234      Inf
      
      > print(ProspectRatio(managers["1996", 1], MAR))
                [,1]
      [1,] 0.9737
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for RPESE.control

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > ES.control <- RPESE.control(estimator = "ES")
      
      > ES.control.2 <- RPESE.control(estimator = "ES", se.method = c("IFcor", 
      +     "BOOTiid"), cleanOutliers = TRUE, freq.include = "Decimate")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for RachevRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec, package = "PerformanceAnalytics")
      
      > class(edhec)
      [1] "xts" "zoo"
      
      > names(edhec) = c("CA", "CTA", "DIS", "EM", "EMN", 
      +     "ED", "FIA", "GM", "LS", "MA", "RV", "SS", "FOF")
      
      > RachevRatio(edhec)
                        CA      CTA      DIS        EM      EMN       ED      FIA
      RachevRatio 1.1794 1.3380 1.1393 0.9967 1.5141 1.0891 1.1351
                        GM       LS     MA       RV       SS      FOF
      RachevRatio 2.0143 1.2386 1.5023 1.1594 1.1345 1.2255
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for Return.Geltner

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > head(Return.Geltner(managers[, 1:3]), n = 20)
                         HAM1          HAM2         HAM3
      1996-01-31           NA            NA           NA
      1996-02-29  0.0220            NA  0.0351
      1996-03-31  0.0146            NA  0.0257
      1996-04-30 -0.0148            NA  0.0450
      1996-05-31  0.0114            NA  0.0352
      1996-06-30 -0.0065            NA -0.0307
      1996-07-31 -0.0275            NA -0.0337
      1996-08-31  0.0540            NA  0.0466
      1996-09-30  0.0089  0.1248  0.0654
      1996-10-31  0.0320  0.0174  0.0393
      1996-11-30  0.0125  0.0835  0.0667
      1996-12-31  0.0180  0.0189  0.0210
      1997-01-31  0.0220  0.0916  0.0774
      1997-02-28 -0.0022 -0.0297 -0.0382
      1997-03-31  0.0110 -0.0315 -0.0335
      1997-04-30  0.0133 -0.0009  0.0290
      1997-05-31  0.0510  0.0686  0.0762
      1997-06-30  0.0182  0.0555  0.0048
      1997-07-31  0.0136  0.1297  0.1088
      1997-08-31  0.0256 -0.0528 -0.0035
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for Return.annualized

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > round(Return.annualized(managers[, 1, drop = FALSE]), 
      +     4)
                          HAM1
      Annualized Return 0.1375
      
      > round(Return.annualized(managers[, 1:8]), 4)
                          HAM1   HAM2   HAM3   HAM4   HAM5   HAM6 EDHEC LS EQ
      Annualized Return 0.1375 0.1747 0.1512 0.1215 0.0373 0.1373       0.118
                        SP500 TR
      Annualized Return   0.0967
      
      > round(Return.annualized(managers[, 1:8], geometric = FALSE), 
      +     4)
                          HAM1   HAM2   HAM3   HAM4   HAM5   HAM6 EDHEC LS EQ
      Annualized Return 0.1335 0.1697 0.1494 0.1322 0.0491 0.1327      0.1145
                        SP500 TR
      Annualized Return    0.104
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for Return.annualized.excess

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > Return.annualized.excess(Rp = managers[, 1], Rb = managers[, 
      +     8])
                              HAM1
      Annualized Return 0.0371
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for Return.calculate

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(prices)
      
      > R.IBM = Return.calculate(xts(prices), method = "discrete")
      
      > colnames(R.IBM) = "IBM"
      
      > chart.CumReturns(R.IBM, legend.loc = "topleft", main = "Cumulative Daily Returns for IBM")
      
      > round(R.IBM, 2)
                   IBM
      1999-01-04    NA
      1999-01-05  0.04
      1999-01-06  0.00
      1999-01-07  0.01
      1999-01-08 -0.01
      1999-01-11  0.01
      1999-01-12 -0.02
      1999-01-13  0.00
      1999-01-14 -0.03
      1999-01-15  0.02
             ...      
      2006-12-15  0.00
      2006-12-18  0.00
      2006-12-19  0.01
      2006-12-20  0.00
      2006-12-21  0.00
      2006-12-22 -0.01
      2006-12-26  0.00
      2006-12-27  0.02
      2006-12-28  0.00
      2006-12-29  0.00
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for Return.clean

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
      dev.off()
    Output
      pdf 
        2 

# Snapshot for Return.convert

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
      dev.off()
    Output
      pdf 
        2 

# Snapshot for Return.cumulative

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > Return.cumulative(managers[, 1, drop = FALSE])
                            HAM1
      Cumulative Return 3.1266
      
      > Return.cumulative(managers[, 1:8])
                            HAM1     HAM2     HAM3    HAM4      HAM5      HAM6
      Cumulative Return 3.1266 4.3485 3.7067 2.5294 0.2650 0.9858
                        EDHEC LS EQ SP500 TR
      Cumulative Return    2.0511 1.7616
      
      > Return.cumulative(managers[, 1:8], geometric = FALSE)
                          HAM1   HAM2  HAM3   HAM4   HAM5   HAM6 EDHEC LS EQ SP500 TR
      Cumulative Return 1.4682 1.7679 1.643 1.4542 0.3148 0.7075      1.1454 1.1438
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for Return.excess

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > head(Return.excess(managers[, 1, drop = FALSE], managers[, 
      +     10, drop = FALSE]))
                 HAM1 > US 3m TR
      1996-01-31         0.0028
      1996-02-29         0.0153
      1996-03-31         0.0117
      1996-04-30        -0.0133
      1996-05-31         0.0031
      1996-06-30        -0.0080
      
      > head(Return.excess(managers[, 1, drop = FALSE], 0.04/12))
                    HAM1 > Rf
      1996-01-31  0.0040
      1996-02-29  0.0159
      1996-03-31  0.0121
      1996-04-30 -0.0124
      1996-05-31  0.0042
      1996-06-30 -0.0072
      
      > head(Return.excess(managers[, 1:6], managers[, 10, 
      +     drop = FALSE]))
                 HAM1 > US 3m TR HAM2 > US 3m TR HAM3 > US 3m TR HAM4 > US 3m TR
      1996-01-31         0.0028              NA         0.0303         0.0176
      1996-02-29         0.0153              NA         0.0311         0.0155
      1996-03-31         0.0117              NA         0.0220        -0.0135
      1996-04-30        -0.0133              NA         0.0406         0.0193
      1996-05-31         0.0031              NA         0.0308        -0.0016
      1996-06-30        -0.0080              NA        -0.0344        -0.0060
                 HAM5 > US 3m TR HAM6 > US 3m TR
      1996-01-31              NA              NA
      1996-02-29              NA              NA
      1996-03-31              NA              NA
      1996-04-30              NA              NA
      1996-05-31              NA              NA
      1996-06-30              NA              NA
      
      > head(Return.excess(managers[, 1, drop = FALSE], managers[, 
      +     8, drop = FALSE]))
                 HAM1 > SP500 TR
      1996-01-31         -0.0266
      1996-02-29          0.0100
      1996-03-31          0.0059
      1996-04-30         -0.0238
      1996-05-31         -0.0182
      1996-06-30         -0.0077
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for Return.locScaleRob

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec, package = "PerformanceAnalytics")
      
      > class(edhec)
      [1] "xts" "zoo"
      
      > names(edhec) = c("CA", "CTA", "DIS", "EM", "EMN", 
      +     "ED", "FIA", "GM", "LS", "MA", "RV", "SS", "FOF")
      
      > if (suppressMessages(suppressWarnings(requireNamespace("RobStatTM", 
      +     quietly = TRUE)))) {
      +     outRob <- suppressMessages(Return.locScaleRob(edhec$CA))
      + }
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for Return.portfolio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > Return.portfolio(edhec["1997", 1:5], rebalance_on = "quarters")
                 portfolio.returns
      1997-01-31       0.0334
      1997-02-28       0.0237
      1997-03-31      -0.0014
      1997-04-30       0.0036
      1997-05-31       0.0176
      1997-06-30       0.0254
      1997-07-31       0.0365
      1997-08-31      -0.0051
      1997-09-30       0.0220
      1997-10-31      -0.0107
      1997-11-30      -0.0026
      1997-12-31       0.0129
      
      > Return.portfolio(edhec["1997", 1:5], rebalance_on = "quarters", 
      +     verbose = TRUE)
      $returns
                 portfolio.returns
      1997-01-31       0.0334
      1997-02-28       0.0237
      1997-03-31      -0.0014
      1997-04-30       0.0036
      1997-05-31       0.0176
      1997-06-30       0.0254
      1997-07-31       0.0365
      1997-08-31      -0.0051
      1997-09-30       0.0220
      1997-10-31      -0.0107
      1997-11-30      -0.0026
      1997-12-31       0.0129
      
      $contribution
                 Convertible Arbitrage    CTA Global Distressed Securities
      1997-01-31           0.0023  0.0078          0.0035
      1997-02-28           0.0024  0.0059          0.0024
      1997-03-31           0.0015 -0.0004         -0.0002
      1997-04-30           0.0017 -0.0034          0.0006
      1997-05-31           0.0031 -0.0002          0.0046
      1997-06-30           0.0042  0.0016          0.0043
      1997-07-31           0.0038  0.0118          0.0046
      1997-08-31           0.0026 -0.0096          0.0029
      1997-09-30           0.0024  0.0038          0.0070
      1997-10-31           0.0020 -0.0019         -0.0012
      1997-11-30           0.0000  0.0026          0.0010
      1997-12-31           0.0013  0.0058          0.0014
                 Emerging Markets Equity Market Neutral
      1997-01-31      0.0158          0.0037
      1997-02-28      0.0109          0.0019
      1997-03-31     -0.0025          0.0003
      1997-04-30      0.0023          0.0023
      1997-05-31      0.0063          0.0038
      1997-06-30      0.0118          0.0033
      1997-07-31      0.0112          0.0049
      1997-08-31     -0.0013          0.0003
      1997-09-30      0.0046          0.0040
      1997-10-31     -0.0114          0.0019
      1997-11-30     -0.0072          0.0008
      1997-12-31      0.0029          0.0013
      
      $BOP.Weight
                 Convertible Arbitrage CTA Global Distressed Securities
      1997-01-31             0.2000  0.2000             0.2000
      1997-02-28             0.1958  0.2011             0.1969
      1997-03-31             0.1936  0.2023             0.1947
      1997-04-30             0.2000  0.2000             0.2000
      1997-05-31             0.2009  0.1958             0.1998
      1997-06-30             0.2005  0.1921             0.2009
      1997-07-31             0.2000  0.2000             0.2000
      1997-08-31             0.1966  0.2043             0.1974
      1997-09-30             0.2003  0.1956             0.2014
      1997-10-31             0.2000  0.2000             0.2000
      1997-11-30             0.2042  0.2001             0.2008
      1997-12-31             0.2047  0.2033             0.2025
                 Emerging Markets Equity Market Neutral
      1997-01-31        0.2000             0.2000
      1997-02-28        0.2088             0.1971
      1997-03-31        0.2147             0.1945
      1997-04-30        0.2000             0.2000
      1997-05-31        0.2016             0.2016
      1997-06-30        0.2043             0.2018
      1997-07-31        0.2000             0.2000
      1997-08-31        0.2037             0.1977
      1997-09-30        0.2034             0.1990
      1997-10-31        0.2000             0.2000
      1997-11-30        0.1906             0.2041
      1997-12-31        0.1838             0.2054
      
      $EOP.Weight
                 Convertible Arbitrage CTA Global Distressed Securities
      1997-01-31             0.1958  0.2011             0.1969
      1997-02-28             0.1936  0.2023             0.1947
      1997-03-31             0.1954  0.2021             0.1947
      1997-04-30             0.2009  0.1958             0.1998
      1997-05-31             0.2005  0.1921             0.2009
      1997-06-30             0.1997  0.1890             0.2002
      1997-07-31             0.1966  0.2043             0.1974
      1997-08-31             0.2003  0.1956             0.2014
      1997-09-30             0.1984  0.1952             0.2039
      1997-10-31             0.2042  0.2001             0.2008
      1997-11-30             0.2047  0.2033             0.2025
      1997-12-31             0.2034  0.2065             0.2013
                 Emerging Markets Equity Market Neutral
      1997-01-31        0.2088             0.1971
      1997-02-28        0.2147             0.1945
      1997-03-31        0.2124             0.1951
      1997-04-30        0.2016             0.2016
      1997-05-31        0.2043             0.2018
      1997-06-30        0.2108             0.2001
      1997-07-31        0.2037             0.1977
      1997-08-31        0.2034             0.1990
      1997-09-30        0.2036             0.1987
      1997-10-31        0.1906             0.2041
      1997-11-30        0.1838             0.2054
      1997-12-31        0.1844             0.2041
      
      $BOP.Value
                 Convertible Arbitrage CTA Global Distressed Securities
      1997-01-31             0.2000  0.2000             0.2000
      1997-02-28             0.2023  0.2078             0.2035
      1997-03-31             0.2048  0.2140             0.2060
      1997-04-30             0.2112  0.2112             0.2112
      1997-05-31             0.2131  0.2077             0.2119
      1997-06-30             0.2164  0.2073             0.2168
      1997-07-31             0.2213  0.2213             0.2213
      1997-08-31             0.2255  0.2343             0.2264
      1997-09-30             0.2286  0.2233             0.2298
      1997-10-31             0.2332  0.2332             0.2332
      1997-11-30             0.2355  0.2309             0.2317
      1997-12-31             0.2355  0.2340             0.2329
                 Emerging Markets Equity Market Neutral
      1997-01-31        0.2000             0.2000
      1997-02-28        0.2158             0.2037
      1997-03-31        0.2271             0.2058
      1997-04-30        0.2112             0.2112
      1997-05-31        0.2138             0.2138
      1997-06-30        0.2205             0.2178
      1997-07-31        0.2213             0.2213
      1997-08-31        0.2337             0.2267
      1997-09-30        0.2321             0.2271
      1997-10-31        0.2332             0.2332
      1997-11-30        0.2198             0.2354
      1997-12-31        0.2115             0.2364
      
      $EOP.Value
                 Convertible Arbitrage CTA Global Distressed Securities
      1997-01-31             0.2023  0.2078             0.2035
      1997-02-28             0.2048  0.2140             0.2060
      1997-03-31             0.2064  0.2136             0.2057
      1997-04-30             0.2131  0.2077             0.2119
      1997-05-31             0.2164  0.2073             0.2168
      1997-06-30             0.2210  0.2091             0.2215
      1997-07-31             0.2255  0.2343             0.2264
      1997-08-31             0.2286  0.2233             0.2298
      1997-09-30             0.2313  0.2277             0.2378
      1997-10-31             0.2355  0.2309             0.2317
      1997-11-30             0.2355  0.2340             0.2329
      1997-12-31             0.2371  0.2407             0.2346
                 Emerging Markets Equity Market Neutral
      1997-01-31        0.2158             0.2037
      1997-02-28        0.2271             0.2058
      1997-03-31        0.2244             0.2061
      1997-04-30        0.2138             0.2138
      1997-05-31        0.2205             0.2178
      1997-06-30        0.2333             0.2214
      1997-07-31        0.2337             0.2267
      1997-08-31        0.2321             0.2271
      1997-09-30        0.2374             0.2317
      1997-10-31        0.2198             0.2354
      1997-11-30        0.2115             0.2364
      1997-12-31        0.2149             0.2379
      
      
      > data(weights)
      
      > chart.StackedBar(weights)
      
      > x <- Return.portfolio(edhec["2000::", 1:11], weights = weights, 
      +     verbose = TRUE)
      
      > chart.CumReturns(x$returns)
      
      > chart.StackedBar(x$BOP.Weight)
      
      > chart.StackedBar(x$BOP.Value)
      
      > Return.portfolio(edhec["1997", 1:5], rebalance_on = "quarters", 
      +     rebal_cost = 0.002)
                 portfolio.returns
      1997-01-31       0.0334
      1997-02-28       0.0237
      1997-03-31      -0.0014
      1997-04-30       0.0036
      1997-05-31       0.0176
      1997-06-30       0.0254
      1997-07-31       0.0364
      1997-08-31      -0.0051
      1997-09-30       0.0220
      1997-10-31      -0.0108
      1997-11-30      -0.0026
      1997-12-31       0.0129
      
      > x_cost <- Return.portfolio(edhec["2000::", 1:11], 
      +     weights = weights, verbose = TRUE, rebal_cost = 0.01)
      
      > chart.CumReturns(x_cost$returns)
      
      > chart.StackedBar(x_cost$BOP.Weight)
      
      > chart.StackedBar(x_cost$BOP.Value)
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for Return.read

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
      dev.off()
    Output
      pdf 
        2 

# Snapshot for Return.relative

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > head(Return.relative(managers[, 1:3], managers[, 8, 
      +     drop = FALSE]), n = 20)
                 HAM1/SP500 TR HAM2/SP500 TR HAM3/SP500 TR
      1996-01-31     0.9742            NA      1.0008
      1996-02-29     0.9839            NA      1.0264
      1996-03-31     0.9896            NA      1.0429
      1996-04-30     0.9664            NA      1.0739
      1996-05-31     0.9493            NA      1.0839
      1996-06-30     0.9420            NA      1.0470
      1996-07-31     0.9628            NA      1.0585
      1996-08-31     0.9801     0.9792      1.0845
      1996-09-30     0.9415     1.0199      1.0937
      1996-10-31     0.9426     1.0260      1.1064
      1996-11-30     0.8900     1.0242      1.0971
      1996-12-31     0.9240     1.0761      1.1432
      1997-01-31     0.8881     1.0932      1.1589
      1997-02-28     0.8832     1.0758      1.1070
      1997-03-31     0.9297     1.0917      1.1156
      1997-04-30     0.8883     1.0240      1.0829
      1997-05-31     0.8740     1.0172      1.0982
      1997-06-30     0.8559     1.0273      1.0568
      1997-07-31     0.8050     1.0610      1.0847
      1997-08-31     0.8729     1.1018      1.1458
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for SFM.alpha

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > SFM.alpha(managers[, "HAM1"], managers[, "SP500 TR"], 
      +     Rf = managers[, "US 3m TR"])
      [1] 0.0057
      
      > SFM.alpha(managers[, 1:3], managers[, 8:10], Rf = 0.035/12)
           Alpha : SP500 TR Alpha : US 10Y TR Alpha : US 3m TR
      HAM1            0.006             0.009            0.008
      HAM2            0.009             0.011            0.010
      HAM3            0.006             0.010            0.008
      
      > if (requireNamespace("RobStatTM", quietly = TRUE)) {
      +     alphas <- SFM.alpha(managers[, 1:6], managers[, 8:10], Rf = 0.035/12, 
      +         method = "Robust", family = "opt", bb = 0.25, max.it = 200, 
      +         digits = 4)
      +     alphas["HAM1", ]
      +     alphas[, "Alpha : SP500 TR"]
      + }
        HAM1   HAM2   HAM3   HAM4   HAM5   HAM6 
      0.0059 0.0053 0.0048 0.0029 0.0044 0.0075 
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for SFM.beta

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > SFM.beta(managers[, "HAM1"], managers[, "SP500 TR"], 
      +     Rf = managers[, "US 3m TR"])
      [1] 0.3900
      
      > SFM.beta(managers[, 1:3], managers[, 8:10], Rf = 0.035/12)
           Beta : SP500 TR Beta : US 10Y TR Beta : US 3m TR
      HAM1           0.391           -0.359           0.692
      HAM2           0.343           -0.043           4.159
      HAM3           0.557           -0.099           4.687
      
      > SFM.beta(managers[, 1], managers[, 8:10], Rf = 0.035/12, 
      +     benchmarkCols = FALSE)
                         HAM1
      Beta : SP500 TR   0.391
      Beta : US 10Y TR -0.359
      Beta : US 3m TR   0.692
      
      > SFM.beta.bull(managers[, "HAM2"], managers[, "SP500 TR"], 
      +     Rf = managers[, "US 3m TR"])
      [1] 0.5226
      
      > SFM.beta.bear(managers[, "HAM2"], managers[, "SP500 TR"], 
      +     Rf = managers[, "US 3m TR"])
      [1] 0.0698
      
      > TimingRatio(managers[, "HAM2"], managers[, "SP500 TR"], 
      +     Rf = managers[, "US 3m TR"])
      [1] 7.4852
      
      > if (requireNamespace("RobStatTM", quietly = TRUE)) {
      +     betas <- SFM.beta(managers[, 1:6], managers[, 8:10], Rf = 0.035/12, 
      +         method = "Robust", family = "opt", bb = 0.25, max.it = 200, 
      +         digits = 4)
      +     betas["HAM1", ]
      +     betas[, "Beta : SP500 TR"]
      +     SFM.beta.bull(managers[, "HAM2"], managers[, "SP500 TR"], 
      +         Rf = managers[, "US 3m TR"], method = "Robust")
      +     SFM.beta.bear(managers[, "HAM2"], managers[, "SP500 TR"], 
      +         Rf = managers[, "US 3m TR"], method = "Robust")
      +     TimingRatio(managers[, "HAM2"], managers[, "SP500 TR"], Rf = managers[, 
      +         "US 3m TR"], method = "Robust", family = "mopt")
      + }
      [1] 19.2649
      
      > chart.Regression(managers[, "HAM2"], managers[, "SP500 TR"], 
      +     Rf = managers[, "US 3m TR"], fit = "conditional", main = "Conditional Beta")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for SFM.coefficients

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > if (requireNamespace("RobStatTM", quietly = TRUE)) {
      +     data(managers)
      +     SFM.coefficients(managers[, 1], managers[, 8])
      +     SFM.coefficients(managers[, 1:6], managers[, 8:9], Rf = managers[, 
      +         10])
      +     SFM.coefficients(managers[, 1:6], managers[, 8:9], Rf = 0.035/12, 
      +         method = "Robust", family = "mopt", bb = 0.25, max.it = 200)
      + }
           Alpha : SP500 TR Alpha : US 10Y TR Beta : SP500 TR Beta : US 10Y TR
      HAM1            0.006             0.010           0.357           -0.238
      HAM2            0.005             0.005           0.290           -0.126
      HAM3            0.005             0.007           0.599           -0.203
      HAM4            0.003             0.010           0.709           -0.247
      HAM5            0.004             0.002           0.296            0.056
      HAM6            0.007             0.009           0.333           -0.339
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for Selectivity

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(Selectivity(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2]))
      [1] -0.0141
      
      > data(managers)
      
      > print(Selectivity(managers["2002", 1], managers["2002", 
      +     8]))
      [1] 0.0378
      
      > print(Selectivity(managers["2002", 1:5], managers["2002", 
      +     8]))
                                            HAM1       HAM2        HAM3       HAM4
      Jensen's Alpha (Risk free = 0%) 0.0378 -0.1015 -0.0941 0.0943
                                             HAM5
      Jensen's Alpha (Risk free = 0%) -0.0866
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for SharpeRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > SharpeRatio(managers[, 1, drop = FALSE], Rf = 0.035/12, 
      +     FUN = "StdDev")
                                          HAM1
      Sharpe Ratio (Rf=0.3%, p=95%): 0.3201
      
      > SharpeRatio(managers[, 1, drop = FALSE], Rf = managers[, 
      +     10, drop = FALSE], FUN = "StdDev")
                                          HAM1
      Sharpe Ratio (Rf=0.3%, p=95%): 0.3083
      
      > SharpeRatio(managers[, 1:6], Rf = 0.035/12, FUN = "StdDev")
                                          HAM1      HAM2      HAM3      HAM4
      Sharpe Ratio (Rf=0.3%, p=95%): 0.3201 0.3057 0.2610 0.1522
                                           HAM5      HAM6
      Sharpe Ratio (Rf=0.3%, p=95%): 0.0256 0.3417
      
      > SharpeRatio(managers[, 1:6], Rf = managers[, 10, drop = FALSE], 
      +     FUN = "StdDev")
                                          HAM1      HAM2      HAM3      HAM4
      Sharpe Ratio (Rf=0.3%, p=95%): 0.3083 0.3007 0.2543 0.1461
                                           HAM5      HAM6
      Sharpe Ratio (Rf=0.3%, p=95%): 0.0354 0.3790
      
      > data(edhec)
      
      > SharpeRatio(edhec[, 6, drop = FALSE], FUN = "VaR")
                                       Event Driven
      VaR Sharpe Ratio (Rf=0%, p=95%):    0.2254
      
      > SharpeRatio(edhec[, 6, drop = FALSE], Rf = 0.04/12, 
      +     FUN = "VaR")
                                         Event Driven
      VaR Sharpe Ratio (Rf=0.3%, p=95%):    0.1014
      
      > SharpeRatio(edhec[, 6, drop = FALSE], Rf = 0.04/12, 
      +     FUN = "VaR", method = "gaussian")
                                         Event Driven
      VaR Sharpe Ratio (Rf=0.3%, p=95%):    0.1194
      
      > SharpeRatio(edhec[, 6, drop = FALSE], FUN = "ES")
                                      Event Driven
      ES Sharpe Ratio (Rf=0%, p=95%):   0.0822
      
      > SharpeRatio(managers[, 1:9], Rf = managers[, 10, drop = FALSE], 
      +     FUN = c("StdDev", "VaR", "ES"))
                                              HAM1      HAM2      HAM3       HAM4
      Sharpe Ratio (Rf=0.3%, p=95%):     0.3083 0.3007 0.2543 0.1461
      VaR Sharpe Ratio (Rf=0.3%, p=95%): 0.2121 0.3534 0.2306 0.0918
      ES Sharpe Ratio (Rf=0.3%, p=95%):  0.1231 0.1780 0.1949 0.0643
                                               HAM5      HAM6 EDHEC LS EQ   SP500 TR
      Sharpe Ratio (Rf=0.3%, p=95%):     0.0354 0.3790   0.3159 0.1257
      VaR Sharpe Ratio (Rf=0.3%, p=95%): 0.0228 0.2842   0.2397 0.0758
      ES Sharpe Ratio (Rf=0.3%, p=95%):  0.0158 0.2188   0.1670 0.0554
                                          US 10Y TR
      Sharpe Ratio (Rf=0.3%, p=95%):     0.0570
      VaR Sharpe Ratio (Rf=0.3%, p=95%): 0.0344
      ES Sharpe Ratio (Rf=0.3%, p=95%):  0.0251
      
      > SharpeRatio(edhec, Rf = 0.04/12, FUN = c("StdDev", 
      +     "VaR", "ES"))
                                         Convertible Arbitrage CTA Global
      Sharpe Ratio (Rf=0.3%, p=95%):                0.1466 0.0431
      VaR Sharpe Ratio (Rf=0.3%, p=95%):            0.0847 0.0278
      ES Sharpe Ratio (Rf=0.3%, p=95%):             0.0265 0.0225
                                         Distressed Securities Emerging Markets
      Sharpe Ratio (Rf=0.3%, p=95%):                0.1924       0.1038
      VaR Sharpe Ratio (Rf=0.3%, p=95%):            0.1114       0.0598
      ES Sharpe Ratio (Rf=0.3%, p=95%):             0.0494       0.0285
                                         Equity Market Neutral Event Driven
      Sharpe Ratio (Rf=0.3%, p=95%):                0.1220   0.1751
      VaR Sharpe Ratio (Rf=0.3%, p=95%):            0.0699   0.1014
      ES Sharpe Ratio (Rf=0.3%, p=95%):             0.0250   0.0395
                                         Fixed Income Arbitrage Global Macro
      Sharpe Ratio (Rf=0.3%, p=95%):                 0.0957    0.1548
      VaR Sharpe Ratio (Rf=0.3%, p=95%):             0.0520    0.1321
      ES Sharpe Ratio (Rf=0.3%, p=95%):              0.0194    0.1115
                                         Long/Short Equity Merger Arbitrage
      Sharpe Ratio (Rf=0.3%, p=95%):            0.1618       0.1958
      VaR Sharpe Ratio (Rf=0.3%, p=95%):        0.1030       0.1224
      ES Sharpe Ratio (Rf=0.3%, p=95%):         0.0651       0.0411
                                         Relative Value Short Selling Funds of Funds
      Sharpe Ratio (Rf=0.3%, p=95%):         0.2017   -0.1009     0.0732
      VaR Sharpe Ratio (Rf=0.3%, p=95%):     0.1156   -0.0701     0.0445
      ES Sharpe Ratio (Rf=0.3%, p=95%):      0.0471   -0.0648     0.0239
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for SharpeRatio.annualized

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > SharpeRatio.annualized(managers[, 1, drop = FALSE], 
      +     Rf = 0.035/12)
                                                    HAM1
      Annualized Sharpe Ratio (Rf=3.5%, p=95%): 1.1091
      
      > SharpeRatio.annualized(managers[, 1, drop = FALSE], 
      +     Rf = managers[, 10, drop = FALSE])
                                                    HAM1
      Annualized Sharpe Ratio (Rf=3.9%, p=95%): 1.0679
      
      > SharpeRatio.annualized(managers[, 1:6], Rf = 0.035/12)
                                                    HAM1     HAM2      HAM3      HAM4
      Annualized Sharpe Ratio (Rf=3.5%, p=95%): 1.1091 1.0592 0.9041 0.5274
                                                     HAM5     HAM6
      Annualized Sharpe Ratio (Rf=3.5%, p=95%): 0.0887 1.1838
      
      > SharpeRatio.annualized(managers[, 1:6], Rf = managers[, 
      +     10, drop = FALSE])
                                                    HAM1     HAM2      HAM3      HAM4
      Annualized Sharpe Ratio (Rf=3.9%, p=95%): 1.0679 1.0417 0.8809 0.5063
                                                     HAM5     HAM6
      Annualized Sharpe Ratio (Rf=3.9%, p=95%): 0.1226 1.3132
      
      > SharpeRatio.annualized(managers[, 1:6], Rf = managers[, 
      +     10, drop = FALSE], geometric = FALSE)
                                                    HAM1     HAM2      HAM3      HAM4
      Annualized Sharpe Ratio (Rf=3.9%, p=95%): 1.0679 1.0417 0.8809 0.5063
                                                     HAM5     HAM6
      Annualized Sharpe Ratio (Rf=3.9%, p=95%): 0.1226 1.3132
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for ShrinkageMoments

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > f <- rowSums(edhec)
      
      > targets <- c(1, 3, 4)
      
      > sigma <- M2.shrink(edhec, targets, f)$M2sh
      
      > m3 <- M3.shrink(edhec, targets, f)$M3sh
      
      > m4 <- M4.shrink(edhec, targets, f)$M4sh
      
      > mu <- colMeans(edhec)
      
      > p <- length(mu)
      
      > ES(p = 0.95, portfolio_method = "component", weights = rep(1/p, 
      +     p), mu = mu, sigma = sigma, m3 = m3, m4 = m4)
      $MES
      [1] 0.0360
      
      $contribution
       [1]  0.0047 -0.0004  0.0045  0.0065  0.0011
       [6]  0.0049  0.0028  0.0011  0.0037  0.0026
      [11]  0.0031 -0.0024  0.0034
      
      $pct_contrib_MES
       [1]  0.1311 -0.0126  0.1256  0.1824  0.0310  0.1379
       [7]  0.0789  0.0330  0.1048  0.0737  0.0873 -0.0679
      [13]  0.0943
      
      
      > sigma <- cov(edhec)
      
      > m3 <- M3.MM(edhec)
      
      > m4 <- M4.MM(edhec)
      
      > ES(p = 0.95, portfolio_method = "component", weights = rep(1/p, 
      +     p), mu = mu, sigma = sigma, m3 = m3, m4 = m4)
      $MES
      [1] 0.0362
      
      $contribution
       [1]  0.0059 -0.0028  0.0052  0.0062  0.0009
       [6]  0.0058  0.0034  0.0001  0.0036  0.0034
      [11]  0.0037 -0.0031  0.0034
      
      $pct_contrib_MES
       [1]  0.1648 -0.0774  0.1456  0.1724  0.0250
       [6]  0.1626  0.0955  0.0036  0.1000  0.0954
      [11]  0.1042 -0.0869  0.0948
      
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for SkewnessKurtosisRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(SkewnessKurtosisRatio(portfolio_bacon[, 1]))
      [1] -0.0339
      
      > data(managers)
      
      > print(SkewnessKurtosisRatio(managers["1996"]))
                                  HAM1      HAM2       HAM3       HAM4 HAM5 HAM6
      SkewnessKurtosisRatio -0.1364 0.1279 -0.3322 -0.0264   NA   NA
                            EDHEC LS EQ    SP500 TR   US 10Y TR   US 3m TR
      SkewnessKurtosisRatio          NA -0.0398 -0.0163 -0.2626
      
      > print(SkewnessKurtosisRatio(managers["1996", 1]))
      [1] -0.1364
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for SmoothingIndex

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > data(edhec)
      
      > SmoothingIndex(managers[, 1, drop = FALSE])
           HAM1 
      0.7017 
      
      > SmoothingIndex(managers[, 1:8])
                           HAM1      HAM2    HAM3      HAM4 HAM5      HAM6
      Smoothing Index 0.7017 0.5337 0.7463 0.7170    1 0.5791
                      EDHEC LS EQ SP500 TR
      Smoothing Index   0.6467        1
      
      > SmoothingIndex(edhec)
                      Convertible Arbitrage CTA Global Distressed Securities
      Smoothing Index             0.4468          1             0.4787
                      Emerging Markets Equity Market Neutral Event Driven
      Smoothing Index        0.5857             0.5508    0.5932
                      Fixed Income Arbitrage Global Macro Long/Short Equity
      Smoothing Index              0.4574    0.8762         0.6655
                      Merger Arbitrage Relative Value Short Selling Funds of Funds
      Smoothing Index        0.6334      0.5199     0.7541      0.5841
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for SortinoRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > round(SortinoRatio(managers[, 1]), 4)
                                 HAM1
      Sortino Ratio (MAR = 0%) 0.7649
      
      > round(SortinoRatio(managers[, 1:8]), 4)
                                 HAM1  HAM2   HAM3   HAM4   HAM5   HAM6 EDHEC LS EQ
      Sortino Ratio (MAR = 0%) 0.7649 1.222 0.7172 0.3234 0.1343 0.9102      0.9691
                               SP500 TR
      Sortino Ratio (MAR = 0%)   0.3064
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for SpecificRisk

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(SpecificRisk(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2]))
      [1] 0.0329
      
      > data(managers)
      
      > print(SpecificRisk(managers["1996", 1], managers["1996", 
      +     8]))
      [1] 0.0497
      
      > print(SpecificRisk(managers["1996", 1:5], managers["1996", 
      +     8]))
                             HAM1 HAM2       HAM3      HAM4 HAM5
      Specific Risk =  0.0497   NA 0.0622 0.0857   NA
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for StdDev

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > StdDev(edhec)
             Convertible Arbitrage CTA Global Distressed Securities Emerging Markets
      StdDev            0.0167 0.0227            0.0181       0.0327
             Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      StdDev           0.0082   0.0190             0.0114   0.0146
             Long/Short Equity Merger Arbitrage Relative Value Short Selling
      StdDev        0.0209       0.0114     0.0118    0.0455
             Funds of Funds
      StdDev     0.0160
      
      > StdDev(edhec, portfolio_method = "single")
             Convertible Arbitrage CTA Global Distressed Securities Emerging Markets
      StdDev            0.0167 0.0227            0.0181       0.0327
             Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      StdDev           0.0082   0.0190             0.0114   0.0146
             Long/Short Equity Merger Arbitrage Relative Value Short Selling
      StdDev        0.0209       0.0114     0.0118    0.0455
             Funds of Funds
      StdDev     0.0160
      
      > StdDev(edhec, clean = "boudt")
             Convertible Arbitrage CTA Global Distressed Securities Emerging Markets
      StdDev            0.0139 0.0227            0.0164       0.0308
             Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      StdDev           0.0073   0.0175            0.0095   0.0143
             Long/Short Equity Merger Arbitrage Relative Value Short Selling
      StdDev         0.0205       0.0103     0.0105     0.0435
             Funds of Funds
      StdDev     0.0158
      
      > StdDev(edhec, clean = "boudt", portfolio_method = "component")
    Message
      no weights passed in, assuming equal weighted portfolio
    Output
      $StdDev
      [1] 0.0101
      
      $contribution
       Convertible Arbitrage             CTA Global  Distressed Securities 
                0.0008           0.0006           0.0010 
            Emerging Markets  Equity Market Neutral           Event Driven 
                0.0019           0.0004           0.0011 
      Fixed Income Arbitrage           Global Macro      Long/Short Equity 
                0.0005           0.0009           0.0012 
            Merger Arbitrage         Relative Value          Short Selling 
                0.0005           0.0007          -0.0009 
              Funds of Funds 
                0.0010 
      
      $pct_contrib_StdDev
       Convertible Arbitrage             CTA Global  Distressed Securities 
                  0.0800             0.0638             0.1026 
            Emerging Markets  Equity Market Neutral           Event Driven 
                  0.1889             0.0420             0.1157 
      Fixed Income Arbitrage           Global Macro      Long/Short Equity 
                  0.0524             0.0886             0.1275 
            Merger Arbitrage         Relative Value          Short Selling 
                  0.0567             0.0692            -0.0952 
              Funds of Funds 
                  0.1072 
      
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for StdDev.annualized

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > sd.annualized(edhec)
                                    Convertible Arbitrage CTA Global
      Annualized Standard Deviation              0.0580 0.0789
                                    Distressed Securities Emerging Markets
      Annualized Standard Deviation            0.0628        0.1133
                                    Equity Market Neutral Event Driven
      Annualized Standard Deviation            0.0284   0.0660
                                    Fixed Income Arbitrage Global Macro
      Annualized Standard Deviation             0.0396   0.0506
                                    Long/Short Equity Merger Arbitrage Relative Value
      Annualized Standard Deviation        0.0724       0.0397     0.0411
                                    Short Selling Funds of Funds
      Annualized Standard Deviation     0.1576     0.0557
      
      > sd.annualized(edhec[, 6, drop = FALSE])
                                    Event Driven
      Annualized Standard Deviation   0.0660
      
      > sd.multiperiod(edhec[, 6, drop = FALSE], scale = 3)
                                    Event Driven
      Annualized Standard Deviation   0.0330
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for StructuredMoments

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > sigma <- M2.struct(edhec, "CC")
      
      > m3 <- M3.struct(edhec, "CC")
      
      > m4 <- M4.struct(edhec, "CC")
      
      > mu <- colMeans(edhec)
      
      > p <- length(mu)
      
      > ES(p = 0.95, portfolio_method = "component", weights = rep(1/p, 
      +     p), mu = mu, sigma = sigma, m3 = m3, m4 = m4)
      $MES
      [1] 0.0066
      
      $contribution
       [1] -2.8380e-4  1.0721e-3 -5.2846e-6  2.0690e-3 -4.8454e-4
       [6]  2.4443e-5 -5.7940e-4 -1.6396e-5  5.2976e-4 -4.9105e-4
      [11] -4.3030e-4  5.1516e-3  1.0473e-4
      
      $pct_contrib_MES
       [1] -0.0426  0.1609 -0.0007  0.3106 -0.0727
       [6]  0.0036 -0.0869 -0.0024  0.0795 -0.0737
      [11] -0.0646  0.7734  0.0157
      
      
      > sigma <- cov(edhec)
      
      > m3 <- M3.MM(edhec)
      
      > m4 <- M4.MM(edhec)
      
      > ES(p = 0.95, portfolio_method = "component", weights = rep(1/p, 
      +     p), mu = mu, sigma = sigma, m3 = m3, m4 = m4)
      $MES
      [1] 0.0362
      
      $contribution
       [1]  0.0059 -0.0028  0.0052  0.0062  0.0009
       [6]  0.0058  0.0034  0.0001  0.0036  0.0034
      [11]  0.0037 -0.0031  0.0034
      
      $pct_contrib_MES
       [1]  0.1648 -0.0774  0.1456  0.1724  0.0250
       [6]  0.1626  0.0955  0.0036  0.1000  0.0954
      [11]  0.1042 -0.0869  0.0948
      
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for SystematicRisk

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(SystematicRisk(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2]))
      [1] 0.1328
      
      > data(managers)
      
      > print(SystematicRisk(managers["2002", 1], managers["2002", 
      +     8]))
      [1] 0.1103
      
      > print(SystematicRisk(managers["2002", 1:5], managers["2002", 
      +     8]))
                                                HAM1       HAM2       HAM3      HAM4
      Systematic Risk to SP500 TR (Rf = 0) 0.1103 0.0204 0.0893 0.1651
                                                 HAM5
      Systematic Risk to SP500 TR (Rf = 0) 0.0201
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for TotalRisk

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > print(TotalRisk(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2]))
      [1] 0.1368
      
      > data(managers)
      
      > print(TotalRisk(managers["1996", 1], managers["1996", 
      +     8]))
      [1] 0.0562
      
      > print(TotalRisk(managers["1996", 1:5], managers["1996", 
      +     8]))
                          HAM1 HAM2      HAM3      HAM4 HAM5
      Total Risk =  0.0562   NA 0.1079 0.1099   NA
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for TrackingError

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > TrackingError(managers[, 1, drop = FALSE], managers[, 
      +     8, drop = FALSE])
      [1] 0.1131
      
      > TrackingError(managers[, 1:6], managers[, 8, drop = FALSE])
                                    HAM1      HAM2      HAM3      HAM4      HAM5
      Tracking Error: SP500 TR 0.1131 0.1533 0.1158 0.1596 0.1800
                                   HAM6
      Tracking Error: SP500 TR 0.1128
      
      > TrackingError(managers[, 1:6], managers[, 8:7, drop = FALSE])
                                        HAM1       HAM2       HAM3      HAM4
      Tracking Error: SP500 TR    0.1131 0.1533 0.1158 0.1596
      Tracking Error: EDHEC LS EQ 0.0757 0.0907 0.0815 0.1569
                                       HAM5       HAM6
      Tracking Error: SP500 TR    0.1800 0.1128
      Tracking Error: EDHEC LS EQ 0.1421 0.0565
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for TreynorRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > data(managers)
      
      > round(TreynorRatio(managers[, 1], managers[, 8], Rf = 0.035/12), 
      +     4)
      [1] 0.2528
      
      > round(TreynorRatio(managers[, 1], managers[, 8], Rf = managers[, 
      +     10]), 4)
      [1] 0.2428
      
      > round(TreynorRatio(managers[, 1:6], managers[, 8], 
      +     Rf = 0.035/12), 4)
                                HAM1   HAM2  HAM3   HAM4   HAM5   HAM6
      Treynor Ratio: SP500 TR 0.2528 0.3925 0.201 0.1209 0.0052 0.3042
      
      > round(TreynorRatio(managers[, 1:6], managers[, 8], 
      +     Rf = managers[, 10]), 4)
                                HAM1   HAM2   HAM3   HAM4   HAM5   HAM6
      Treynor Ratio: SP500 TR 0.2428 0.3883 0.1956 0.1144 0.0219 0.3401
      
      > round(TreynorRatio(managers[, 1:6], managers[, 8:7], 
      +     Rf = 0.035/12), 4)
                                   HAM1   HAM2   HAM3   HAM4   HAM5   HAM6
      Treynor Ratio: SP500 TR    0.2528 0.3925 0.2010 0.1209 0.0052 0.3042
      Treynor Ratio: EDHEC LS EQ 0.1297 0.1088 0.0776 0.0504 0.0014 0.0966
      
      > round(TreynorRatio(managers[, 1:6], managers[, 8:7], 
      +     Rf = managers[, 10]), 4)
                                   HAM1   HAM2   HAM3   HAM4   HAM5   HAM6
      Treynor Ratio: SP500 TR    0.2428 0.3883 0.1956 0.1144 0.0219 0.3401
      Treynor Ratio: EDHEC LS EQ 0.1242 0.1068 0.0753 0.0471 0.0060 0.1086
      
      > print(TreynorRatio(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2], modified = TRUE))
      [1] 0.7806
      
      > print(TreynorRatio(managers["2002", 1], managers["2002", 
      +     8], modified = TRUE))
      [1] -0.7275
      
      > print(TreynorRatio(managers["2002", 1:5], managers["2002", 
      +     8], modified = TRUE))
                                   HAM1      HAM2      HAM3       HAM4     HAM5
      Treynor Ratio: SP500 TR -0.7275 -6.0451 -2.1238 -0.4990 -5.3748
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for UpDownRatios

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > UpDownRatios(managers[, 1, drop = FALSE], managers[, 
      +     8, drop = FALSE])
                                 HAM1
      SP500 TR Up Capture   0.3215
      SP500 TR Down Capture 0.3770
      SP500 TR Up Number    0.8941
      SP500 TR Down Number  0.5106
      SP500 TR Up Percent   0.2941
      SP500 TR Down Percent 0.8085
      
      > UpDownRatios(managers[, 1:6, drop = FALSE], managers[, 
      +     8, drop = FALSE])
                                 HAM1      HAM2      HAM3      HAM4      HAM5
      SP500 TR Up Capture   0.3215 0.3462 0.5261 0.7675 0.3678
      SP500 TR Down Capture 0.3770 0.1090 0.5866 0.8732 0.4979
      SP500 TR Up Number    0.8941 0.6835 0.8705 0.7647 0.7083
      SP500 TR Down Number  0.5106 0.6956 0.7659 0.6595 0.7241
      SP500 TR Up Percent   0.2941 0.3670 0.4235 0.4588 0.3958
      SP500 TR Down Percent 0.8085 0.8695 0.8085 0.5319 0.8275
                                 HAM6
      SP500 TR Up Capture   0.7182
      SP500 TR Down Capture 0.3113
      SP500 TR Up Number    0.8372
      SP500 TR Down Number  0.5238
      SP500 TR Up Percent   0.5348
      SP500 TR Down Percent 0.7142
      
      > UpDownRatios(managers[, 1, drop = FALSE], managers[, 
      +     8, drop = FALSE], method = "Capture")
                                 HAM1
      SP500 TR Up Capture   0.3215
      SP500 TR Down Capture 0.3770
      
      > UpDownRatios(managers[, 1, drop = FALSE], managers[, 
      +     8, drop = FALSE], side = "Up", method = "Capture")
      [1] 0.3215
      
      > UpDownRatios(managers[, 1, drop = FALSE], managers[, 
      +     8, drop = FALSE], side = "Down", method = "Capture")
      [1] 0.3770
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for UpsideFrequency

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > MAR = 0.005
      
      > print(UpsideFrequency(portfolio_bacon[, 1], MAR))
      [1] 0.5416
      
      > data(managers)
      
      > print(UpsideFrequency(managers["1996"]))
                                  HAM1 HAM2      HAM3      HAM4 HAM5 HAM6 EDHEC LS EQ
      Upside Frequency (MAR = 0%) 0.75  0.8 0.8333 0.6666  NaN  NaN         NaN
                                   SP500 TR US 10Y TR US 3m TR
      Upside Frequency (MAR = 0%) 0.8333 0.4166        1
      
      > print(UpsideFrequency(managers["1996", 1]))
      [1] 0.75
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for UpsidePotentialRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > UpsidePotentialRatio(edhec[, 6], MAR = 0.05/12)
                                    Event Driven
      Upside Potential (MAR = 0.4%)    0.5448
      
      > UpsidePotentialRatio(edhec[, 1:6], MAR = 0)
                                  Convertible Arbitrage CTA Global
      Upside Potential (MAR = 0%)             0.4988   1.0552
                                  Distressed Securities Emerging Markets
      Upside Potential (MAR = 0%)             0.6986        0.6074
                                  Equity Market Neutral Event Driven
      Upside Potential (MAR = 0%)             0.6077    0.5938
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for UpsideRisk

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > MAR = 0.005
      
      > print(UpsideRisk(portfolio_bacon[, 1], MAR, stat = "risk"))
      [1] 0.0293
      
      > print(UpsideRisk(portfolio_bacon[, 1], MAR, stat = "variance"))
      [1] 0.0008
      
      > print(UpsideRisk(portfolio_bacon[, 1], MAR, stat = "potential"))
      [1] 0.0177
      
      > MAR = 0
      
      > data(managers)
      
      > print(UpsideRisk(managers["1996"], MAR, stat = "risk"))
                                   HAM1       HAM2       HAM3      HAM4 HAM5 HAM6
      Upside Risk (MAR = 0%) 0.0179 0.0591 0.0400 0.0321    0    0
                             EDHEC LS EQ   SP500 TR  US 10Y TR    US 3m TR
      Upside Risk (MAR = 0%)           0 0.0320 0.0139 0.0043
      
      > print(UpsideRisk(managers["1996", 1], MAR, stat = "risk"))
      [1] 0.0179
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for VaR

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > VaR(edhec, p = 0.95, method = "historical")
          Convertible Arbitrage CTA Global Distressed Securities Emerging Markets
      VaR              -0.0150   -0.0314              -0.0197         -0.0423
          Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      VaR              -0.0083     -0.0255               -0.0072     -0.0149
          Long/Short Equity Merger Arbitrage Relative Value Short Selling
      VaR          -0.0262         -0.0106       -0.0114      -0.0667
          Funds of Funds
      VaR       -0.0203
      
      > VaR(edhec, p = 0.95, method = "gaussian")
          Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      VaR           -0.0217 -0.0331           -0.0229      -0.0469
          Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      VaR          -0.0091  -0.0246            -0.0143  -0.0184
          Long/Short Equity Merger Arbitrage Relative Value Short Selling
      VaR       -0.0276      -0.0132    -0.0137   -0.0759
          Funds of Funds
      VaR    -0.0219
      
      > VaR(edhec, p = 0.95, method = "modified")
          Convertible Arbitrage CTA Global Distressed Securities Emerging Markets
      VaR           -0.0256 -0.0320           -0.0280      -0.0534
          Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      VaR            -0.0109  -0.0296            -0.0177  -0.0138
          Long/Short Equity Merger Arbitrage Relative Value Short Selling
      VaR       -0.0295      -0.0150    -0.0173   -0.0621
          Funds of Funds
      VaR    -0.0230
      
      > VaR(edhec, p = 0.99)
          Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      VaR           -0.0953 -0.0456           -0.0709       -0.1261
          Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      VaR           -0.0387  -0.0843            -0.0603  -0.0230
          Long/Short Equity Merger Arbitrage Relative Value Short Selling
      VaR       -0.0565      -0.0576    -0.0488    -0.1093
          Funds of Funds
      VaR    -0.0542
      
      > VaR(edhec, p = 0.01)
          Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      VaR           -0.0953 -0.0456           -0.0709       -0.1261
          Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      VaR           -0.0387  -0.0843            -0.0603  -0.0230
          Long/Short Equity Merger Arbitrage Relative Value Short Selling
      VaR       -0.0565      -0.0576    -0.0488    -0.1093
          Funds of Funds
      VaR    -0.0542
      
      > VaR(edhec, clean = "boudt")
          Convertible Arbitrage CTA Global Distressed Securities Emerging Markets
      VaR           -0.0171 -0.0320            -0.0227      -0.0468
          Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      VaR          -0.0084  -0.0248            -0.0135   -0.0146
          Long/Short Equity Merger Arbitrage Relative Value Short Selling
      VaR       -0.0281      -0.0111    -0.0134   -0.0682
          Funds of Funds
      VaR    -0.0223
      
      > VaR(edhec, clean = "boudt", portfolio_method = "component")
    Message
      no weights passed in, assuming equal weighted portfolio
    Output
      $MVaR
      [1] 0.0124
      
      $contribution
       Convertible Arbitrage             CTA Global  Distressed Securities 
                0.0011           0.0003           0.0014 
            Emerging Markets  Equity Market Neutral           Event Driven 
                0.0028           0.0004           0.0016 
      Fixed Income Arbitrage           Global Macro      Long/Short Equity 
                0.0007           0.0008           0.0017 
            Merger Arbitrage         Relative Value          Short Selling 
                0.0005           0.0009          -0.0020 
              Funds of Funds 
                0.0016 
      
      $pct_contrib_MVaR
       Convertible Arbitrage             CTA Global  Distressed Securities 
                  0.0893             0.0321             0.1172 
            Emerging Markets  Equity Market Neutral           Event Driven 
                  0.2308             0.0359             0.1341 
      Fixed Income Arbitrage           Global Macro      Long/Short Equity 
                  0.0622             0.0722             0.1402 
            Merger Arbitrage         Relative Value          Short Selling 
                  0.0475             0.0742            -0.1672 
              Funds of Funds 
                  0.1308 
      
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for VaR.backtest

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > v <- as.numeric(VaR(edhec[, 1], p = 0.95, method = "historical"))
      
      > VaR.backtest(edhec[, 1], v, p = 0.95)
      $expected_exceedances
      [1] 14.65
      
      $actual_exceedances
      [1] 15
      
      $p.value
      [1] 1
      
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for VolatilitySkewness

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > MAR = 0.005
      
      > print(VolatilitySkewness(portfolio_bacon[, 1], MAR, 
      +     stat = "volatility"))
               [,1]
      [1,] 1.3230
      
      > print(VolatilitySkewness(portfolio_bacon[, 1], MAR, 
      +     stat = "variability"))
               [,1]
      [1,] 1.1502
      
      > MAR = 0
      
      > data(managers)
      
      > print(VolatilitySkewness(managers["1996", 1], MAR, 
      +     stat = "volatility"))
               [,1]
      [1,] 6.1494
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for apply.fromstart

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > apply.fromstart(managers[, 1, drop = FALSE], FUN = "mean", 
      +     width = 36)
       1996-01-31  1996-02-29  1996-03-31  1996-04-30  1996-05-31  1996-06-30 
      0.0074 0.0133 0.0140 0.0082 0.0081 0.0061 
       1996-07-31  1996-08-31  1996-09-30  1996-10-31  1996-11-30  1996-12-31 
      0.0019 0.0066 0.0075 0.0096 0.0102 0.0108 
       1997-01-31  1997-02-28  1997-03-31  1997-04-30  1997-05-31  1997-06-30 
      0.0116 0.0109 0.0108 0.0109 0.0128 0.0134 
       1997-07-31  1997-08-31  1997-09-30  1997-10-31  1997-11-30  1997-12-31 
      0.0135 0.0140 0.0144 0.0128 0.0133 0.0132 
       1998-01-31  1998-02-28  1998-03-31  1998-04-30  1998-05-31  1998-06-30 
      0.0129 0.0141 0.0149 0.0146 0.0133 0.0133 
       1998-07-31  1998-08-31  1998-09-30  1998-10-31  1998-11-30  1998-12-31 
      0.0122 0.0088 0.0093 0.0107 0.0107 0.0107 
       1999-01-31  1999-02-28  1999-03-31  1999-04-30  1999-05-31  1999-06-30 
      0.0102 0.0101 0.0111 0.0121 0.0122 0.0126 
       1999-07-31  1999-08-31  1999-09-30  1999-10-31  1999-11-30  1999-12-31 
      0.0126 0.0119 0.0115 0.0113 0.0111 0.0112 
       2000-01-31  2000-02-29  2000-03-31  2000-04-30  2000-05-31  2000-06-30 
      0.0108 0.0108 0.0117 0.0119 0.0123 0.0123 
       2000-07-31  2000-08-31  2000-09-30  2000-10-31  2000-11-30  2000-12-31 
      0.0121 0.0126 0.0124 0.0121 0.0120 0.0117 
       2001-01-31  2001-02-28  2001-03-31  2001-04-30  2001-05-31  2001-06-30 
      0.0117 0.0116 0.0113 0.0116 0.0123 0.0122 
       2001-07-31  2001-08-31  2001-09-30  2001-10-31  2001-11-30  2001-12-31 
      0.0123 0.0124 0.0117 0.0116 0.0119 0.0127 
       2002-01-31  2002-02-28  2002-03-31  2002-04-30  2002-05-31  2002-06-30 
      0.0127 0.0123 0.0122 0.0121 0.0120 0.0115 
       2002-07-31  2002-08-31  2002-09-30  2002-10-31  2002-11-30  2002-12-31 
      0.0104 0.0104 0.0095 0.0098 0.0105 0.0099 
       2003-01-31  2003-02-28  2003-03-31  2003-04-30  2003-05-31  2003-06-30 
      0.0093 0.0089 0.0093 0.0099 0.0102 0.0104 
       2003-07-31  2003-08-31  2003-09-30  2003-10-31  2003-11-30  2003-12-31 
      0.0105 0.0104 0.0103 0.0107 0.0108 0.0110 
       2004-01-31  2004-02-29  2004-03-31  2004-04-30  2004-05-31  2004-06-30 
      0.0109 0.0108 0.0108 0.0106 0.0106 0.0108 
       2004-07-31  2004-08-31  2004-09-30  2004-10-31  2004-11-30  2004-12-31 
      0.0107 0.0106 0.0106 0.0105 0.0107 0.0111 
       2005-01-31  2005-02-28  2005-03-31  2005-04-30  2005-05-31  2005-06-30 
      0.0110 0.0110 0.0108 0.0105 0.0104 0.0105 
       2005-07-31  2005-08-31  2005-09-30  2005-10-31  2005-11-30  2005-12-31 
      0.0105 0.0105 0.0106 0.0104 0.0105 0.0106 
       2006-01-31  2006-02-28  2006-03-31  2006-04-30  2006-05-31  2006-06-30 
      0.0111 0.0111 0.0113 0.0112 0.0109 0.0110 
       2006-07-31  2006-08-31  2006-09-30  2006-10-31  2006-11-30  2006-12-31 
      0.0108 0.0109 0.0108 0.0111 0.0111 0.0111 
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for apply.rolling

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > apply.rolling(managers[, 1, drop = FALSE], FUN = "mean", 
      +     width = 36)
                      calcs
      1996-01-31         NA
      1996-02-29         NA
      1996-03-31         NA
      1996-04-30         NA
      1996-05-31         NA
      1996-06-30         NA
      1996-07-31         NA
      1996-08-31         NA
      1996-09-30         NA
      1996-10-31         NA
             ...           
      2006-03-31 0.0164
      2006-04-30 0.0145
      2006-05-31 0.0128
      2006-06-30 0.0126
      2006-07-31 0.0117
      2006-08-31 0.0121
      2006-09-30 0.0121
      2006-10-31 0.0119
      2006-11-30 0.0118
      2006-12-31 0.0113
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for centeredmoments

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > Return.centered(managers[, 1:3, drop = FALSE])
                          HAM1       HAM2          HAM3
      1996-01-31 -0.0037         NA  0.0224
      1996-02-29  0.0081         NA  0.0226
      1996-03-31  0.0043         NA  0.0133
      1996-04-30 -0.0202         NA  0.0324
      1996-05-31 -0.0035         NA  0.0228
      1996-06-30 -0.0150         NA -0.0427
      1996-07-31 -0.0342         NA -0.0461
      1996-08-31  0.0283 -0.0142  0.0336
      1996-09-30  0.0035  0.0860  0.0528
      1996-10-31  0.0176  0.0196  0.0270
      1996-11-30  0.0044  0.0595  0.0541
      1996-12-31  0.0064  0.0156  0.0089
      1997-01-31  0.0100  0.0652  0.0646
      1997-02-28 -0.0089 -0.0223 -0.0498
      1997-03-31 -0.0017 -0.0410 -0.0460
      1997-04-30  0.0014 -0.0202  0.0161
      1997-05-31  0.0326  0.0397  0.0634
      1997-06-30  0.0119  0.0410 -0.0070
      1997-07-31  0.0042  0.1008  0.0956
      1997-08-31  0.0125 -0.0338 -0.0152
      1997-09-30  0.0107  0.0434  0.0424
      1997-10-31 -0.0318 -0.0363 -0.0478
      1997-11-30  0.0138 -0.0192  0.0051
      1997-12-31 -0.0001  0.0050 -0.0127
      1998-01-31 -0.0055 -0.0253  0.0366
      1998-02-28  0.0317  0.0865  0.0341
      1998-03-31  0.0250  0.0483  0.0083
      1998-04-30 -0.0033 -0.0151  0.0109
      1998-05-31 -0.0342 -0.0248 -0.0260
      1998-06-30  0.0009  0.0250  0.0270
      1998-07-31 -0.0326 -0.0413 -0.0119
      1998-08-31 -0.1055 -0.0141 -0.0842
      1998-09-30  0.0136 -0.0187  0.0540
      1998-10-31  0.0446  0.0207 -0.0175
      1998-11-30  0.0014  0.0557  0.0430
      1998-12-31 -0.0014  0.0771  0.0339
      1999-01-31 -0.0204  0.0645  0.0144
      1999-02-28 -0.0017 -0.0371 -0.0654
      1999-03-31  0.0350  0.0940  0.0062
      1999-04-30  0.0398  0.0024  0.0292
      1999-05-31  0.0050 -0.0143 -0.0045
      1999-06-30  0.0214  0.0508  0.0422
      1999-07-31 -0.0013  0.0137 -0.0082
      1999-08-31 -0.0276  0.0143 -0.0002
      1999-09-30 -0.0156  0.0190 -0.0056
      1999-10-31 -0.0117  0.0280  0.0622
      1999-11-30 -0.0076  0.0555  0.0949
      1999-12-31  0.0035  0.1307  0.0455
      2000-01-31 -0.0213  0.0236 -0.0174
      2000-02-29  0.0011  0.1414  0.1671
      2000-03-31  0.0463 -0.0504 -0.0096
      2000-04-30  0.0090 -0.0239 -0.0251
      2000-05-31  0.0227 -0.0258 -0.0352
      2000-06-30  0.0011 -0.0036  0.0282
      2000-07-31 -0.0062  0.0046 -0.0649
      2000-08-31  0.0275  0.0205  0.0670
      2000-09-30 -0.0098 -0.0438 -0.0697
      2000-10-31 -0.0188 -0.0429  0.0037
      2000-11-30 -0.0007 -0.0361 -0.0492
      2000-12-31 -0.0179  0.0048  0.0044
      2001-01-31 -0.0032 -0.0466  0.0137
      2001-02-28 -0.0029 -0.0334 -0.0479
      2001-03-31 -0.0216 -0.0173 -0.0234
      2001-04-30  0.0233 -0.0160  0.0007
      2001-05-31  0.0467 -0.0066 -0.0177
      2001-06-30 -0.0090 -0.0348 -0.0361
      2001-07-31  0.0095 -0.0165 -0.0146
      2001-08-31  0.0049 -0.0058 -0.0370
      2001-09-30 -0.0423  0.0191 -0.0192
      2001-10-31 -0.0100 -0.0361 -0.0329
      2001-11-30  0.0228 -0.0059  0.0123
      2001-12-31  0.0564 -0.0144 -0.0106
      2002-01-31  0.0023 -0.0329 -0.0269
      2002-02-28 -0.0235 -0.0512 -0.0538
      2002-03-31 -0.0048  0.0079  0.0082
      2002-04-30 -0.0065 -0.0257 -0.0377
      2002-05-31 -0.0126 -0.0436 -0.0148
      2002-06-30 -0.0352 -0.0479 -0.0575
      2002-07-31 -0.0866 -0.0265 -0.0464
      2002-08-31 -0.0034 -0.0145 -0.0100
      2002-09-30 -0.0686 -0.0158 -0.0563
      2002-10-31  0.0185 -0.0098 -0.0022
      2002-11-30  0.0549 -0.0224  0.0225
      2002-12-31 -0.0434 -0.0163 -0.0776
      2003-01-31 -0.0523 -0.0144 -0.0139
      2003-02-28 -0.0362 -0.0290 -0.0103
      2003-03-31  0.0252 -0.0197 -0.0013
      2003-04-30  0.0539 -0.0260  0.0400
      2003-05-31  0.0225  0.0373  0.0298
      2003-06-30  0.0196  0.0093 -0.0015
      2003-07-31  0.0064  0.0092 -0.0038
      2003-08-31 -0.0108 -0.0225  0.0177
      2003-09-30 -0.0021 -0.0194 -0.0149
      2003-10-31  0.0369 -0.0007  0.0480
      2003-11-30  0.0057 -0.0027 -0.0006
      2003-12-31  0.0163  0.0147 -0.0029
      2004-01-31 -0.0058  0.0049 -0.0070
      2004-02-29 -0.0112 -0.0054  0.0018
      2004-03-31 -0.0024 -0.0013 -0.0162
      2004-04-30 -0.0154 -0.0059 -0.0324
      2004-05-31 -0.0029 -0.0066 -0.0191
      2004-06-30  0.0147 -0.0003 -0.0114
      2004-07-31 -0.0111 -0.0099 -0.0471
      2004-08-31 -0.0056 -0.0206 -0.0180
      2004-09-30 -0.0023 -0.0041 -0.0156
      2004-10-31 -0.0117  0.0005 -0.0135
      2004-11-30  0.0282  0.0110  0.0429
      2004-12-31  0.0328 -0.0058  0.0056
      2005-01-31 -0.0110 -0.0195 -0.0051
      2005-02-28  0.0103  0.0068  0.0292
      2005-03-31 -0.0318 -0.0210 -0.0031
      2005-04-30 -0.0320 -0.0180 -0.0143
      2005-05-31 -0.0068 -0.0280  0.0108
      2005-06-30  0.0049  0.0052 -0.0089
      2005-07-31 -0.0020  0.0057  0.0078
      2005-08-31  0.0001 -0.0018 -0.0144
      2005-09-30  0.0149  0.0038  0.0072
      2005-10-31 -0.0298 -0.0327 -0.0235
      2005-11-30  0.0119 -0.0047  0.0037
      2005-12-31  0.0149 -0.0056  0.0056
      2006-01-31  0.0580  0.0670  0.0226
      2006-02-28  0.0033 -0.0467  0.0141
      2006-03-31  0.0285  0.0009 -0.0005
      2006-04-30 -0.0122  0.0030 -0.0093
      2006-05-31 -0.0378 -0.0193 -0.0336
      2006-06-30  0.0104 -0.0257 -0.0313
      2006-07-31 -0.0255 -0.0272 -0.0022
      2006-08-31  0.0049 -0.0254  0.0128
      2006-09-30 -0.0043 -0.0372 -0.0052
      2006-10-31  0.0315  0.0025  0.0058
      2006-11-30  0.0005  0.0064  0.0144
      2006-12-31  0.0003 -0.0203 -0.0014
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.ACF

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > chart.ACFplus(edhec[, 1, drop = FALSE])
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.Bar

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > chart.Bar(edhec[, "Funds of Funds"], main = "Monthly Returns")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.BarVaR

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.Boxplot

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > chart.Boxplot(edhec)
      
      > chart.Boxplot(edhec, as.Tufte = TRUE)
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.CaptureRatios

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > chart.CaptureRatios(managers[, 1:6], managers[, 7, 
      +     drop = FALSE])
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.Correlation

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > chart.Correlation(managers[, 1:8], histogram = TRUE, 
      +     pch = "+")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.CumReturns

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > chart.CumReturns(edhec[, "Funds of Funds"], main = "Cumulative Returns")
      
      > chart.CumReturns(edhec[, "Funds of Funds"], wealth.index = TRUE, 
      +     main = "Growth of $1")
      
      > data(managers)
      
      > chart.CumReturns(managers, main = "Cumulative Returns", 
      +     begin = "first")
      
      > chart.CumReturns(managers, main = "Cumulative Returns", 
      +     begin = "axis")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.Drawdown

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > chart.Drawdown(edhec[, c(1, 2)], main = "Drawdown from Peak Equity Attained", 
      +     legend.loc = "bottomleft")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.ECDF

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > chart.ECDF(edhec[, 1, drop = FALSE])
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.Events

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.Histogram

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > chart.Histogram(edhec[, "Equity Market Neutral", drop = FALSE])
      
      > chart.Histogram(edhec[, "Equity Market Neutral", drop = FALSE], 
      +     breaks = 40, methods = c("add.density", "add.rug"))
      
      > chart.Histogram(edhec[, "Equity Market Neutral", drop = FALSE], 
      +     methods = c("add.density", "add.normal"))
      
      > chart.Histogram(edhec[, "Equity Market Neutral", drop = FALSE], 
      +     methods = c("add.density", "add.centered"))
      
      > chart.Histogram(edhec[, "Equity Market Neutral", drop = FALSE], 
      +     methods = c("add.centered", "add.density", "add.rug"))
      
      > chart.Histogram(edhec[, "Equity Market Neutral", drop = FALSE], 
      +     methods = c("add.centered", "add.density", "add.rug", "add.qqplot"))
      
      > chart.Histogram(edhec[, "Equity Market Neutral", drop = FALSE], 
      +     methods = c("add.density", "add.centered", "add.rug", "add.risk"))
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.QQPlot

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.Regression

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > chart.Regression(managers[, 1:2, drop = FALSE], managers[, 
      +     8, drop = FALSE], Rf = managers[, 10, drop = FALSE], excess.returns = TRUE, 
      +     fit = c("loess", "linear"), legend.loc = "topleft")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.RelativePerformance

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > chart.RelativePerformance(managers[, 1:6, drop = FALSE], 
      +     managers[, 8, drop = FALSE], colorset = rich8equal, legend.loc = "bottomright", 
      +     main = "Relative Performance to S&P")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.RiskReturnScatter

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > chart.RiskReturnScatter(edhec, Rf = 0.04/12)
      
      > chart.RiskReturnScatter(edhec, Rf = 0.04/12, add.boxplots = TRUE)
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.RollingCorrelation

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > chart.RollingCorrelation(managers[, 1:6, drop = FALSE], 
      +     managers[, 8, drop = FALSE], colorset = rich8equal, legend.loc = "bottomright", 
      +     width = 24, main = "Rolling 12-Month Correlation")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.RollingMean

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > chart.RollingMean(edhec[, 9, drop = FALSE])
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.RollingPerformance

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > chart.RollingPerformance(edhec[, 1:3], width = 24)
      
      > chart.RollingPerformance(edhec[, 1:3], FUN = "mean", 
      +     width = 24, colorset = rich8equal, lwd = 2, legend.loc = "topleft", 
      +     main = "Rolling 24-Month Mean Return")
      
      > chart.RollingPerformance(edhec[, 1:3], FUN = "SharpeRatio.annualized", 
      +     width = 24, colorset = rich8equal, lwd = 2, legend.loc = "topleft", 
      +     main = "Rolling 24-Month Sharpe Ratio")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.SFM

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > mgrs <- managers["2002/"]
      
      > names(mgrs)[7:10] <- c("LSEQ", "SP500", "Bond10Yr", 
      +     "RF")
      
      > plot.zoo(mgrs)
      
      > chart.SFM(mgrs$HAM1, mgrs$SP500, Rf = mgrs$RF)
      
      > for (k in 1:7) {
      +     chart.SFM(mgrs[, k], mgrs$SP500, mgrs$RF, makePct = TRUE, 
      +         main = names(mgrs[, k]))
      + }
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.Scatter

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.SnailTrail

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > chart.SnailTrail(managers[, c("HAM2", "SP500 TR"), 
      +     drop = FALSE], width = 36, stepsize = 12, colorset = c("red", 
      +     "orange"), add.names = "firstandlast", rf = 0.04/12, main = "Trailing 36-month Performance Calc'd Every 12 Months")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.StackedBar

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(weights)
      
      > head(weights)
                 Convertible Arbitrage CTA Global Distressed Securities
      2000-01-01            0.0250 0.1460             0.0250
      2001-01-01            0.1578 0.1957             0.0250
      2002-01-01            0.2443 0.0250             0.0250
      2003-01-01            0.2195 0.0659             0.0250
      2004-01-01            0.0978 0.0255             0.1050
      2005-01-01            0.0250 0.0250             0.2445
                 Emerging Markets Equity Market Neutral Event Driven
      2000-01-01            0.025             0.3500        0.025
      2001-01-01            0.025             0.3500        0.025
      2002-01-01            0.025             0.3500        0.025
      2003-01-01            0.025             0.2817        0.025
      2004-01-01            0.025             0.3500        0.025
      2005-01-01            0.025             0.2054        0.025
                 Fixed Income Arbitrage Global Macro Long/Short Equity
      2000-01-01              0.0250        0.025             0.025
      2001-01-01              0.0250        0.025             0.025
      2002-01-01              0.2056        0.025             0.025
      2003-01-01              0.2577        0.025             0.025
      2004-01-01              0.2715        0.025             0.025
      2005-01-01              0.3500        0.025             0.025
                 Merger Arbitrage Relative Value
      2000-01-01       0.0714      0.2575
      2001-01-01       0.1213      0.0250
      2002-01-01       0.0250      0.0250
      2003-01-01       0.0250      0.0250
      2004-01-01       0.0250      0.0250
      2005-01-01       0.0250      0.0250
      
      > chart.StackedBar(weights, date.format = "%Y", cex.legend = 0.7, 
      +     colorset = rainbow12equal)
      
      > chart.StackedBar(weights, colorset = rainbow12equal, 
      +     legend.loc = NULL)
      
      > chart.StackedBar(weights[1, , drop = FALSE], unstacked = TRUE, 
      +     las = 3)
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for chart.VaRSensitivity

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > chart.VaRSensitivity(managers[, 1, drop = FALSE], 
      +     methods = c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"), 
      +     colorset = bluefocus, lwd = 2)
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for charts.RollingPerformance

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > charts.RollingPerformance(managers[, 1:8], Rf = managers[, 
      +     10, drop = FALSE], colorset = tim8equal, main = "Rolling 12-Month Performance", 
      +     legend.loc = "topleft")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for edhec

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > head(edhec)
                 Convertible Arbitrage CTA Global Distressed Securities
      1997-01-31                0.0119     0.0393                0.0178
      1997-02-28                0.0123     0.0298                0.0122
      1997-03-31                0.0078    -0.0021               -0.0012
      1997-04-30                0.0086    -0.0170                0.0030
      1997-05-31                0.0156    -0.0015                0.0233
      1997-06-30                0.0212     0.0085                0.0217
                 Emerging Markets Equity Market Neutral Event Driven
      1997-01-31           0.0791                0.0189       0.0213
      1997-02-28           0.0525                0.0101       0.0084
      1997-03-31          -0.0120                0.0016      -0.0023
      1997-04-30           0.0119                0.0119      -0.0005
      1997-05-31           0.0315                0.0189       0.0346
      1997-06-30           0.0581                0.0165       0.0258
                 Fixed Income Arbitrage Global Macro Long/Short Equity
      1997-01-31                 0.0191       0.0573            0.0281
      1997-02-28                 0.0122       0.0175           -0.0006
      1997-03-31                 0.0109      -0.0119           -0.0084
      1997-04-30                 0.0130       0.0172            0.0084
      1997-05-31                 0.0118       0.0108            0.0394
      1997-06-30                 0.0108       0.0218            0.0223
                 Merger Arbitrage Relative Value Short Selling Funds of Funds
      1997-01-31           0.0150         0.0180       -0.0166         0.0317
      1997-02-28           0.0034         0.0118        0.0426         0.0106
      1997-03-31           0.0060         0.0010        0.0778        -0.0077
      1997-04-30          -0.0001         0.0122       -0.0129         0.0009
      1997-05-31           0.0197         0.0173       -0.0737         0.0275
      1997-06-30           0.0231         0.0198       -0.0065         0.0225
      
      > summary(edhec)
           Index            Convertible Arbitrage   CTA Global       
       Min.   :1997-01-31   Min.   :-0.1237     Min.   :-0.0568  
       1st Qu.:2003-02-28   1st Qu.: 0.0002     1st Qu.:-0.0114  
       Median :2009-03-31   Median : 0.0065     Median : 0.0020  
       Mean   :2009-03-31   Mean   : 0.0057     Mean   : 0.0043  
       3rd Qu.:2015-04-30   3rd Qu.: 0.0137     3rd Qu.: 0.0199  
       Max.   :2021-05-31   Max.   : 0.0611     Max.   : 0.0691  
       Distressed Securities Emerging Markets   Equity Market Neutral
       Min.   :-0.1061     Min.   :-0.1922   Min.   :-0.0587    
       1st Qu.:-0.0021     1st Qu.:-0.0092   1st Qu.: 0.0009    
       Median : 0.0088     Median : 0.0100   Median : 0.0047    
       Mean   : 0.0068     Mean   : 0.0067   Mean   : 0.0043    
       3rd Qu.: 0.0179     3rd Qu.: 0.0257   3rd Qu.: 0.0083    
       Max.   : 0.0504     Max.   : 0.1230   Max.   : 0.0253    
        Event Driven       Fixed Income Arbitrage  Global Macro      
       Min.   :-0.1269   Min.   :-0.0867       Min.   :-0.0313  
       1st Qu.:-0.0012   1st Qu.: 0.0018       1st Qu.:-0.0039  
       Median : 0.0088   Median : 0.0055       Median : 0.0047  
       Mean   : 0.0066   Mean   : 0.0044       Mean   : 0.0055  
       3rd Qu.: 0.0168   3rd Qu.: 0.0093       3rd Qu.: 0.0128  
       Max.   : 0.0666   Max.   : 0.0365       Max.   : 0.0738  
       Long/Short Equity   Merger Arbitrage    Relative Value      Short Selling     
       Min.   :-0.0813   Min.   :-0.0790   Min.   :-0.0692   Min.   :-0.1340  
       1st Qu.:-0.0047   1st Qu.: 0.0007   1st Qu.: 0.0011   1st Qu.:-0.0251  
       Median : 0.0082   Median : 0.0059   Median : 0.0067   Median :-0.0032  
       Mean   : 0.0067   Mean   : 0.0055   Mean   : 0.0057   Mean   :-0.0012  
       3rd Qu.: 0.0195   3rd Qu.: 0.0111   3rd Qu.: 0.0130   3rd Qu.: 0.0181  
       Max.   : 0.0745   Max.   : 0.0472   Max.   : 0.0392   Max.   : 0.2463  
       Funds of Funds     
       Min.   :-0.0705  
       1st Qu.:-0.0033  
       Median : 0.0052  
       Mean   : 0.0045  
       3rd Qu.: 0.0127  
       Max.   : 0.0666  
      
      > tail(cumprod(1 + edhec), 1)
                 Convertible Arbitrage CTA Global Distressed Securities
      2021-05-31              5.2088   3.2780              6.9895
                 Emerging Markets Equity Market Neutral Event Driven
      2021-05-31         6.0883              3.5173     6.6540
                 Fixed Income Arbitrage Global Macro Long/Short Equity
      2021-05-31               3.5806     4.9778          6.6731
                 Merger Arbitrage Relative Value Short Selling Funds of Funds
      2021-05-31         5.0111       5.2222     0.5130       3.6010
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for findDrawdowns

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > findDrawdowns(edhec[, "Funds of Funds", drop = FALSE])
      $return
       [1]  0.0000 -0.0077  0.0000 -0.0132  0.0000 -0.0706
       [7]  0.0000 -0.0387  0.0000 -0.0373  0.0000 -0.0162
      [13]  0.0000 -0.0015  0.0000 -0.0260  0.0000 -0.0004
      [19]  0.0000 -0.0174  0.0000 -0.0184  0.0000 -0.0149
      [25]  0.0000 -0.0165  0.0000 -0.0222  0.0000 -0.2059
      [31]  0.0000 -0.0040  0.0000 -0.0073  0.0000 -0.0783
      [37]  0.0000 -0.0007  0.0000 -0.0593  0.0000 -0.0827
      [43]  0.0000 -0.0019  0.0000 -0.0095  0.0000
      
      $from
       [1]   1   3   5  10  14  17  27  40  44  45  54  55  60  62  63  66  74  75  76
      [20]  88  95  99 103 106 107 113 118 128 130 131 210 211 212 213 215 222 248 251
      [39] 252 254 276 278 283 285 286 289 290
      
      $trough
       [1]   1   3   5  11  14  22  27  41  44  47  54  57  60  62  63  70  74  75  76
      [20]  92  95 100 103 106 107 115 118 128 130 144 210 211 212 214 215 230 248 251
      [39] 252 264 276 279 283 285 286 289 290
      
      $to
       [1]   3   5  10  14  17  27  40  44  45  54  55  60  62  63  66  74  75  76  88
      [20]  95  99 103 106 107 113 118 128 130 131 210 211 212 213 215 222 248 251 252
      [39] 254 276 278 283 285 286 289 290 294
      
      $length
       [1]  3  3  6  5  4 11 14  5  2 10  2  6  3  2  4  9  2  2 13  8  5  5  4  2  7
      [26]  6 11  3  2 80  2  2  2  3  8 27  4  2  3 23  3  6  3  2  4  2  5
      
      $peaktotrough
       [1]  1  1  1  2  1  6  1  2  1  3  1  3  1  1  1  5  1  1  1  5  1  2  1  1  1
      [26]  3  1  1  1 14  1  1  1  2  1  9  1  1  1 11  1  2  1  1  1  1  1
      
      $recovery
       [1]  2  2  5  3  3  5 13  3  1  7  1  3  2  1  3  4  1  1 12  3  4  3  3  1  6
      [26]  3 10  2  1 66  1  1  1  1  7 18  3  1  2 12  2  4  2  1  3  1  4
      
      
      > sortDrawdowns(findDrawdowns(edhec[, "Funds of Funds", 
      +     drop = FALSE]))
      $return
       [1] -0.2059 -0.0827 -0.0783 -0.0706 -0.0593 -0.0387
       [7] -0.0373 -0.0260 -0.0222 -0.0184 -0.0174 -0.0165
      [13] -0.0162 -0.0149 -0.0132 -0.0095 -0.0077 -0.0073
      [19] -0.0040 -0.0019 -0.0015 -0.0007 -0.0004  0.0000
      [25]  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000
      [31]  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000
      [37]  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000
      [43]  0.0000  0.0000  0.0000  0.0000  0.0000
      
      $from
       [1] 131 278 222  17 254  40  45  66 128  99  88 113  55 106  10 289   3 213 211
      [20] 285  62 251  75   1   5  14  27  44  54  60  63  74  76  95 103 107 118 130
      [39] 210 212 215 248 252 276 283 286 290
      
      $trough
       [1] 144 279 230  22 264  41  47  70 128 100  92 115  57 106  11 289   3 214 211
      [20] 285  62 251  75   1   5  14  27  44  54  60  63  74  76  95 103 107 118 130
      [39] 210 212 215 248 252 276 283 286 290
      
      $to
       [1] 210 283 248  27 276  44  54  74 130 103  95 118  60 107  14 290   5 215 212
      [20] 286  63 252  76   3  10  17  40  45  55  62  66  75  88  99 106 113 128 131
      [39] 211 213 222 251 254 278 285 289 294
      
      $length
       [1] 80  6 27 11 23  5 10  9  3  5  8  6  6  2  5  2  3  3  2  2  2  2  2  3  6
      [26]  4 14  2  2  3  4  2 13  5  4  7 11  2  2  2  8  4  3  3  3  4  5
      
      $peaktotrough
       [1] 14  2  9  6 11  2  3  5  1  2  5  3  3  1  2  1  1  2  1  1  1  1  1  1  1
      [26]  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
      
      $recovery
       [1] 66  4 18  5 12  3  7  4  2  3  3  3  3  1  3  1  2  1  1  1  1  1  1  2  5
      [26]  3 13  1  1  2  3  1 12  4  3  6 10  1  1  1  7  3  2  2  2  3  4
      
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for managers

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > head(managers)
                    HAM1 HAM2    HAM3    HAM4 HAM5 HAM6 EDHEC LS EQ SP500 TR
      1996-01-31  0.0074   NA  0.0349  0.0222   NA   NA          NA   0.0340
      1996-02-29  0.0193   NA  0.0351  0.0195   NA   NA          NA   0.0093
      1996-03-31  0.0155   NA  0.0258 -0.0098   NA   NA          NA   0.0096
      1996-04-30 -0.0091   NA  0.0449  0.0236   NA   NA          NA   0.0147
      1996-05-31  0.0076   NA  0.0353  0.0028   NA   NA          NA   0.0258
      1996-06-30 -0.0039   NA -0.0303 -0.0019   NA   NA          NA   0.0038
                 US 10Y TR US 3m TR
      1996-01-31   0.0038  0.0045
      1996-02-29  -0.0353  0.0039
      1996-03-31  -0.0105  0.0037
      1996-04-30  -0.0173  0.0042
      1996-05-31  -0.0054  0.0044
      1996-06-30   0.0150  0.0041
      
      > tail(cumprod(1 + managers), 1)
                     HAM1 HAM2     HAM3    HAM4 HAM5 HAM6 EDHEC LS EQ SP500 TR
      2006-12-31 4.1266   NA 4.7067 3.5294   NA   NA          NA 2.7616
                 US 10Y TR US 3m TR
      2006-12-31  1.7340 1.5296
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for maxDrawdown

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > t(round(maxDrawdown(edhec[, "Funds of Funds"]), 4))
             [,1]
      [1,] 0.2059
      
      > data(managers)
      
      > t(round(maxDrawdown(managers), 4))
                  Worst Drawdown
      HAM1                0.1518
      HAM2                0.2399
      HAM3                0.2894
      HAM4                0.2874
      HAM5                0.3405
      HAM6                0.0788
      EDHEC LS EQ         0.1075
      SP500 TR            0.4473
      US 10Y TR           0.1006
      US 3m TR            0.0000
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for mean.geometric

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > mean.geometric(edhec[, "Funds of Funds"])
                     Funds of Funds
      Geometric Mean    0.0043
      
      > mean.stderr(edhec[, "Funds of Funds"])
                     Funds of Funds
      Standard Error   0.0009
      
      > mean.UCL(edhec[, "Funds of Funds"])
                             Funds of Funds
      Upper Confidence Level    0.0063
      
      > mean.LCL(edhec[, "Funds of Funds"])
                             Funds of Funds
      Lower Confidence Level    0.0026
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for portfolio-moments

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > p <- ncol(edhec)
      
      > w <- rep(1/p, p)
      
      > sigma <- cov(edhec)
      
      > pm2 <- portm2(w, sigma)
      
      > dpm2 <- derportm2(w, sigma)
      
      > m3 <- M3.MM(edhec)
      
      > pm3 <- portm3(w, m3)
      
      > dpm3 <- derportm3(w, m3)
      
      > m4 <- M4.MM(edhec)
      
      > pm4 <- portm4(w, m4)
      
      > dpm4 <- derportm4(w, m4)
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for portfolio_bacon

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(portfolio_bacon)
      
      > head(portfolio_bacon)
                 portfolio.monthly.return.... benchmark.return....
      2000-01-30                        0.003                0.002
      2000-02-27                        0.026                0.025
      2000-03-30                        0.011                0.018
      2000-04-29                       -0.010               -0.011
      2000-05-30                        0.015                0.014
      2000-06-29                        0.025                0.018
      
      > summary(portfolio_bacon)
           Index            portfolio.monthly.return.... benchmark.return....
       Min.   :2000-01-30   Min.   :-0.0650              Min.   :-0.0670    
       1st Qu.:2000-07-22   1st Qu.:-0.0110              1st Qu.:-0.0072    
       Median :2001-01-14   Median : 0.0130              Median : 0.0145    
       Mean   :2001-01-13   Mean   : 0.0090              Mean   : 0.0100    
       3rd Qu.:2001-07-06   3rd Qu.: 0.0295              3rd Qu.: 0.0285    
       Max.   :2001-12-30   Max.   : 0.0810              Max.   : 0.0830    
      
      > tail(cumprod(1 + portfolio_bacon), 1)
                 portfolio.monthly.return.... benchmark.return....
      2001-12-30                     1.2181             1.2498
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for prices

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(prices)
      
      > head(prices)
                 AdjClose
      1999-01-04    82.28
      1999-01-05    85.26
      1999-01-06    84.86
      1999-01-07    85.51
      1999-01-08    84.33
      1999-01-11    85.09
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for sortDrawdowns

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > findDrawdowns(edhec[, "Funds of Funds", drop = FALSE])
      $return
       [1]  0.0000 -0.0077  0.0000 -0.0132  0.0000 -0.0706
       [7]  0.0000 -0.0387  0.0000 -0.0373  0.0000 -0.0162
      [13]  0.0000 -0.0015  0.0000 -0.0260  0.0000 -0.0004
      [19]  0.0000 -0.0174  0.0000 -0.0184  0.0000 -0.0149
      [25]  0.0000 -0.0165  0.0000 -0.0222  0.0000 -0.2059
      [31]  0.0000 -0.0040  0.0000 -0.0073  0.0000 -0.0783
      [37]  0.0000 -0.0007  0.0000 -0.0593  0.0000 -0.0827
      [43]  0.0000 -0.0019  0.0000 -0.0095  0.0000
      
      $from
       [1]   1   3   5  10  14  17  27  40  44  45  54  55  60  62  63  66  74  75  76
      [20]  88  95  99 103 106 107 113 118 128 130 131 210 211 212 213 215 222 248 251
      [39] 252 254 276 278 283 285 286 289 290
      
      $trough
       [1]   1   3   5  11  14  22  27  41  44  47  54  57  60  62  63  70  74  75  76
      [20]  92  95 100 103 106 107 115 118 128 130 144 210 211 212 214 215 230 248 251
      [39] 252 264 276 279 283 285 286 289 290
      
      $to
       [1]   3   5  10  14  17  27  40  44  45  54  55  60  62  63  66  74  75  76  88
      [20]  95  99 103 106 107 113 118 128 130 131 210 211 212 213 215 222 248 251 252
      [39] 254 276 278 283 285 286 289 290 294
      
      $length
       [1]  3  3  6  5  4 11 14  5  2 10  2  6  3  2  4  9  2  2 13  8  5  5  4  2  7
      [26]  6 11  3  2 80  2  2  2  3  8 27  4  2  3 23  3  6  3  2  4  2  5
      
      $peaktotrough
       [1]  1  1  1  2  1  6  1  2  1  3  1  3  1  1  1  5  1  1  1  5  1  2  1  1  1
      [26]  3  1  1  1 14  1  1  1  2  1  9  1  1  1 11  1  2  1  1  1  1  1
      
      $recovery
       [1]  2  2  5  3  3  5 13  3  1  7  1  3  2  1  3  4  1  1 12  3  4  3  3  1  6
      [26]  3 10  2  1 66  1  1  1  1  7 18  3  1  2 12  2  4  2  1  3  1  4
      
      
      > sortDrawdowns(findDrawdowns(edhec[, "Funds of Funds", 
      +     drop = FALSE]))
      $return
       [1] -0.2059 -0.0827 -0.0783 -0.0706 -0.0593 -0.0387
       [7] -0.0373 -0.0260 -0.0222 -0.0184 -0.0174 -0.0165
      [13] -0.0162 -0.0149 -0.0132 -0.0095 -0.0077 -0.0073
      [19] -0.0040 -0.0019 -0.0015 -0.0007 -0.0004  0.0000
      [25]  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000
      [31]  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000
      [37]  0.0000  0.0000  0.0000  0.0000  0.0000  0.0000
      [43]  0.0000  0.0000  0.0000  0.0000  0.0000
      
      $from
       [1] 131 278 222  17 254  40  45  66 128  99  88 113  55 106  10 289   3 213 211
      [20] 285  62 251  75   1   5  14  27  44  54  60  63  74  76  95 103 107 118 130
      [39] 210 212 215 248 252 276 283 286 290
      
      $trough
       [1] 144 279 230  22 264  41  47  70 128 100  92 115  57 106  11 289   3 214 211
      [20] 285  62 251  75   1   5  14  27  44  54  60  63  74  76  95 103 107 118 130
      [39] 210 212 215 248 252 276 283 286 290
      
      $to
       [1] 210 283 248  27 276  44  54  74 130 103  95 118  60 107  14 290   5 215 212
      [20] 286  63 252  76   3  10  17  40  45  55  62  66  75  88  99 106 113 128 131
      [39] 211 213 222 251 254 278 285 289 294
      
      $length
       [1] 80  6 27 11 23  5 10  9  3  5  8  6  6  2  5  2  3  3  2  2  2  2  2  3  6
      [26]  4 14  2  2  3  4  2 13  5  4  7 11  2  2  2  8  4  3  3  3  4  5
      
      $peaktotrough
       [1] 14  2  9  6 11  2  3  5  1  2  5  3  3  1  2  1  1  2  1  1  1  1  1  1  1
      [26]  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
      
      $recovery
       [1] 66  4 18  5 12  3  7  4  2  3  3  3  3  1  3  1  2  1  1  1  1  1  1  2  5
      [26]  3 13  1  1  2  3  1 12  4  3  6 10  1  1  1  7  3  2  2  2  3  4
      
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for table.AnnualizedReturns

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > table.AnnualizedReturns(managers[, 1:8])
                                  HAM1   HAM2   HAM3   HAM4   HAM5   HAM6 EDHEC LS EQ
      Annualized Return         0.1375 0.1747 0.1512 0.1215 0.0373 0.1373      0.1180
      Annualized Std Dev        0.0888 0.1272 0.1265 0.1843 0.1584 0.0825      0.0708
      Annualized Sharpe (Rf=0%) 1.5491 1.3732 1.1955 0.6592 0.2356 1.6642      1.6657
                                SP500 TR
      Annualized Return           0.0967
      Annualized Std Dev          0.1500
      Annualized Sharpe (Rf=0%)   0.6449
      
      > require("Hmisc")
    Message
      Loading required package: Hmisc
      
      Attaching package: 'Hmisc'
      
      The following object is masked from 'package:testthat':
      
          describe
      
      The following objects are masked from 'package:base':
      
          format.pval, units
      
    Output
      
      > result = t(table.AnnualizedReturns(managers[, 1:8], 
      +     Rf = 0.04/12))
      
      > textplot(format.df(result, na.blank = TRUE, numeric.dollar = FALSE, 
      +     cdec = c(3, 3, 1)), rmar = 0.8, cmar = 2, max.cex = 0.9, 
      +     halign = "center", valign = "top", row.valign = "center", 
      +     wrap.rownames = 20, wrap.colnames = 10, col.rownames = c("red", 
      +         rep("darkgray", 5), rep("orange", 2)), mar = c(0, 0, 
      +         3, 0) + 0.1)
      
      > title(main = "Annualized Performance")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for table.Arbitrary

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > table.Arbitrary(edhec, metrics = c("VaR", "ES"), metricsNames = c("Modified VaR", 
      +     "Modified Expected Shortfall"))
                                  Convertible Arbitrage  CTA Global
      Modified VaR                          -0.0256 -0.0320
      Modified Expected Shortfall           -0.0894 -0.0403
                                  Distressed Securities Emerging Markets
      Modified VaR                          -0.0280      -0.0534
      Modified Expected Shortfall           -0.0673      -0.1157
                                  Equity Market Neutral Event Driven
      Modified VaR                          -0.0109  -0.0296
      Modified Expected Shortfall           -0.0367  -0.0811
                                  Fixed Income Arbitrage Global Macro
      Modified VaR                           -0.0177  -0.0138
      Modified Expected Shortfall            -0.0530  -0.0169
                                  Long/Short Equity Merger Arbitrage Relative Value
      Modified VaR                      -0.0295      -0.0150    -0.0173
      Modified Expected Shortfall       -0.0485      -0.0513    -0.0475
                                  Short Selling Funds of Funds
      Modified VaR                  -0.0621    -0.0230
      Modified Expected Shortfall   -0.0675    -0.0459
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for table.Autocorrelation

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > t(table.Autocorrelation(managers))
                    rho 1   rho 2   rho 3   rho 4   rho 5   rho 6 Q(6) p-value
      HAM1         0.1890 -0.0847 -0.0602 -0.1842 -0.0035  0.0492       0.0788
      HAM2         0.1975  0.3046  0.0719  0.0770  0.0626  0.1574       0.0011
      HAM3         0.0071  0.1970  0.0413  0.1237 -0.0717  0.2022       0.0286
      HAM4         0.1954 -0.0840 -0.1694 -0.0923 -0.0041 -0.0065       0.0812
      HAM5        -0.0579 -0.1714 -0.0330  0.1371 -0.1462 -0.1148       0.2989
      HAM6         0.0982  0.1816 -0.0274 -0.1711 -0.0501 -0.1248       0.3885
      EDHEC LS EQ  0.2119  0.0834  0.0254 -0.0435 -0.0533  0.1758       0.0872
      SP500 TR    -0.0134 -0.0336  0.0514 -0.0878  0.0853  0.0776       0.7487
      US 10Y TR    0.0398 -0.1739  0.1049 -0.0355 -0.1116 -0.0602       0.2199
      US 3m TR     0.9224  0.9081  0.8968  0.8746  0.8363  0.8127       0.0000
      
      > result = t(table.Autocorrelation(managers[, 1:8]))
      
      > textplot(result, rmar = 0.8, cmar = 2, max.cex = 0.9, 
      +     halign = "center", valign = "top", row.valign = "center", 
      +     wrap.rownames = 15, wrap.colnames = 10, mar = c(0, 0, 3, 
      +         0) + 0.1)
      
      > title(main = "Autocorrelation")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for table.CAPM

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > table.SFM(managers[, 1:3], managers[, 8], Rf = managers[, 
      +     10])
                          HAM1 to SP500 TR HAM2 to SP500 TR HAM3 to SP500 TR
      Alpha                         0.0058           0.0091           0.0062
      Beta                          0.3901           0.3384           0.5523
      Alpha Robust                  0.0061           0.0042           0.0050
      Beta Robust                   0.3315           0.2629           0.6064
      Beta+                         0.3005           0.5227           0.4858
      Beta-                         0.4264           0.0698           0.5067
      Beta+ Robust                  0.3753           0.5738           0.7045
      Beta- Robust                  0.4166           0.0298           0.4634
      R-squared                     0.4339           0.1673           0.4341
      R-squared Robust              0.3257           0.1211           0.5591
      Annualized Alpha              0.0715           0.1147           0.0772
      Correlation                   0.6587           0.4090           0.6589
      Correlation p-value           0.0000           0.0000           0.0000
      Tracking Error                0.1132           0.1534           0.1159
      Active Premium                0.0408           0.0776           0.0545
      Information Ratio             0.3604           0.5060           0.4701
      Treynor Ratio                 0.2428           0.3883           0.1956
      
      > result = table.SFM(managers[, 1:3], managers[, 8], 
      +     Rf = managers[, 10])
      
      > require(Hmisc)
      
      > textplot(result, rmar = 0.8, cmar = 1.5, max.cex = 0.9, 
      +     halign = "center", valign = "top", row.valign = "center", 
      +     wrap.rownames = 15, wrap.colnames = 10, mar = c(0, 0, 3, 
      +         0) + 0.1)
      
      > title(main = "Single Factor Model Related Statistics")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for table.CalendarReturns

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > t(table.CalendarReturns(managers[, c(1, 7, 8)]))
                  1996 1997 1998 1999 2000  2001  2002 2003 2004 2005 2006
      Jan          0.7  2.1  0.6 -0.9 -1.0   0.8   1.4 -4.1  0.5  0.0  6.9
      Feb          1.9  0.2  4.3  0.9  1.2   0.8  -1.2 -2.5  0.0  2.1  1.5
      Mar          1.6  0.9  3.6  4.6  5.8  -1.0   0.6  3.6  0.9 -2.1  4.0
      Apr         -0.9  1.3  0.8  5.1  2.0   3.5   0.5  6.5 -0.4 -2.1 -0.1
      May          0.8  4.4 -2.3  1.6  3.4   5.8  -0.1  3.4  0.8  0.4 -2.7
      Jun         -0.4  2.3  1.2  3.3  1.2   0.2  -2.4  3.1  2.6  1.6  2.2
      Jul         -2.3  1.5 -2.1  1.0  0.5   2.1  -7.6  1.8  0.0  0.9 -1.4
      Aug          4.0  2.4 -9.4 -1.7  3.9   1.6   0.8  0.0  0.5  1.1  1.6
      Sep          1.5  2.2  2.5 -0.4  0.1  -3.1  -5.8  0.9  0.9  2.6  0.7
      Oct          2.9 -2.1  5.6 -0.1 -0.8   0.1   3.0  4.8 -0.1 -1.9  4.3
      Nov          1.6  2.5  1.3  0.4  1.0   3.4   6.6  1.7  3.9  2.3  1.2
      Dec          1.8  1.1  1.0  1.5 -0.7   6.8  -3.2  2.8  4.4  2.6  1.1
      HAM1        13.6 20.4  6.1 16.1 17.7  22.4  -8.0 23.7 14.9  7.8 20.5
      EDHEC LS EQ   NA 21.4 14.6 31.4 12.0  -1.2  -6.4 19.3  8.6 11.3 11.7
      SP500 TR    23.0 33.4 28.6 21.0 -9.1 -11.9 -22.1 28.7 10.9  4.9 15.8
      
      > require("Hmisc")
      
      > result = t(table.CalendarReturns(managers[, c(1, 8)]))
      
      > textplot(format.df(result, na.blank = TRUE, numeric.dollar = FALSE, 
      +     cdec = rep(1, dim(result)[2])), rmar = 0.8, cmar = 1, max.cex = 0.9, 
      +     halign = "center", valign = "top", row.valign = "center", 
      +     wrap.rownames = 20, wrap.colnames = 10, col.rownames = c(rep("darkgray", 
      +         12), "black", "blue"), mar = c(0, 0, 3, 0) + 0.1)
      
      > title(main = "Calendar Returns")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for table.CaptureRatios

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > table.CaptureRatios(managers[, 1:6], managers[, 7, 
      +     drop = FALSE])
           Up Capture Down Capture
      HAM1     1.0017       0.7090
      HAM2     1.3349       0.8473
      HAM3     1.5373       1.3590
      HAM4     2.2840       1.8126
      HAM5     0.8564       1.2373
      HAM6     1.4836       0.8592
      
      > table.UpDownRatios(managers[, 1:6], managers[, 7, 
      +     drop = FALSE])
                          Up Capture Down Capture Up Number Down Number Up Percent
      HAM1 to EDHEC LS EQ     1.0017       0.7090    0.8675      0.5135     0.5663
      HAM2 to EDHEC LS EQ     1.3349       0.8473    0.6988      0.8378     0.4458
      HAM3 to EDHEC LS EQ     1.5373       1.3590    0.8193      0.8108     0.4819
      HAM4 to EDHEC LS EQ     2.2840       1.8126    0.7590      0.7297     0.6386
      HAM5 to EDHEC LS EQ     0.8564       1.2373    0.7143      0.7500     0.5510
      HAM6 to EDHEC LS EQ     1.4836       0.8592    0.8837      0.6190     0.6977
                          Down Percent
      HAM1 to EDHEC LS EQ       0.6486
      HAM2 to EDHEC LS EQ       0.4324
      HAM3 to EDHEC LS EQ       0.4324
      HAM4 to EDHEC LS EQ       0.3784
      HAM5 to EDHEC LS EQ       0.4286
      HAM6 to EDHEC LS EQ       0.4762
      
      > result <- t(table.UpDownRatios(managers[, 1:6], managers[, 
      +     7, drop = FALSE]))
      
      > colnames(result) <- colnames(managers[, 1:6])
      
      > textplot(result, rmar = 0.8, cmar = 1.5, max.cex = 0.9, 
      +     halign = "center", valign = "top", row.valign = "center", 
      +     wrap.rownames = 15, wrap.colnames = 10, mar = c(0, 0, 3, 
      +         0) + 0.1)
      
      > title(main = "Capture Ratios for EDHEC LS EQ")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for table.Correlation

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > table.Correlation(managers[, 1:6], managers[, 7:8])
                          Correlation      p-value   Lower CI  Upper CI
      HAM1 to EDHEC LS EQ   0.5896 1.3785e-12 0.4589 0.6954
      HAM1 to SP500 TR      0.6600 7.3978e-18 0.5513 0.7467
      HAM2 to EDHEC LS EQ   0.7015 4.4683e-19 0.5974 0.7824
      HAM2 to SP500 TR      0.4128 1.7153e-6 0.2557 0.5486
      HAM3 to EDHEC LS EQ   0.8053 1.4438e-28 0.7317 0.8603
      HAM3 to SP500 TR      0.6608 6.5454e-18 0.5523 0.7473
      HAM4 to EDHEC LS EQ   0.6148 7.9704e-14 0.4895 0.7152
      HAM4 to SP500 TR      0.5601 2.8701e-12 0.4305 0.6671
      HAM5 to EDHEC LS EQ   0.4462 4.7496e-5 0.2469 0.6093
      HAM5 to SP500 TR      0.2844 1.2168e-2 0.0645 0.4779
      HAM6 to EDHEC LS EQ   0.7285 8.8798e-12 0.5880 0.8263
      HAM6 to SP500 TR      0.5091 1.7359e-5 0.3010 0.6709
      
      > result = table.Correlation(managers[, 1:6], managers[, 
      +     8])
      
      > rownames(result) = colnames(managers[, 1:6])
      
      > require("Hmisc")
      
      > textplot(format.df(result, na.blank = TRUE, numeric.dollar = FALSE, 
      +     cdec = rep(3, dim(result)[2])), rmar = 0.8, cmar = 1.5, max.cex = 0.9, 
      +     halign = "center", valign = "top", row.valign = "center", 
      +     wrap.rownames = 20, wrap.colnames = 10, mar = c(0, 0, 3, 
      +         0) + 0.1)
      
      > title(main = "Correlations to SP500 TR")
      
      > ctable = table.Correlation(managers[, 1:6], managers[, 
      +     8, drop = FALSE], conf.level = 0.99)
      
      > dotchart(ctable[, 1], labels = rownames(ctable), xlim = c(-1, 
      +     1))
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for table.Distributions

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > table.Distributions(managers[, 1:8])
                                HAM1   HAM2   HAM3    HAM4   HAM5    HAM6 EDHEC LS EQ
      monthly Std Dev         0.0256 0.0367 0.0365  0.0532 0.0457  0.0238      0.0205
      Skewness               -0.6588 1.4580 0.7908 -0.4311 0.0738 -0.2800      0.0177
      Kurtosis                5.3616 5.3794 5.6829  3.8632 5.3143  2.6511      3.9105
      Excess kurtosis         2.3616 2.3794 2.6829  0.8632 2.3143 -0.3489      0.9105
      Sample skewness        -0.6741 1.4937 0.8091 -0.4410 0.0768 -0.2936      0.0182
      Sample excess kurtosis  2.5004 2.5270 2.8343  0.9437 2.5541 -0.2778      1.0013
                             SP500 TR
      monthly Std Dev          0.0433
      Skewness                -0.5531
      Kurtosis                 3.5598
      Excess kurtosis          0.5598
      Sample skewness         -0.5659
      Sample excess kurtosis   0.6285
      
      > require("Hmisc")
      
      > result <- t(table.Distributions(managers[, 1:8]))
      
      > textplot(format.df(result, na.blank = TRUE, numeric.dollar = FALSE, 
      +     cdec = c(3, 3, 1)), rmar = 0.8, cmar = 2, max.cex = 0.9, 
      +     halign = "center", valign = "top", row.valign = "center", 
      +     wrap.rownames = 20, wrap.colnames = 10, col.rownames = c("red", 
      +         rep("darkgray", 5), rep("orange", 2)), mar = c(0, 0, 
      +         3, 0) + 0.1)
      
      > title(main = "Portfolio Distributions statistics")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for table.DownsideRisk

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > table.DownsideRisk(edhec, Rf = 0.04/12, MAR = 0.05/12, 
      +     p = 0.95)
                                  Convertible Arbitrage CTA Global
      Semi Deviation                             0.0136     0.0156
      Gain Deviation                             0.0095     0.0153
      Loss Deviation                             0.0203     0.0123
      Downside Deviation (MAR=5%)                0.0131     0.0156
      Downside Deviation (Rf=4%)                 0.0128     0.0151
      Downside Deviation (0%)                    0.0118     0.0132
      Maximum Drawdown                           0.2927     0.1256
      Historical VaR (95%)                      -0.0151    -0.0315
      Historical ES (95%)                       -0.0388    -0.0406
      Modified VaR (95%)                        -0.0257    -0.0320
      Modified ES (95%)                         -0.0894    -0.0404
                                  Distressed Securities Emerging Markets
      Semi Deviation                             0.0146           0.0255
      Gain Deviation                             0.0099           0.0186
      Loss Deviation                             0.0177           0.0287
      Downside Deviation (MAR=5%)                0.0134           0.0244
      Downside Deviation (Rf=4%)                 0.0131           0.0240
      Downside Deviation (0%)                    0.0119           0.0226
      Maximum Drawdown                           0.2292           0.3598
      Historical VaR (95%)                      -0.0198          -0.0423
      Historical ES (95%)                       -0.0413          -0.0754
      Modified VaR (95%)                        -0.0280          -0.0534
      Modified ES (95%)                         -0.0673          -0.1157
                                  Equity Market Neutral Event Driven
      Semi Deviation                             0.0065       0.0154
      Gain Deviation                             0.0051       0.0106
      Loss Deviation                             0.0093       0.0198
      Downside Deviation (MAR=5%)                0.0064       0.0143
      Downside Deviation (Rf=4%)                 0.0061       0.0140
      Downside Deviation (0%)                    0.0050       0.0129
      Maximum Drawdown                           0.1108       0.2008
      Historical VaR (95%)                      -0.0084      -0.0255
      Historical ES (95%)                       -0.0175      -0.0445
      Modified VaR (95%)                        -0.0110      -0.0296
      Modified ES (95%)                         -0.0367      -0.0811
                                  Fixed Income Arbitrage Global Macro
      Semi Deviation                              0.0099       0.0093
      Gain Deviation                              0.0055       0.0119
      Loss Deviation                              0.0179       0.0067
      Downside Deviation (MAR=5%)                 0.0098       0.0085
      Downside Deviation (Rf=4%)                  0.0096       0.0080
      Downside Deviation (0%)                     0.0088       0.0063
      Maximum Drawdown                            0.1788       0.0792
      Historical VaR (95%)                       -0.0072      -0.0149
      Historical ES (95%)                        -0.0291      -0.0211
      Modified VaR (95%)                         -0.0177      -0.0138
      Modified ES (95%)                          -0.0531      -0.0170
                                  Long/Short Equity Merger Arbitrage Relative Value
      Semi Deviation                         0.0155           0.0089         0.0097
      Gain Deviation                         0.0132           0.0073         0.0066
      Loss Deviation                         0.0154           0.0125         0.0137
      Downside Deviation (MAR=5%)            0.0143           0.0084         0.0091
      Downside Deviation (Rf=4%)             0.0139           0.0081         0.0088
      Downside Deviation (0%)                0.0125           0.0070         0.0078
      Maximum Drawdown                       0.2182           0.0850         0.1594
      Historical VaR (95%)                  -0.0262          -0.0107        -0.0114
      Historical ES (95%)                   -0.0448          -0.0233        -0.0271
      Modified VaR (95%)                    -0.0295          -0.0150        -0.0174
      Modified ES (95%)                     -0.0486          -0.0513        -0.0475
                                  Short Selling Funds of Funds
      Semi Deviation                     0.0296         0.0120
      Gain Deviation                     0.0372         0.0105
      Loss Deviation                     0.0271         0.0132
      Downside Deviation (MAR=5%)        0.0326         0.0118
      Downside Deviation (Rf=4%)         0.0322         0.0114
      Downside Deviation (0%)            0.0303         0.0101
      Maximum Drawdown                   0.7687         0.2059
      Historical VaR (95%)              -0.0668        -0.0203
      Historical ES (95%)               -0.0948        -0.0357
      Modified VaR (95%)                -0.0622        -0.0231
      Modified ES (95%)                 -0.0675        -0.0459
      
      > result = t(table.DownsideRisk(edhec, Rf = 0.04/12, 
      +     MAR = 0.05/12, p = 0.95))
      
      > require("Hmisc")
      
      > textplot(format.df(result, na.blank = TRUE, numeric.dollar = FALSE, 
      +     cdec = rep(3, dim(result)[2])), rmar = 0.8, cmar = 1.5, max.cex = 0.9, 
      +     halign = "center", valign = "top", row.valign = "center", 
      +     wrap.rownames = 15, wrap.colnames = 10, mar = c(0, 0, 3, 
      +         0) + 0.1)
      
      > title(main = "Downside Risk Statistics")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for table.DownsideRiskRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > table.DownsideRiskRatio(managers[, 1:8])
                                 HAM1   HAM2   HAM3   HAM4   HAM5   HAM6 EDHEC LS EQ
      monthly downside risk    0.0145 0.0116 0.0174 0.0341 0.0304 0.0121      0.0098
      Annualised downside risk 0.0504 0.0401 0.0601 0.1180 0.1054 0.0421      0.0341
      Downside potential       0.0051 0.0061 0.0079 0.0159 0.0145 0.0054      0.0041
      Omega                    3.1907 3.3041 2.5803 1.6920 1.2816 3.0436      3.3186
      Sortino ratio            0.7649 1.2220 0.7172 0.3234 0.1343 0.9102      0.9691
      Upside potential         0.0162 0.0203 0.0203 0.0269 0.0186 0.0165      0.0137
      Upside potential ratio   0.7503 2.2078 1.0852 0.8009 0.7557 1.0003      1.1136
      Omega-sharpe ratio       2.1907 2.3041 1.5803 0.6920 0.2816 2.0436      2.3186
                               SP500 TR
      monthly downside risk      0.0283
      Annualised downside risk   0.0980
      Downside potential         0.0132
      Omega                      1.6581
      Sortino ratio              0.3064
      Upside potential           0.0218
      Upside potential ratio     0.7153
      Omega-sharpe ratio         0.6581
      
      > require("Hmisc")
      
      > result <- t(table.DownsideRiskRatio(managers[, 1:8]))
      
      > textplot(format.df(result, na.blank = TRUE, numeric.dollar = FALSE, 
      +     cdec = c(3, 3, 1)), rmar = 0.8, cmar = 2, max.cex = 0.9, 
      +     halign = "center", valign = "top", row.valign = "center", 
      +     wrap.rownames = 20, wrap.colnames = 10, col.rownames = c("red", 
      +         rep("darkgray", 5), rep("orange", 2)), mar = c(0, 0, 
      +         3, 0) + 0.1)
      
      > title(main = "Downside risk statistics")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for table.Drawdowns

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > table.Drawdowns(edhec[, 1, drop = FALSE])
              From     Trough         To   Depth Length To Trough Recovery
      1 2007-11-30 2008-11-30 2009-09-30 -0.2927     23        13       10
      2 2004-05-31 2005-05-31 2006-02-28 -0.0822     22        13        9
      3 1998-08-31 1998-10-31 1999-03-31 -0.0712      8         3        5
      4 2020-03-31 2020-03-31 2020-07-31 -0.0700      5         1        4
      5 2015-06-30 2016-02-29 2016-07-31 -0.0527     14         9        5
      
      > table.Drawdowns(edhec[, 12, drop = FALSE])
              From     Trough         To   Depth Length To Trough Recovery
      1 2009-03-31 2017-11-30       <NA> -0.7687    148       105       NA
      2 1998-09-30 2000-08-31 2002-09-30 -0.4956     49        24       25
      3 2002-10-31 2007-05-31 2009-02-28 -0.3630     77        56       21
      4 1997-04-30 1997-09-30 1998-03-31 -0.1502     12         6        6
      5 1997-01-31 1997-01-31 1997-02-28 -0.0166      2         1        1
      
      > data(managers)
      
      > table.Drawdowns(managers[, 8, drop = FALSE])
              From     Trough         To   Depth Length To Trough Recovery
      1 2000-09-30 2002-09-30 2006-10-31 -0.4473     74        25       49
      2 1998-07-31 1998-08-31 1998-11-30 -0.1537      5         2        3
      3 2000-01-31 2000-02-29 2000-03-31 -0.0682      3         2        1
      4 1999-07-31 1999-09-30 1999-11-30 -0.0624      5         3        2
      5 1997-08-31 1997-08-31 1997-11-30 -0.0560      4         1        3
      
      > result = table.Drawdowns(managers[, 1, drop = FALSE])
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for table.DrawdownsRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > table.DrawdownsRatio(managers[, 1:8])
                       HAM1   HAM2   HAM3   HAM4   HAM5   HAM6 EDHEC LS EQ SP500 TR
      Sterling ratio 0.5463 0.5139 0.3884 0.3136 0.0847 0.7678      0.5688   0.1768
      Calmar ratio   0.9062 0.7281 0.5226 0.4227 0.1096 1.7425      1.0982   0.2163
      Burke ratio    0.6593 0.8970 0.6079 0.1998 0.1008 1.0788      0.8452   0.2191
      Pain index     0.0161 0.0617 0.0661 0.0787 0.1596 0.0184      0.0180   0.1258
      Ulcer index    0.0363 0.0938 0.1048 0.1146 0.1877 0.0297      0.0324   0.1791
      Pain ratio     8.5601 2.8291 2.2889 1.5427 0.2339 7.4443      6.5718   0.7693
      Martin ratio   3.7895 1.8615 1.4425 1.0598 0.1988 4.6165      3.6377   0.5401
      
      > require("Hmisc")
      
      > result = t(table.DrawdownsRatio(managers[, 1:8], Rf = 0.04/12))
      
      > textplot(format.df(result, na.blank = TRUE, numeric.dollar = FALSE, 
      +     cdec = c(3, 3, 1)), rmar = 0.8, cmar = 2, max.cex = 0.9, 
      +     halign = "center", valign = "top", row.valign = "center", 
      +     wrap.rownames = 20, wrap.colnames = 10, col.rownames = c("red", 
      +         rep("darkgray", 5), rep("orange", 2)), mar = c(0, 0, 
      +         3, 0) + 0.1)
      
      > title(main = "Drawdowns ratio statistics")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for table.HigherMoments

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > table.HigherMoments(managers[, 1:3], managers[, 8, 
      +     drop = FALSE])
                      HAM1 to SP500 TR HAM2 to SP500 TR HAM3 to SP500 TR
      CoSkewness                0.0000           0.0000           0.0000
      CoKurtosis                0.0000           0.0000           0.0000
      Beta CoVariance           0.3906           0.3432           0.5572
      Beta CoSkewness           0.5602           0.0454           0.5999
      Beta CoKurtosis           0.4815           0.1988           0.5068
      
      > result = t(table.HigherMoments(managers[, 1:6], managers[, 
      +     8, drop = FALSE]))
      
      > rownames(result) = colnames(managers[, 1:6])
      
      > require("Hmisc")
      
      > textplot(format.df(result, na.blank = TRUE, numeric.dollar = FALSE, 
      +     cdec = rep(3, dim(result)[2])), rmar = 0.8, cmar = 1.5, max.cex = 0.9, 
      +     halign = "center", valign = "top", row.valign = "center", 
      +     wrap.rownames = 5, wrap.colnames = 10, mar = c(0, 0, 3, 0) + 
      +         0.1)
      
      > title(main = "Higher Co-Moments with SP500 TR")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for table.InformationRatio

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > table.InformationRatio(managers[, 1:8], managers[, 
      +     8])
                                  HAM1   HAM2   HAM3   HAM4   HAM5   HAM6 EDHEC LS EQ
      Tracking Error            0.0327 0.0443 0.0334 0.0461 0.0520 0.0326      0.0326
      Annualised Tracking Error 0.1132 0.1534 0.1159 0.1597 0.1800 0.1128      0.1130
      Information Ratio         0.3604 0.5060 0.4701 0.1549 0.1212 0.6723      0.2985
                                SP500 TR
      Tracking Error                   0
      Annualised Tracking Error        0
      Information Ratio              NaN
      
      > require("Hmisc")
      
      > result = t(table.InformationRatio(managers[, 1:8], 
      +     managers[, 8]))
      
      > textplot(format.df(result, na.blank = TRUE, numeric.dollar = FALSE, 
      +     cdec = c(3, 3, 1)), rmar = 0.8, cmar = 2, max.cex = 0.9, 
      +     halign = "center", valign = "top", row.valign = "center", 
      +     wrap.rownames = 20, wrap.colnames = 10, col.rownames = c("red", 
      +         rep("darkgray", 5), rep("orange", 2)), mar = c(0, 0, 
      +         3, 0) + 0.1)
      
      > title(main = "Portfolio information ratio")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for table.MonthlyReturns

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > table.Stats(edhec[, 1:3])
                      Convertible Arbitrage CTA Global Distressed Securities
      Observations                 293.0000   293.0000              293.0000
      NAs                            0.0000     0.0000                0.0000
      Minimum                       -0.1237    -0.0568               -0.1061
      Quartile 1                     0.0002    -0.0114               -0.0021
      Median                         0.0065     0.0020                0.0088
      Arithmetic Mean                0.0058     0.0043                0.0068
      Geometric Mean                 0.0056     0.0041                0.0067
      Quartile 3                     0.0137     0.0199                0.0179
      Maximum                        0.0611     0.0691                0.0504
      SE Mean                        0.0010     0.0013                0.0011
      LCL Mean (0.95)                0.0039     0.0017                0.0047
      UCL Mean (0.95)                0.0077     0.0069                0.0089
      Variance                       0.0003     0.0005                0.0003
      Stdev                          0.0168     0.0228                0.0181
      Skewness                      -2.5970     0.1628               -1.7283
      Kurtosis                      18.6011    -0.0076                7.7946
      
      > t(table.Stats(edhec))
                             Observations NAs Minimum Quartile 1  Median
      Convertible Arbitrage           293   0 -0.1237     0.0002  0.0065
      CTA Global                      293   0 -0.0568    -0.0114  0.0020
      Distressed Securities           293   0 -0.1061    -0.0021  0.0088
      Emerging Markets                293   0 -0.1922    -0.0092  0.0100
      Equity Market Neutral           293   0 -0.0587     0.0009  0.0047
      Event Driven                    293   0 -0.1269    -0.0012  0.0088
      Fixed Income Arbitrage          293   0 -0.0867     0.0018  0.0055
      Global Macro                    293   0 -0.0313    -0.0039  0.0047
      Long/Short Equity               293   0 -0.0813    -0.0047  0.0082
      Merger Arbitrage                293   0 -0.0790     0.0007  0.0059
      Relative Value                  293   0 -0.0692     0.0011  0.0067
      Short Selling                   293   0 -0.1340    -0.0251 -0.0032
      Funds of Funds                  293   0 -0.0705    -0.0033  0.0052
                             Arithmetic Mean Geometric Mean Quartile 3 Maximum
      Convertible Arbitrage           0.0058         0.0056     0.0137  0.0611
      CTA Global                      0.0043         0.0041     0.0199  0.0691
      Distressed Securities           0.0068         0.0067     0.0179  0.0504
      Emerging Markets                0.0067         0.0062     0.0257  0.1230
      Equity Market Neutral           0.0043         0.0043     0.0083  0.0253
      Event Driven                    0.0067         0.0065     0.0168  0.0666
      Fixed Income Arbitrage          0.0044         0.0044     0.0093  0.0365
      Global Macro                    0.0056         0.0055     0.0128  0.0738
      Long/Short Equity               0.0067         0.0065     0.0195  0.0745
      Merger Arbitrage                0.0056         0.0055     0.0111  0.0472
      Relative Value                  0.0057         0.0057     0.0130  0.0392
      Short Selling                  -0.0013        -0.0023     0.0181  0.2463
      Funds of Funds                  0.0045         0.0044     0.0127  0.0666
                             SE Mean LCL Mean (0.95) UCL Mean (0.95) Variance  Stdev
      Convertible Arbitrage   0.0010          0.0039          0.0077   0.0003 0.0168
      CTA Global              0.0013          0.0017          0.0069   0.0005 0.0228
      Distressed Securities   0.0011          0.0047          0.0089   0.0003 0.0181
      Emerging Markets        0.0019          0.0030          0.0105   0.0011 0.0327
      Equity Market Neutral   0.0005          0.0034          0.0053   0.0001 0.0082
      Event Driven            0.0011          0.0045          0.0089   0.0004 0.0191
      Fixed Income Arbitrage  0.0007          0.0031          0.0057   0.0001 0.0115
      Global Macro            0.0009          0.0039          0.0073   0.0002 0.0146
      Long/Short Equity       0.0012          0.0043          0.0091   0.0004 0.0209
      Merger Arbitrage        0.0007          0.0043          0.0069   0.0001 0.0115
      Relative Value          0.0007          0.0044          0.0071   0.0001 0.0119
      Short Selling           0.0027         -0.0065          0.0040   0.0021 0.0455
      Funds of Funds          0.0009          0.0027          0.0064   0.0003 0.0161
                             Skewness Kurtosis
      Convertible Arbitrage   -2.5970  18.6011
      CTA Global               0.1628  -0.0076
      Distressed Securities   -1.7283   7.7946
      Emerging Markets        -1.2205   6.0126
      Equity Market Neutral   -1.9173  12.4266
      Event Driven            -1.8806  10.2736
      Fixed Income Arbitrage  -3.7918  25.4966
      Global Macro             0.8826   2.4863
      Long/Short Equity       -0.4702   1.9028
      Merger Arbitrage        -1.6216  12.7706
      Relative Value          -2.0781  10.1597
      Short Selling            0.7737   3.6282
      Funds of Funds          -0.5969   4.3957
      
      > result = t(table.Stats(edhec))
      
      > require("Hmisc")
      
      > textplot(format.df(result, na.blank = TRUE, numeric.dollar = FALSE, 
      +     cdec = c(rep(1, 2), rep(3, 14))), rmar = 0.8, cmar = 1.5, 
      +     max.cex = 0.9, halign = "center", valign = "top", row.valign = "center", 
      +     wrap.rownames = 10, wrap.colnames = 10, mar = c(0, 0, 3, 
      +         0) + 0.1)
      
      > title(main = "Statistics for EDHEC Indexes")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for table.ProbOutPerformance

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > table.ProbOutPerformance(edhec[, 1], edhec[, 2])
        period_lengths Convertible Arbitrage CTA Global total periods
      1              1                   162        131           293
      2              3                   183        108           291
      3              6                   183        105           288
      4              9                   185        100           285
      5             12                   177        105           282
      6             18                   179         97           276
      7             36                   159         99           258
        prob_Convertible Arbitrage_outperformance prob_CTA Global_outperformance
      1                                 0.5529                      0.4470
      2                                 0.6288                      0.3711
      3                                 0.6354                      0.3645
      4                                 0.6491                      0.3508
      5                                 0.6276                      0.3723
      6                                 0.6485                      0.3514
      7                                 0.6162                      0.3837
      
      > title(main = "Table of Convertible Arbitrage vs Benchmark")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for table.RollingPeriods

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(edhec)
      
      > table.TrailingPeriods(edhec[, 10:13], periods = c(12, 
      +     24, 36))
                            Merger Arbitrage Relative Value Short Selling
      Last 12 month Average           0.0175         0.0110        0.0074
      Last 24 month Average           0.0085         0.0055        0.0055
      Last 36 month Average           0.0069         0.0039        0.0016
      Last 12 month Std Dev           0.0181         0.0066        0.0154
      Last 24 month Std Dev           0.0245         0.0164        0.0147
      Last 36 month Std Dev           0.0203         0.0141        0.0154
                            Funds of Funds
      Last 12 month Average         0.0142
      Last 24 month Average         0.0074
      Last 36 month Average         0.0046
      Last 12 month Std Dev         0.0150
      Last 24 month Std Dev         0.0217
      Last 36 month Std Dev         0.0195
      
      > table.TrailingPeriodsRel(edhec[, 10:12], Rb = edhec[, 
      +     13], periods = c(12, 24, 36), FUNCS = c("cor", "CAPM.beta"), 
      +     funcs.names = c("Correlation", "Beta"))
                                                  Merger Arbitrage Relative Value
      Last 12 month Correlation to Funds of Funds           0.5337         0.8250
      Last 24 month Correlation to Funds of Funds           0.8547         0.9427
      Last 36 month Correlation to Funds of Funds           0.8257         0.9423
      Last 12 month Beta to Funds of Funds                  0.6456         0.3630
      Last 24 month Beta to Funds of Funds                  0.9624         0.7103
      Last 36 month Beta to Funds of Funds                  0.8615         0.6840
                                                  Short Selling
      Last 12 month Correlation to Funds of Funds        0.1892
      Last 24 month Correlation to Funds of Funds       -0.3029
      Last 36 month Correlation to Funds of Funds       -0.2859
      Last 12 month Beta to Funds of Funds               0.1946
      Last 24 month Beta to Funds of Funds              -0.2046
      Last 36 month Beta to Funds of Funds              -0.2260
      
      > result <- table.TrailingPeriods(edhec[, 10:13], periods = c(12, 
      +     24, 36))
      
      > require("Hmisc")
      
      > textplot(format.df(result, na.blank = TRUE, numeric.dollar = FALSE, 
      +     cdec = rep(3, dim(result)[2])), rmar = 0.8, cmar = 1.5, max.cex = 0.9, 
      +     halign = "center", valign = "top", row.valign = "center", 
      +     wrap.rownames = 15, wrap.colnames = 10, mar = c(0, 0, 3, 
      +         0) + 0.1)
      
      > title(main = "Trailing Period Statistics")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for table.SpecificRisk

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > table.SpecificRisk(managers[, 1:8], managers[, 8])
                        HAM1   HAM2   HAM3   HAM4   HAM5   HAM6 EDHEC LS EQ SP500 TR
      Specific Risk   0.0664     NA 0.0946 0.1521     NA     NA          NA     0.00
      Systematic Risk 0.0586 0.0515 0.0836 0.1032 0.0477 0.0486      0.0503     0.15
      Total Risk      0.0886     NA 0.1262 0.1838     NA     NA          NA     0.15
      
      > require("Hmisc")
      
      > result = t(table.SpecificRisk(managers[, 1:8], managers[, 
      +     8], Rf = 0.04/12))
      
      > textplot(format.df(result, na.blank = TRUE, numeric.dollar = FALSE, 
      +     cdec = c(3, 3, 1)), rmar = 0.8, cmar = 2, max.cex = 0.9, 
      +     halign = "center", valign = "top", row.valign = "center", 
      +     wrap.rownames = 20, wrap.colnames = 10, col.rownames = c("red", 
      +         rep("darkgray", 5), rep("orange", 2)), mar = c(0, 0, 
      +         3, 0) + 0.1)
      
      > title(main = "Portfolio specific, systematic and total risk")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for table.Variability

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > table.Variability(managers[, 1:8])
                                HAM1   HAM2   HAM3   HAM4   HAM5   HAM6 EDHEC LS EQ
      Mean Absolute deviation 0.0182 0.0268 0.0268 0.0410 0.0329 0.0187      0.0159
      monthly Std Dev         0.0256 0.0367 0.0365 0.0532 0.0457 0.0238      0.0205
      Annualized Std Dev      0.0888 0.1272 0.1265 0.1843 0.1584 0.0825      0.0708
                              SP500 TR
      Mean Absolute deviation   0.0333
      monthly Std Dev           0.0433
      Annualized Std Dev        0.1500
      
      > require("Hmisc")
      
      > result <- t(table.Variability(managers[, 1:8]))
      
      > textplot(format.df(result, na.blank = TRUE, numeric.dollar = FALSE, 
      +     cdec = c(3, 3, 1)), rmar = 0.8, cmar = 2, max.cex = 0.9, 
      +     halign = "center", valign = "top", row.valign = "center", 
      +     wrap.rownames = 20, wrap.colnames = 10, col.rownames = c("red", 
      +         rep("darkgray", 5), rep("orange", 2)), mar = c(0, 0, 
      +         3, 0) + 0.1)
      
      > title(main = "Portfolio variability")
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for to.period.contributions

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers, package = "PerformanceAnalytics")
      
      > res_qtr_rebal = Return.portfolio(managers["2002::", 
      +     1:5], weights = c(0.05, 0.1, 0.3, 0.4, 0.15), rebalance_on = "quarters", 
      +     verbose = TRUE)
      
      > to.period.contributions(res_qtr_rebal$contribution, 
      +     period = "years")
                         HAM1         HAM2         HAM3        HAM4        HAM5
      2002-12-31 -0.0040 -0.0128 -0.0600 -0.0210 -0.0165
      2003-12-31  0.0124  0.0119  0.0790  0.2203  0.0317
      2004-12-31  0.0071  0.0128  0.0057  0.0565  0.0105
      2005-12-31  0.0040  0.0061  0.0445  0.0402  0.0103
      2006-12-31  0.0099  0.0045  0.0425  0.0528  0.0231
                 Portfolio Return
      2002-12-31      -0.1144
      2003-12-31       0.3556
      2004-12-31       0.0928
      2005-12-31       0.1053
      2006-12-31       0.1331
      
      > to.yearly.contributions(res_qtr_rebal$contribution)
                         HAM1         HAM2         HAM3        HAM4        HAM5
      2002-12-31 -0.0040 -0.0128 -0.0600 -0.0210 -0.0165
      2003-12-31  0.0124  0.0119  0.0790  0.2203  0.0317
      2004-12-31  0.0071  0.0128  0.0057  0.0565  0.0105
      2005-12-31  0.0040  0.0061  0.0445  0.0402  0.0103
      2006-12-31  0.0099  0.0045  0.0425  0.0528  0.0231
                 Portfolio Return
      2002-12-31      -0.1144
      2003-12-31       0.3556
      2004-12-31       0.0928
      2005-12-31       0.1053
      2006-12-31       0.1331
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for unique-comoments

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(managers)
      
      > p <- ncol(edhec)
      
      > m3 <- M3.MM(edhec, as.mat = TRUE)
      
      > m3bis <- M3.vec2mat(M3.MM(edhec, as.mat = FALSE), 
      +     p)
      
      > sum((m3 - m3bis)^2)
      [1] 0
      
      > m4 <- M4.MM(edhec, as.mat = FALSE)
      
      > m4bis <- M4.mat2vec(M4.MM(edhec, as.mat = TRUE))
      
      > sum((m4 - m4bis)^2)
      [1] 0
    Code
      dev.off()
    Output
      pdf 
        2 

# Snapshot for weights

    Code
      pdf(file = NULL)
      suppressWarnings(try({
        source(ex_file, echo = TRUE, max.deparse.length = Inf)
      }, silent = TRUE))
    Output
      
      > data(weights)
      
      > head(weights)
                 Convertible Arbitrage CTA Global Distressed Securities
      2000-01-01            0.0250 0.1460             0.0250
      2001-01-01            0.1578 0.1957             0.0250
      2002-01-01            0.2443 0.0250             0.0250
      2003-01-01            0.2195 0.0659             0.0250
      2004-01-01            0.0978 0.0255             0.1050
      2005-01-01            0.0250 0.0250             0.2445
                 Emerging Markets Equity Market Neutral Event Driven
      2000-01-01            0.025             0.3500        0.025
      2001-01-01            0.025             0.3500        0.025
      2002-01-01            0.025             0.3500        0.025
      2003-01-01            0.025             0.2817        0.025
      2004-01-01            0.025             0.3500        0.025
      2005-01-01            0.025             0.2054        0.025
                 Fixed Income Arbitrage Global Macro Long/Short Equity
      2000-01-01              0.0250        0.025             0.025
      2001-01-01              0.0250        0.025             0.025
      2002-01-01              0.2056        0.025             0.025
      2003-01-01              0.2577        0.025             0.025
      2004-01-01              0.2715        0.025             0.025
      2005-01-01              0.3500        0.025             0.025
                 Merger Arbitrage Relative Value
      2000-01-01       0.0714      0.2575
      2001-01-01       0.1213      0.0250
      2002-01-01       0.0250      0.0250
      2003-01-01       0.0250      0.0250
      2004-01-01       0.0250      0.0250
      2005-01-01       0.0250      0.0250
    Code
      dev.off()
    Output
      pdf 
        2 

