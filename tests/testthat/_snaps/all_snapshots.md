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
      [1] 0.04078
      
      > ActivePremium(managers[, 1, drop = FALSE], managers[, 
      +     8, drop = FALSE])
      [1] 0.04078
      
      > ActivePremium(managers[, 1:6], managers[, 8, drop = FALSE])
                                     HAM1       HAM2       HAM3       HAM4       HAM5
      Active Premium: SP500 TR 0.04078 0.07759 0.05446 0.02473 0.02182
                                     HAM6
      Active Premium: SP500 TR 0.07585
      
      > ActivePremium(managers[, 1:6], managers[, 8:7, drop = FALSE])
                                        HAM1       HAM2       HAM3        HAM4
      Active Premium: SP500 TR    0.04078 0.07759 0.05446  0.02473
      Active Premium: EDHEC LS EQ 0.01965 0.03776 0.01043 -0.00462
                                         HAM5       HAM6
      Active Premium: SP500 TR     0.02182 0.07585
      Active Premium: EDHEC LS EQ -0.03237 0.05463
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
      Annualized Sharpe Ratio (Rf=0%)                    0.75914
      
      > data(managers)
      
      > print(AdjustedSharpeRatio(managers["1996"]))
                                                HAM1    HAM2      HAM3     HAM4 HAM5
      Adjusted Sharpe ratio (Risk free = 0) 2.04596 14.5593 0.93227 1.88336   NA
                                            HAM6 EDHEC LS EQ SP500 TR   US 10Y TR
      Adjusted Sharpe ratio (Risk free = 0)   NA          NA 1.98696 0.00631
                                             US 3m TR
      Adjusted Sharpe ratio (Risk free = 0) -576.9696
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
      [1] -0.43027
      
      > print(AppraisalRatio(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2], method = "modified"))
      [1] -0.01418
      
      > print(AppraisalRatio(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2], method = "alternative"))
      [1] -0.10669
      
      > data(managers)
      
      > print(AppraisalRatio(managers["1996", 1], managers["1996", 
      +     8]))
      [1] 1.62302
      
      > print(AppraisalRatio(managers["1996", 1:5], managers["1996", 
      +     8]))
                                          HAM1 HAM2     HAM3      HAM4 HAM5
      Appraisal ratio (Risk free = 0) 1.62302   NA 3.52772 0.70704   NA
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
      [1] 1.77978
      
      > data(managers)
      
      > print(BernardoLedoitRatio(managers["1996"]))
                                    HAM1 HAM2     HAM3     HAM4 HAM5 HAM6 EDHEC LS EQ
      Bernardo and Ledoit ratio 4.59833 2375 6.48281 3.61507  NaN  NaN         NaN
                                SP500 TR US 10Y TR US 3m TR
      Bernardo and Ledoit ratio 4.34062  1.02827     -Inf
      
      > print(BernardoLedoitRatio(managers["1996", 1]))
      [1] 4.59833
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
      [1] 0.34316
      
      > BetaCoSkewness(managers[, "HAM2", drop = FALSE], managers[, 
      +     "SP500 TR", drop = FALSE])
      [1] 0.04542
      
      > BetaCoKurtosis(managers[, "HAM2", drop = FALSE], managers[, 
      +     "SP500 TR", drop = FALSE])
      [1] 0.19883
      
      > BetaCoKurtosis(managers[, 1:6], managers[, 8, drop = FALSE])
                                     HAM1      HAM2     HAM3      HAM4      HAM5
      Beta Cokurtosis: SP500 TR 0.48146 0.19883 0.50681 0.84835 0.27386
                                     HAM6
      Beta Cokurtosis: SP500 TR 0.15412
      
      > BetaCoKurtosis(managers[, 1:6], managers[, 8:7])
                                        HAM1      HAM2     HAM3      HAM4      HAM5
      Beta Cokurtosis: SP500 TR    0.48146 0.19883 0.50681 0.84835 0.27386
      Beta Cokurtosis: EDHEC LS EQ 0.71005 1.26760 1.42666 1.45330 1.28312
                                        HAM6
      Beta Cokurtosis: SP500 TR    0.15412
      Beta Cokurtosis: EDHEC LS EQ 0.86183
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
      [1] 0.74473
      
      > print(BurkeRatio(portfolio_bacon[, 1], modified = TRUE))
      [1] 3.64842
      
      > data(managers)
      
      > print(BurkeRatio(managers["1996"]))
                                      HAM1 HAM2     HAM3     HAM4 HAM5 HAM6
      Burke ratio (Risk free = 0) 4.77974  Inf 6.34048 4.04815   NA   NA
                                  EDHEC LS EQ SP500 TR   US 10Y TR US 3m TR
      Burke ratio (Risk free = 0)          NA 4.73982 0.00613      Inf
      
      > print(BurkeRatio(managers["1996", 1]))
      [1] 4.77974
      
      > print(BurkeRatio(managers["1996"], modified = TRUE))
                                               HAM1 HAM2     HAM3     HAM4 HAM5 HAM6
      Modified Burke ratio (Risk free = 0) 16.55753  Inf 21.96408 14.02321   NA   NA
                                           EDHEC LS EQ SP500 TR  US 10Y TR US 3m TR
      Modified Burke ratio (Risk free = 0)          NA 16.41925 0.02125      Inf
      
      > print(BurkeRatio(managers["1996", 1], modified = TRUE))
      [1] 16.55753
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
      Capital Market Line Slope: SP500 TR 0.12558
      
      > CAPM.CML(managers[, "HAM1", drop = FALSE], managers[, 
      +     "SP500 TR", drop = FALSE], Rf = 0)
      [1] 0.00222
      
      > CAPM.RiskPremium(managers[, "SP500 TR", drop = FALSE], 
      +     Rf = 0)
                              SP500 TR
      Risk Premium (Rf=0%) 0.00866
      
      > CAPM.RiskPremium(managers[, "HAM1", drop = FALSE], 
      +     Rf = 0)
                                 HAM1
      Risk Premium (Rf=0%) 0.01112
      
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
      HAM1 to SP500 TR     0.00709                -0.19635               0.16653
                       Average beta US 10Y TR beta at t - 1 US 3m TR beta at t - 1
      HAM1 to SP500 TR    0.32480                3.49333              -63.74814
      
      > CAPM.dynamic(managers[80:120, 1:6], managers[80:120, 
      +     7, drop = FALSE], Rf = managers[80:120, 10, drop = FALSE], 
      +     Z = managers[80:120, 9:10])
                          Average alpha US 10Y TR alpha at t - 1
      HAM1 to EDHEC LS EQ -0.00017              -0.23890
      HAM2 to EDHEC LS EQ -0.00276              -0.06632
      HAM3 to EDHEC LS EQ  0.00626              -0.21733
      HAM4 to EDHEC LS EQ -0.00332               0.16135
      HAM5 to EDHEC LS EQ  0.00433               0.26882
      HAM6 to EDHEC LS EQ -0.00538               0.05000
                          US 3m TR alpha at t - 1 Average beta
      HAM1 to EDHEC LS EQ              -0.43850    1.17930
      HAM2 to EDHEC LS EQ              -4.01769    0.70673
      HAM3 to EDHEC LS EQ               7.68048    0.42606
      HAM4 to EDHEC LS EQ              -0.20918    1.63676
      HAM5 to EDHEC LS EQ               3.84971    1.22245
      HAM6 to EDHEC LS EQ              -3.06643    1.62819
                          US 10Y TR beta at t - 1 US 3m TR beta at t - 1
      HAM1 to EDHEC LS EQ                3.86121              -51.01409
      HAM2 to EDHEC LS EQ                5.68208              171.16658
      HAM3 to EDHEC LS EQ                1.50791             -705.20354
      HAM4 to EDHEC LS EQ               -7.62213             -565.85196
      HAM5 to EDHEC LS EQ                7.08395               39.70358
      HAM6 to EDHEC LS EQ              -11.03513              343.52891
      
      > CAPM.dynamic(managers[80:120, 1:6], managers[80:120, 
      +     8:7], managers[80:120, 10, drop = FALSE], Z = managers[80:120, 
      +     9:10])
                          Average alpha US 10Y TR alpha at t - 1
      HAM1 to SP500 TR     0.00363              -0.03538
      HAM2 to SP500 TR     0.00169              -0.05484
      HAM3 to SP500 TR     0.00726              -0.05978
      HAM4 to SP500 TR    -0.00158               0.41314
      HAM5 to SP500 TR     0.00833               0.35300
      HAM6 to SP500 TR     0.00128               0.03521
      HAM1 to EDHEC LS EQ -0.00017              -0.23890
      HAM2 to EDHEC LS EQ -0.00276              -0.06632
      HAM3 to EDHEC LS EQ  0.00626              -0.21733
      HAM4 to EDHEC LS EQ -0.00332               0.16135
      HAM5 to EDHEC LS EQ  0.00433               0.26882
      HAM6 to EDHEC LS EQ -0.00538               0.05000
                          US 3m TR alpha at t - 1 Average beta
      HAM1 to SP500 TR                 0.08506   0.51861
      HAM2 to SP500 TR                -2.91835   0.05157
      HAM3 to SP500 TR                 4.10231   0.17720
      HAM4 to SP500 TR                -6.04090   1.20562
      HAM5 to SP500 TR                 1.56695   0.57212
      HAM6 to SP500 TR                -1.72313   0.59611
      HAM1 to EDHEC LS EQ             -0.43850   1.17930
      HAM2 to EDHEC LS EQ             -4.01769   0.70673
      HAM3 to EDHEC LS EQ              7.68048   0.42606
      HAM4 to EDHEC LS EQ             -0.20918   1.63676
      HAM5 to EDHEC LS EQ              3.84971   1.22245
      HAM6 to EDHEC LS EQ             -3.06643   1.62819
                          US 10Y TR beta at t - 1 US 3m TR beta at t - 1
      HAM1 to SP500 TR                  -1.18105              -65.73676
      HAM2 to SP500 TR                   2.07553              -23.79983
      HAM3 to SP500 TR                   1.06335             -256.19346
      HAM4 to SP500 TR                  -1.81221              162.03456
      HAM5 to SP500 TR                   4.27730              183.06200
      HAM6 to SP500 TR                  -5.10631              189.51371
      HAM1 to EDHEC LS EQ                3.86121              -51.01409
      HAM2 to EDHEC LS EQ                5.68208              171.16658
      HAM3 to EDHEC LS EQ                1.50791             -705.20354
      HAM4 to EDHEC LS EQ               -7.62213             -565.85196
      HAM5 to EDHEC LS EQ                7.08395               39.70358
      HAM6 to EDHEC LS EQ              -11.03513              343.52891
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
      [1] -0.01313
      
      > data(managers)
      
      > print(SFM.epsilon(managers["1996", 1], managers["1996", 
      +     8]))
      [1] 0.07425
      
      > print(SFM.epsilon(managers["1996", 1:5], managers["1996", 
      +     8]))
                                               HAM1      HAM2      HAM3       HAM4
      Regression epsilon (Risk free = 0) 0.07425 0.53991 0.20480 0.05570
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
      [1] -0.01416
      
      > data(managers)
      
      > print(SFM.jensenAlpha(managers["1996", 1], managers["1996", 
      +     8]))
      [1] 0.08077
      
      > print(SFM.jensenAlpha(managers["1996", 1:5], managers["1996", 
      +     8]))
                                           HAM1 HAM2      HAM3       HAM4 HAM5
      Jensen's Alpha (Risk free = 0) 0.08077   NA 0.21960 0.06063   NA
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
                             Conditional Drawdown 5%
      Convertible Arbitrage                   0.0706
      CTA Global                              0.0734
      Distressed Securities                   0.1326
      Emerging Markets                        0.1587
      Equity Market Neutral                   0.0320
      Event Driven                            0.1052
      Fixed Income Arbitrage                  0.0362
      Global Macro                            0.0375
      Long/Short Equity                       0.1076
      Merger Arbitrage                        0.0325
      Relative Value                          0.0462
      Short Selling                           0.6322
      Funds of Funds                          0.0761
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
             5% 
      0.11437 
      
      > CDaR.alpha(edhec[, 1], edhec[, 2], type = "max")
            max 
      0.11145 
      
      > CDaR.alpha(edhec[, 1], edhec[, 2], type = "average")
        average 
      0.12170 
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
              5% 
      -0.80315 
      
      > CDaR.beta(edhec[, 1], edhec[, 2], type = "max")
             max 
      -0.74806 
      
      > CDaR.beta(edhec[, 1], edhec[, 2], type = "average")
         average 
      -0.94129 
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
      Calmar Ratio 0.90616
      
      > CalmarRatio(managers[, 1:6])
                        HAM1     HAM2      HAM3      HAM4      HAM5     HAM6
      Calmar Ratio 0.90616 0.72809 0.52258 0.42273 0.10959 1.74252
      
      > SterlingRatio(managers[, 1, drop = FALSE])
                                         HAM1
      Sterling Ratio (Excess = 10%) 0.54625
      
      > SterlingRatio(managers[, 1:6])
                                         HAM1      HAM2      HAM3      HAM4
      Sterling Ratio (Excess = 10%) 0.54625 0.51387 0.38836 0.31360
                                          HAM5      HAM6
      Sterling Ratio (Excess = 10%) 0.08471 0.76784
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
      [1] 0.00066
      
      > CoSkewness(managers[, "HAM2", drop = FALSE], managers[, 
      +     "SP500 TR", drop = FALSE])
      [1] -2.10128e-06
      
      > CoKurtosis(managers[, "HAM2", drop = FALSE], managers[, 
      +     "SP500 TR", drop = FALSE])
      [1] 2.57906e-06
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
      [1] 0.40133
      
      > data(managers)
      
      > print(DRatio(managers["1996"]))
                    HAM1         HAM2       HAM3      HAM4 HAM5 HAM6 EDHEC LS EQ
      d ratio 0.07248 0.00010 0.03085 0.13830  NaN  NaN         NaN
                SP500 TR US 10Y TR US 3m TR
      d ratio 0.04607  1.36150        0
      
      > print(DRatio(managers["1996", 1]))
      [1] 0.07248
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
      [1,] 0.02553
      
      > DownsidePotential(portfolio_bacon[, 1], MAR)
                 [,1]
      [1,] 0.01370
      
      > data(managers)
      
      > apply(managers[, 1:6], 2, sd, na.rm = TRUE)
            HAM1       HAM2       HAM3       HAM4       HAM5       HAM6 
      0.02562 0.03671 0.03651 0.05319 0.04573 0.02381 
      
      > DownsideDeviation(managers[, 1:6])
                                          HAM1      HAM2       HAM3       HAM4
      Downside Deviation (MAR = 0%) 0.01454 0.01157 0.01735 0.03406
                                         HAM5       HAM6
      Downside Deviation (MAR = 0%) 0.03043 0.01214
      
      > DownsideDeviation(managers[, 1:6], MAR = 0.04/12)
                                         HAM1       HAM2       HAM3       HAM4
      Downside Deviation (MAR = 0%) 0.01576 0.01341 0.01891 0.03565
                                         HAM5       HAM6
      Downside Deviation (MAR = 0%) 0.03206 0.01366
      
      > SemiDeviation(managers[, 1, drop = FALSE])
                          HAM1
      Semi-Deviation 0.01907
      
      > SemiDeviation(managers[, 1:6])
                          HAM1       HAM2       HAM3       HAM4       HAM5       HAM6
      Semi-Deviation 0.01907 0.02011 0.02369 0.03950 0.03244 0.01751
      
      > SemiVariance(managers[, 1, drop = FALSE])
                            HAM1
      Semi-Variance 0.00072
      
      > SemiVariance(managers[, 1:6])
                            HAM1         HAM2        HAM3        HAM4        HAM5
      Semi-Variance 0.00072 0.00066 0.00101 0.00332 0.00207
                           HAM6
      Semi-Variance 0.00067
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
      [1] 0.45833
      
      > data(managers)
      
      > print(DownsideFrequency(managers["1996"]))
                                    HAM1 HAM2      HAM3      HAM4 HAM5 HAM6
      Downside Frequency (MAR = 0%) 0.25  0.2 0.16666 0.33333  NaN  NaN
                                    EDHEC LS EQ  SP500 TR US 10Y TR US 3m TR
      Downside Frequency (MAR = 0%)         NaN 0.16666 0.58333        0
      
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
      Downside Sharpe Ratio 0.30018 0.19516 0.33159 0.18664 0.47209
                                   ED       FIA       GM        LS        MA
      Downside Sharpe Ratio 0.30741 0.31550 0.42622 0.30552 0.44288
                                   RV          SS       FOF
      Downside Sharpe Ratio 0.41687 -0.03014 0.26681
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
      ES              -0.03878   -0.04062              -0.04132      -0.07544
         Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      ES              -0.01752  -0.04445            -0.02906  -0.02109
         Long/Short Equity Merger Arbitrage Relative Value Short Selling
      ES       -0.04481      -0.02333    -0.02712   -0.09484
         Funds of Funds
      ES    -0.03569
      
      > ES(edhec, p = 0.95, method = "gaussian")
         Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      ES           -0.02872 -0.04260            -0.03053      -0.06062
         Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      ES           -0.01256  -0.03259            -0.01916  -0.02451
         Long/Short Equity Merger Arbitrage Relative Value Short Selling
      ES       -0.03632      -0.01805    -0.01871   -0.09495
         Funds of Funds
      ES    -0.02861
      
      > ES(edhec, p = 0.95, method = "modified")
         Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      ES           -0.08941 -0.04038           -0.06732       -0.11570
         Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      ES           -0.03670  -0.08113            -0.05308  -0.01696
         Long/Short Equity Merger Arbitrage Relative Value Short Selling
      ES       -0.04857      -0.05130    -0.04751   -0.06754
         Funds of Funds
      ES    -0.04590
      
      > ES(edhec, p = 0.99)
         Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      ES           -0.09538 -0.05203           -0.07097       -0.12613
         Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      ES           -0.03875  -0.08433            -0.06036  -0.02309
         Long/Short Equity Merger Arbitrage Relative Value Short Selling
      ES       -0.06580      -0.05760    -0.04882    -0.19411
         Funds of Funds
      ES    -0.05423
      
      > ES(edhec, p = 0.01)
         Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      ES           -0.09538 -0.05203           -0.07097       -0.12613
         Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      ES           -0.03875  -0.08433            -0.06036  -0.02309
         Long/Short Equity Merger Arbitrage Relative Value Short Selling
      ES       -0.06580      -0.05760    -0.04882    -0.19411
         Funds of Funds
      ES    -0.05423
      
      > if (requireNamespace("robustbase", quietly = TRUE)) {
      +     ES(edhec, clean = "boudt")
      +     ES(edhec, clean = "boudt", portfolio_method = "component")
      + }
    Message
      no weights passed in, assuming equal weighted portfolio
    Output
      $MES
      [1] 0.02311
      
      $contribution
       Convertible Arbitrage             CTA Global  Distressed Securities 
                0.00278          -0.00147           0.00292 
            Emerging Markets  Equity Market Neutral           Event Driven 
                0.00526           0.00091           0.00339 
      Fixed Income Arbitrage           Global Macro      Long/Short Equity 
                0.00227           0.00079           0.00303 
            Merger Arbitrage         Relative Value          Short Selling 
                0.00152           0.00200          -0.00343 
              Funds of Funds 
                0.00310 
      
      $pct_contrib_MES
       Convertible Arbitrage             CTA Global  Distressed Securities 
                  0.12034            -0.06387             0.12672 
            Emerging Markets  Equity Market Neutral           Event Driven 
                  0.22781             0.03962             0.14686 
      Fixed Income Arbitrage           Global Macro      Long/Short Equity 
                  0.09830             0.03418             0.13146 
            Merger Arbitrage         Relative Value          Short Selling 
                  0.06580             0.08684            -0.14839 
              Funds of Funds 
                  0.13427 
      
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
      [1] 0.03549
      
      $contribution
       [1]  3.83954e-03 -2.22354e-03  6.61048e-03  6.50602e-03  8.65328e-04
       [6]  7.27639e-03  2.32450e-03 -7.84254e-05  3.12801e-03  3.93234e-03
      [11]  3.63082e-03 -3.90560e-03  3.58830e-03
      
      $pct_contrib_MES
       [1]  0.10817 -0.06264  0.18624  0.18329  0.02437
       [6]  0.20500  0.06548 -0.00220  0.08812  0.11078
      [11]  0.10229 -0.11003  0.10109
      
      
      > sigma <- cov(edhec)
      
      > m3 <- M3.MM(edhec)
      
      > m4 <- M4.MM(edhec)
      
      > ES(p = 0.95, portfolio_method = "component", weights = rep(1/p, 
      +     p), mu = mu, sigma = sigma, m3 = m3, m4 = m4)
      $MES
      [1] 0.03625
      
      $contribution
       [1]  0.00597 -0.00280  0.00528  0.00625  0.00090
       [6]  0.00589  0.00346  0.00013  0.00362  0.00346
      [11]  0.00377 -0.00315  0.00343
      
      $pct_contrib_MES
       [1]  0.16485 -0.07746  0.14569  0.17246  0.02501
       [6]  0.16268  0.09552  0.00369  0.10002  0.09545
      [11]  0.10420 -0.08699  0.09484
      
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
      portfolio.monthly.return....                     1.03039
      
      > data(managers)
      
      > print(FamaBeta(managers["1996", 1], managers["1996", 
      +     8]))
                HAM1
      HAM1 0.53512
      
      > print(FamaBeta(managers["1996", 1:5], managers["1996", 
      +     8]))
                      HAM1 HAM2     HAM3     HAM4 HAM5
      Fama Beta  0.53512   NA 1.00708 1.03763   NA
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
      [1] 0.36041
      
      > InformationRatio(managers[, 1:6], managers[, 8, drop = FALSE])
                                       HAM1      HAM2      HAM3     HAM4      HAM5
      Information Ratio: SP500 TR 0.36041 0.50597 0.47010 0.15491 0.12121
                                       HAM6
      Information Ratio: SP500 TR 0.67228
      
      > InformationRatio(managers[, 1:6], managers[, 8:7])
                                          HAM1      HAM2      HAM3       HAM4
      Information Ratio: SP500 TR    0.36041 0.50597 0.47010  0.15491
      Information Ratio: EDHEC LS EQ 0.25937 0.41627 0.12793 -0.02948
                                           HAM5      HAM6
      Information Ratio: SP500 TR     0.12121 0.67228
      Information Ratio: EDHEC LS EQ -0.22774 0.96672
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
      [1] 0.15663
      
      > data(managers)
      
      > MAR = 0
      
      > print(Kappa(managers["1996"], MAR, l))
                           HAM1     HAM2     HAM3    HAM4 HAM5 HAM6 EDHEC LS EQ
      kappa (MAR = 0%) 1.49206 1061.685 2.23519 1.14188  NaN  NaN         NaN
                       SP500 TR  US 10Y TR US 3m TR
      kappa (MAR = 0%) 1.27433 0.01674      Inf
      
      > print(Kappa(managers["1996", 1], MAR, l))
      [1] 1.49206
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
      Kelly Ratio 5.92948
      
      > KellyRatio(managers[, 1, drop = FALSE], Rf = managers[, 
      +     10, drop = FALSE])
                      HAM1
      Kelly Ratio 6.01085
      
      > KellyRatio(managers[, 1:6], Rf = managers[, 10, drop = FALSE])
                      HAM1     HAM2     HAM3     HAM4      HAM5     HAM6
      Kelly Ratio 6.01085 4.06987 3.45812 1.37635 0.38764 7.94829
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
      Sortino Ratio (MAR = 0.5%)                    0.10347
      
      > data(managers)
      
      > MAR = 0
      
      > print(MSquaredExcess(managers["1996", 1], managers["1996", 
      +     8], MAR))
                 SP500 TR
      SP500 TR 0.02027
      
      > print(MSquaredExcess(managers["1996", 1:5], managers["1996", 
      +     8], MAR))
                                           HAM1 HAM2      HAM3        HAM4 HAM5
      MSquaredExcess (Risk free = 0) 0.02027   NA 0.14095 -0.02546   NA
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
      benchmark.return....              0.10062
      
      > data(managers)
      
      > print(MSquared(managers["1996", 1], managers["1996", 
      +     8]))
                SP500 TR
      SP500 TR 0.25448
      
      > print(MSquared(managers["1996", 1:5], managers["1996", 
      +     8]))
                                    HAM1 HAM2      HAM3      HAM4 HAM5
      MSquared (Risk free = 0) 0.25448   NA 0.40287 0.19824   NA
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
      benchmark.return....          -0.01553
      
      > MSquaredExcess(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2], Method = "arithmetic")
                           benchmark.return....
      benchmark.return....          -0.01736
      
      > data(managers)
      
      > MSquaredExcess(managers["1996", 1], managers["1996", 
      +     8])
                 SP500 TR
      SP500 TR 0.02027
      
      > MSquaredExcess(managers["1996", 1:5], managers["1996", 
      +     8])
                                           HAM1 HAM2      HAM3        HAM4 HAM5
      MSquaredExcess (Risk free = 0) 0.02027   NA 0.14095 -0.02546   NA
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
                             Alpha      Beta     Gamma
      HAM1 to SP500 TR 0.00827 0.32114 0.13444
      
      > MarketTiming(managers[80:120, 1:6], managers[80:120, 
      +     7], managers[80:120, 10])
                                  Alpha      Beta     Gamma
      HAM1 to EDHEC LS EQ -0.00057 1.31210 -0.40515
      HAM2 to EDHEC LS EQ -0.00036 0.43709  8.52062
      HAM3 to EDHEC LS EQ -0.00581 1.18982 11.91378
      HAM4 to EDHEC LS EQ -0.00551 2.06165 18.79734
      HAM5 to EDHEC LS EQ  0.00051 1.07037 -5.07788
      HAM6 to EDHEC LS EQ  0.00035 1.27110 -7.44342
      
      > MarketTiming(managers[80:120, 1:6], managers[80:120, 
      +     8:7], managers[80:120, 10], method = "TM")
                                  Alpha      Beta      Gamma
      HAM1 to SP500 TR     0.00488 0.59701 -0.28016
      HAM2 to SP500 TR     0.00506 0.11904 -0.50002
      HAM3 to SP500 TR     0.00321 0.52729 -0.66456
      HAM4 to SP500 TR     0.00946 0.87795 -0.81551
      HAM5 to SP500 TR     0.00872 0.28699 -2.77280
      HAM6 to SP500 TR     0.00480 0.29022  0.69108
      HAM1 to EDHEC LS EQ -0.00057 1.31210 -0.40515
      HAM2 to EDHEC LS EQ -0.00036 0.43709  8.52061
      HAM3 to EDHEC LS EQ -0.00581 1.18982 11.91378
      HAM4 to EDHEC LS EQ -0.00551 2.06165 18.79733
      HAM5 to EDHEC LS EQ  0.00051 1.07037 -5.07788
      HAM6 to EDHEC LS EQ  0.00035 1.27110 -7.44342
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
      Ulcer Index                      1.70772
      
      > data(managers)
      
      > print(MartinRatio(managers["1996"]))
                                HAM1     HAM2     HAM3     HAM4 HAM5 HAM6 EDHEC LS EQ
      Martin Ratio (Rf = 0) 15.49888 16390.63 19.24429 13.67313   NA   NA          NA
                            SP500 TR  US 10Y TR US 3m TR
      Martin Ratio (Rf = 0) 14.81878 0.01003      Inf
      
      > print(MartinRatio(managers["1996", 1]))
                      HAM1
      Ulcer Index 15.49888
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
      [1] 0.03108
      
      > data(managers)
      
      > print(MeanAbsoluteDeviation(managers["1996"]))
                                   HAM1     HAM2       HAM3       HAM4 HAM5 HAM6
      Mean absolute deviation 0.01253 0.03157 0.02229 0.02540  NaN  NaN
                              EDHEC LS EQ SP500 TR  US 10Y TR US 3m TR
      Mean absolute deviation         NaN  0.02225 0.01611  0.00021
      
      > print(MeanAbsoluteDeviation(managers["1996", 1]))
      [1] 0.01253
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
      [1] 52.37369
      
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
      [1] 0.01678
      
      > Modigliani(managers[, 1:6], managers[, 8, drop = FALSE], 
      +     managers[, 8, drop = FALSE])
                                                    HAM1       HAM2      HAM3
      Modigliani-Modigliani measure: SP500 TR 0.01281 0.01505 0.01315
                                                    HAM4       HAM5       HAM6
      Modigliani-Modigliani measure: SP500 TR 0.01057 0.01053 0.01844
      
      > Modigliani(managers[, 1:6], managers[, 8:7], managers[, 
      +     8, drop = FALSE])
                                                       HAM1       HAM2       HAM3
      Modigliani-Modigliani measure: SP500 TR    0.01281 0.01505 0.01315
      Modigliani-Modigliani measure: EDHEC LS EQ 0.01062 0.01168 0.01078
                                                       HAM4        HAM5       HAM6
      Modigliani-Modigliani measure: SP500 TR    0.01057 0.01053 0.01844
      Modigliani-Modigliani measure: EDHEC LS EQ 0.00956 0.00954 0.01328
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
      portfolio.monthly.return....                   -0.01789
      
      > data(managers)
      
      > print(NetSelectivity(managers["1996", 1], managers["1996", 
      +     8]))
                 HAM1
      HAM1 0.01333
      
      > print(NetSelectivity(managers["1996", 1:5], managers["1996", 
      +     8]))
                                            HAM1 HAM2      HAM3        HAM4 HAM5
      Net Selectivity (Risk free = 0) 0.01333   NA 0.17453 -0.03249   NA
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
      Omega (L = 0%)              2.84849   1.61855              2.75658
                     Emerging Markets Equity Market Neutral Event Driven
      Omega (L = 0%)         1.75295              4.29178     2.63012
                     Fixed Income Arbitrage Global Macro Long/Short Equity
      Omega (L = 0%)               3.36904      2.89794          2.31443
                     Merger Arbitrage Relative Value Short Selling Funds of Funds
      Omega (L = 0%)         3.95536       3.66201     0.92479       2.18566
      
      > if (requireNamespace("Hmisc", quietly = TRUE)) {
      +     Omega(edhec[, 13], method = "interp", output = "point")
      +     Omega(edhec[, 13], method = "interp", output = "full")
      + }
              Funds of Funds
      -0.0705    292.00000
      -0.0705    292.00000
      -0.0618    218.75000
      -0.0616    166.42857
      -0.06      132.18181
      -0.0272    102.41176
      -0.0269     81.04000
      -0.0266     67.94117
      -0.0264     58.93181
      -0.0262     51.32142
      -0.0252     45.71014
      -0.0222     41.36144
      -0.0205     37.86734
      -0.0202     34.98245
      -0.0192     32.54961
      -0.0176     30.46308
      -0.0163     28.64880
      -0.0156     27.05319
      -0.0149     25.63636
      -0.0148     24.36796
      -0.0142     23.22440
      -0.0141     22.18705
      -0.014      21.16776
      -0.0138     20.24471
      -0.0133     19.34722
      -0.0132     18.53333
      -0.0127     17.79097
      -0.0126     17.11037
      -0.0122     16.48353
      -0.0119     15.90384
      -0.0116     15.36576
      -0.0108     14.86463
      -0.0104     14.39649
      -0.0099     13.95795
      -0.0095     13.52549
      -0.0093     13.12048
      -0.0089     12.74017
      -0.0083     12.38221
      -0.0082     12.04452
      -0.0079     11.72529
      -0.0077     11.42295
      -0.0074     11.13609
      -0.0072     10.86346
      -0.0071     10.59352
      -0.007      10.33705
      -0.0069     10.09300
      -0.0068      9.83477
      -0.0063      9.58239
      -0.0062      9.34365
      -0.0059      9.11740
      -0.0054      8.90258
      -0.0049      8.69828
      -0.0044      8.49785
      -0.004       8.30158
      -0.0037      8.09937
      -0.0036      7.90287
      -0.0034      7.71659
      -0.0033      7.52684
      -0.0031      7.34717
      -0.0028      7.17674
      -0.0027      7.01479
      -0.0025      6.86066
      -0.0022      6.71052
      -0.0021      6.56739
      -0.0019      6.43074
      -0.0018      6.30011
      -0.0015      6.17507
      -0.0012      6.05524
      -0.001       5.94026
      -9e-04       5.82983
      -7e-04       5.72148
      -6e-04       5.61731
      -5e-04       5.51706
      -4e-04       5.42049
      -3e-04       5.32738
      -2e-04       5.23753
      1e-04        5.15076
      4e-04        5.06689
      6e-04        4.98422
      8e-04        4.90428
      9e-04        4.82690
      0.0013       4.74922
      0.0015       4.67405
      0.0017       4.60127
      0.0018       4.52952
      0.0019       4.46002
      0.0021       4.39149
      0.0022       4.32177
      0.0024       4.25428
      0.0025       4.18788
      0.0026       4.12257
      0.0028       4.05930
      0.003        3.99706
      0.0031       3.93672
      0.0032       3.87819
      0.0033       3.82139
      0.0034       3.76542
      0.0035       3.71107
      0.0037       3.65676
      0.0039       3.60329
      0.004        3.55066
      0.0041       3.49887
      0.0043       3.44789
      0.0046       3.39838
      0.005        3.35025
      0.0051       3.30345
      0.0052       3.25733
      0.0053       3.21246
      0.0057       3.16877
      0.0058       3.12623
      0.0059       3.08477
      0.006        3.04436
      0.0064       3.00495
      0.0066       2.96604
      0.0067       2.92807
      0.0068       2.89012
      0.0069       2.85266
      0.007        2.81611
      0.0071       2.78003
      0.0072       2.74480
      0.0073       2.71002
      0.0075       2.67568
      0.0076       2.64177
      0.0077       2.60866
      0.0078       2.57631
      0.0079       2.54469
      0.008        2.51312
      0.0082       2.48194
      0.0083       2.45115
      0.0085       2.42105
      0.0086       2.39162
      0.0088       2.36283
      0.0089       2.33467
      0.009        2.30655
      0.0091       2.27876
      0.0092       2.25157
      0.0093       2.22495
      0.0094       2.19889
      0.0095       2.17312
      0.0096       2.14763
      0.0097       2.12220
      0.0099       2.09706
      0.0104       2.07245
      0.0106       2.04811
      0.0108       2.02427
      0.0109       2.00070
      0.0111       1.97760
      0.0113       1.95475
      0.0114       1.93216
      0.0119       1.91001
      0.0121       1.88811
      0.0125       1.86644
      0.0126       1.84520
      0.0127       1.82436
      0.013        1.80391
      0.0131       1.78384
      0.0133       1.76415
      0.0134       1.74448
      0.0136       1.72518
      0.0137       1.70622
      0.0138       1.68761
      0.0139       1.66932
      0.014        1.65136
      0.0142       1.63370
      0.0145       1.61635
      0.0147       1.59915
      0.0148       1.58224
      0.0152       1.56562
      0.0153       1.54926
      0.0156       1.53318
      0.0157       1.51735
      0.016        1.50178
      0.0163       1.48633
      0.0164       1.47113
      0.0169       1.45616
      0.0171       1.44108
      0.0172       1.42624
      0.0175       1.41163
      0.0182       1.39724
      0.0185       1.38308
      0.0189       1.36913
      0.0191       1.35538
      0.0199       1.34185
      0.0202       1.32851
      0.0203       1.31536
      0.0204       1.30240
      0.0205       1.28954
      0.0206       1.27685
      0.0209       1.26435
      0.0213       1.25202
      0.0216       1.23986
      0.0217       1.22787
      0.0219       1.21604
      0.022        1.20437
      0.0222       1.19286
      0.0223       1.18150
      0.0225       1.17028
      0.0233       1.15922
      0.0244       1.14829
      0.0256       1.13751
      0.0267       1.12686
      0.0274       1.11635
      0.0275       1.10597
      0.0282       1.09571
      0.0286       1.08559
      0.0303       1.07558
      0.0311       1.06569
      0.0312       1.05593
      0.0313       1.04628
      0.0317       1.03674
      0.0334       1.02731
      0.034        1.01799
      0.0373       1.00878
      0.0384       0.99968
      0.04         0.99067
      0.0435       0.98177
      0.0483       0.97297
      0.0622       0.96426
      0.0666       0.95565
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
      [1,] 0.08053
      
      > data(managers)
      
      > MAR = 0
      
      > print(OmegaExcessReturn(managers["1996", 1], managers["1996", 
      +     8], MAR))
                [,1]
      [1,] 0.13253
      
      > print(OmegaExcessReturn(managers["1996", 1:5], managers["1996", 
      +     8], MAR))
                                         HAM1 HAM2      HAM3      HAM4 HAM5
      Omega Excess Return (MAR = 0) 0.13253   NA 0.39914 0.19857   NA
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
      [1,] 0.29179
      
      > MAR = 0
      
      > data(managers)
      
      > print(OmegaSharpeRatio(managers["1996"], MAR))
                                      HAM1 HAM2     HAM3     HAM4 HAM5 HAM6
      OmegaSharpeRatio (MAR = 0%) 3.59833 2374 5.48281 2.61507   NA   NA
                                  EDHEC LS EQ SP500 TR  US 10Y TR US 3m TR
      OmegaSharpeRatio (MAR = 0%)          NA 3.34062 0.02827      Inf
      
      > print(OmegaSharpeRatio(managers["1996", 1], MAR))
               [,1]
      [1,] 3.59833
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
      Pain Index                    0.03901
      
      > data(managers)
      
      > print(PainIndex(100 * managers["1996"]))
                      HAM1  HAM2      HAM3      HAM4 HAM5 HAM6 EDHEC LS EQ  SP500 TR
      Pain Index 0.37140 0.002 0.94217 0.74216  NaN  NaN         NaN 0.73360
                 US 10Y TR US 3m TR
      Pain Index  3.69796        0
      
      > print(PainIndex(100 * managers["1996", 1]))
                      HAM1
      Pain Index 0.37140
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
      Pain Index                     2.65764
      
      > data(managers)
      
      > print(PainRatio(managers["1996"]))
                             HAM1     HAM2     HAM3     HAM4 HAM5 HAM6 EDHEC LS EQ
      Pain Ratio (Rf = 0) 36.7226 36650.56 43.38967 28.17458   NA   NA          NA
                          SP500 TR  US 10Y TR US 3m TR
      Pain Ratio (Rf = 0) 31.62377 0.01188      Inf
      
      > print(PainRatio(managers["1996", 1]))
                    HAM1
      Pain Index 36.7226
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
      Probabilistic Sharpe Ratio(p= 95 %):                          0.92093
      
      $sr_confidence_interval
                            Lower Bound Sharpe Ratio Upper Bound
      Convertible Arbitrage      0.2109       0.3455      0.4802
      
      
      > ProbSharpeRatio(refSR = 1/12^0.5, Rf = 0, p = 0.95, 
      +     sr = 2/12^0.5, sk = -0.72, kr = 5.78, n = 59)
      $sr_prob
      [1] 0.95974
      
      $sr_confidence_interval
        Lower Bound Sharpe Ratio Upper Bound
      1      0.3057       0.5774       0.849
      
      
      > ProbSharpeRatio(edhec[, 1:2], refSR = c(0.28, 0.24), 
      +     ignore_skewness = FALSE, ignore_kurtosis = FALSE)
      $sr_prob
      [1] 0.76011
      
      $sr_confidence_interval
        Lower Bound Sharpe Ratio Upper Bound
      1       0.193       0.3455      0.4981
      
      
      > ProbSharpeRatio(edhec[, 1:2], refSR = c(0.28, 0.24), 
      +     ignore_skewness = FALSE, ignore_kurtosis = TRUE)
      $sr_prob
      [1] 0.78833
      
      $sr_confidence_interval
        Lower Bound Sharpe Ratio Upper Bound
      1      0.2109       0.3455      0.4802
      
      
      > ProbSharpeRatio(edhec[, 1:2], refSR = c(0.28, 0.24), 
      +     ignore_skewness = TRUE, ignore_kurtosis = TRUE)
      $sr_prob
      [1] 0.86171
      
      $sr_confidence_interval
        Lower Bound Sharpe Ratio Upper Bound
      1      0.2465       0.3455      0.4446
      
      
      > ProbSharpeRatio(edhec[, 1:2], refSR = 0.26, weights = c(0.5, 
      +     0.5), ignore_skewness = FALSE, ignore_kurtosis = FALSE)
      $sr_prob
                                           portfolio.returns (SR > 0.26 )
      Probabilistic Sharpe Ratio(p= 95 %):                      0.95522
      
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
      [1,] -0.13470
      
      > data(managers)
      
      > MAR = 0
      
      > print(ProspectRatio(managers["1996"], MAR))
                                     HAM1     HAM2     HAM3      HAM4 HAM5 HAM6
      Prospect ratio (MAR = 0%) 0.97374 442.1359 1.72560 0.59606   NA   NA
                                EDHEC LS EQ  SP500 TR  US 10Y TR US 3m TR
      Prospect ratio (MAR = 0%)          NA 0.79750 -0.72345      Inf
      
      > print(ProspectRatio(managers["1996", 1], MAR))
                [,1]
      [1,] 0.97374
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
      RachevRatio 1.17941 1.33800 1.13938 0.99673 1.51418 1.08917 1.13516
                        GM       LS     MA       RV       SS      FOF
      RachevRatio 2.01437 1.23869 1.5023 1.15946 1.13450 1.22554
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
      1996-02-29  0.02207            NA  0.03510
      1996-03-31  0.01461            NA  0.02573
      1996-04-30 -0.01483            NA  0.04503
      1996-05-31  0.01149            NA  0.03523
      1996-06-30 -0.00658            NA -0.03076
      1996-07-31 -0.02757            NA -0.03372
      1996-08-31  0.05409            NA  0.04666
      1996-09-30  0.00891  0.12487  0.06543
      1996-10-31  0.03208  0.01746  0.03931
      1996-11-30  0.01252  0.08351  0.06679
      1996-12-31  0.01806  0.01899  0.02107
      1997-01-31  0.02203  0.09160  0.07749
      1997-02-28 -0.00222 -0.02975 -0.03821
      1997-03-31  0.01107 -0.03150 -0.03357
      1997-04-30  0.01334 -0.00098  0.02904
      1997-05-31  0.05107  0.06866  0.07623
      1997-06-30  0.01827  0.05551  0.00489
      1997-07-31  0.01360  0.12971  0.10882
      1997-08-31  0.02563 -0.05284 -0.00358
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
      Annualized Return 0.03718
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
      Cumulative Return 3.12667
      
      > Return.cumulative(managers[, 1:8])
                            HAM1     HAM2     HAM3    HAM4      HAM5      HAM6
      Cumulative Return 3.12667 4.34859 3.70673 2.52944 0.26501 0.98586
                        EDHEC LS EQ SP500 TR
      Cumulative Return    2.05119 1.76161
      
      > Return.cumulative(managers[, 1:8], geometric = FALSE)
                          HAM1   HAM2  HAM3   HAM4   HAM5   HAM6 EDHEC LS EQ SP500 TR
      Cumulative Return 1.4682 1.7679 1.643 1.4542 0.3148 0.7075      1.1454 1.14382
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
      1996-01-31         0.00284
      1996-02-29         0.01532
      1996-03-31         0.01179
      1996-04-30        -0.01338
      1996-05-31         0.00317
      1996-06-30        -0.00802
      
      > head(Return.excess(managers[, 1, drop = FALSE], 0.04/12))
                    HAM1 > Rf
      1996-01-31  0.00406
      1996-02-29  0.01596
      1996-03-31  0.01216
      1996-04-30 -0.01243
      1996-05-31  0.00426
      1996-06-30 -0.00723
      
      > head(Return.excess(managers[, 1:6], managers[, 10, 
      +     drop = FALSE]))
                 HAM1 > US 3m TR HAM2 > US 3m TR HAM3 > US 3m TR HAM4 > US 3m TR
      1996-01-31         0.00284              NA         0.03034         0.01764
      1996-02-29         0.01532              NA         0.03112         0.01552
      1996-03-31         0.01179              NA         0.02209        -0.01351
      1996-04-30        -0.01338              NA         0.04062         0.01932
      1996-05-31         0.00317              NA         0.03087        -0.00163
      1996-06-30        -0.00802              NA        -0.03442        -0.00602
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
      1997-01-31       0.03340
      1997-02-28       0.02376
      1997-03-31      -0.00141
      1997-04-30       0.00368
      1997-05-31       0.01766
      1997-06-30       0.02545
      1997-07-31       0.03650
      1997-08-31      -0.00513
      1997-09-30       0.02204
      1997-10-31      -0.01078
      1997-11-30      -0.00262
      1997-12-31       0.01298
      
      > Return.portfolio(edhec["1997", 1:5], rebalance_on = "quarters", 
      +     verbose = TRUE)
      $returns
                 portfolio.returns
      1997-01-31       0.03340
      1997-02-28       0.02376
      1997-03-31      -0.00141
      1997-04-30       0.00368
      1997-05-31       0.01766
      1997-06-30       0.02545
      1997-07-31       0.03650
      1997-08-31      -0.00513
      1997-09-30       0.02204
      1997-10-31      -0.01078
      1997-11-30      -0.00262
      1997-12-31       0.01298
      
      $contribution
                 Convertible Arbitrage    CTA Global Distressed Securities
      1997-01-31           0.00238  0.00786          0.00356
      1997-02-28           0.00240  0.00599          0.00240
      1997-03-31           0.00151 -0.00042         -0.00023
      1997-04-30           0.00172 -0.00340          0.00060
      1997-05-31           0.00313 -0.00029          0.00465
      1997-06-30           0.00425  0.00163          0.00436
      1997-07-31           0.00386  0.01182          0.00468
      1997-08-31           0.00263 -0.00966          0.00290
      1997-09-30           0.00244  0.00387          0.00704
      1997-10-31           0.00200 -0.00196         -0.00128
      1997-11-30           0.00000  0.00266          0.00108
      1997-12-31           0.00139  0.00581          0.00147
                 Emerging Markets Equity Market Neutral
      1997-01-31      0.01582          0.00378
      1997-02-28      0.01096          0.00199
      1997-03-31     -0.00257          0.00031
      1997-04-30      0.00238          0.00238
      1997-05-31      0.00635          0.00381
      1997-06-30      0.01187          0.00333
      1997-07-31      0.01120          0.00494
      1997-08-31     -0.00134          0.00033
      1997-09-30      0.00465          0.00402
      1997-10-31     -0.01144          0.00190
      1997-11-30     -0.00720          0.00083
      1997-12-31      0.00294          0.00135
      
      $BOP.Weight
                 Convertible Arbitrage CTA Global Distressed Securities
      1997-01-31             0.20000  0.20000             0.20000
      1997-02-28             0.19583  0.20114             0.19698
      1997-03-31             0.19364  0.20232             0.19475
      1997-04-30             0.20000  0.20000             0.20000
      1997-05-31             0.20098  0.19587             0.19986
      1997-06-30             0.20057  0.19219             0.20097
      1997-07-31             0.20000  0.20000             0.20000
      1997-08-31             0.19668  0.20436             0.19747
      1997-09-30             0.20034  0.19569             0.20140
      1997-10-31             0.20000  0.20000             0.20000
      1997-11-30             0.20420  0.20019             0.20088
      1997-12-31             0.20473  0.20339             0.20250
                 Emerging Markets Equity Market Neutral
      1997-01-31        0.20000             0.20000
      1997-02-28        0.20884             0.19719
      1997-03-31        0.21470             0.19456
      1997-04-30        0.20000             0.20000
      1997-05-31        0.20163             0.20163
      1997-06-30        0.20438             0.20188
      1997-07-31        0.20000             0.20000
      1997-08-31        0.20376             0.19772
      1997-09-30        0.20346             0.19908
      1997-10-31        0.20000             0.20000
      1997-11-30        0.19061             0.20410
      1997-12-31        0.18389             0.20547
      
      $EOP.Weight
                 Convertible Arbitrage CTA Global Distressed Securities
      1997-01-31             0.19583  0.20114             0.19698
      1997-02-28             0.19364  0.20232             0.19475
      1997-03-31             0.19543  0.20218             0.19479
      1997-04-30             0.20098  0.19587             0.19986
      1997-05-31             0.20057  0.19219             0.20097
      1997-06-30             0.19974  0.18901             0.20023
      1997-07-31             0.19668  0.20436             0.19747
      1997-08-31             0.20034  0.19569             0.20140
      1997-09-30             0.19841  0.19526             0.20396
      1997-10-31             0.20420  0.20019             0.20088
      1997-11-30             0.20473  0.20339             0.20250
      1997-12-31             0.20348  0.20652             0.20136
                 Emerging Markets Equity Market Neutral
      1997-01-31        0.20884             0.19719
      1997-02-28        0.21470             0.19456
      1997-03-31        0.21243             0.19514
      1997-04-30        0.20163             0.20163
      1997-05-31        0.20438             0.20188
      1997-06-30        0.21088             0.20012
      1997-07-31        0.20376             0.19772
      1997-08-31        0.20346             0.19908
      1997-09-30        0.20363             0.19872
      1997-10-31        0.19061             0.20410
      1997-11-30        0.18389             0.20547
      1997-12-31        0.18443             0.20418
      
      $BOP.Value
                 Convertible Arbitrage CTA Global Distressed Securities
      1997-01-31             0.20000  0.20000             0.20000
      1997-02-28             0.20238  0.20786             0.20356
      1997-03-31             0.20486  0.21405             0.20604
      1997-04-30             0.21129  0.21129             0.21129
      1997-05-31             0.21310  0.20770             0.21192
      1997-06-30             0.21643  0.20738             0.21686
      1997-07-31             0.22130  0.22130             0.22130
      1997-08-31             0.22557  0.23438             0.22648
      1997-09-30             0.22860  0.22330             0.22981
      1997-10-31             0.23323  0.23323             0.23323
      1997-11-30             0.23557  0.23095             0.23174
      1997-12-31             0.23557  0.23402             0.23299
                 Emerging Markets Equity Market Neutral
      1997-01-31        0.20000             0.20000
      1997-02-28        0.21582             0.20378
      1997-03-31        0.22715             0.20583
      1997-04-30        0.21129             0.21129
      1997-05-31        0.21380             0.21380
      1997-06-30        0.22054             0.21784
      1997-07-31        0.22130             0.22130
      1997-08-31        0.23370             0.22677
      1997-09-30        0.23215             0.22715
      1997-10-31        0.23323             0.23323
      1997-11-30        0.21989             0.23545
      1997-12-31        0.21158             0.23642
      
      $EOP.Value
                 Convertible Arbitrage CTA Global Distressed Securities
      1997-01-31             0.20238  0.20786             0.20356
      1997-02-28             0.20486  0.21405             0.20604
      1997-03-31             0.20646  0.21360             0.20579
      1997-04-30             0.21310  0.20770             0.21192
      1997-05-31             0.21643  0.20738             0.21686
      1997-06-30             0.22102  0.20915             0.22156
      1997-07-31             0.22557  0.23438             0.22648
      1997-08-31             0.22860  0.22330             0.22981
      1997-09-30             0.23139  0.22772             0.23785
      1997-10-31             0.23557  0.23095             0.23174
      1997-11-30             0.23557  0.23402             0.23299
      1997-12-31             0.23717  0.24071             0.23469
                 Emerging Markets Equity Market Neutral
      1997-01-31        0.21582             0.20378
      1997-02-28        0.22715             0.20583
      1997-03-31        0.22442             0.20616
      1997-04-30        0.21380             0.21380
      1997-05-31        0.22054             0.21784
      1997-06-30        0.23335             0.22144
      1997-07-31        0.23370             0.22677
      1997-08-31        0.23215             0.22715
      1997-09-30        0.23747             0.23174
      1997-10-31        0.21989             0.23545
      1997-11-30        0.21158             0.23642
      1997-12-31        0.21497             0.23798
      
      
      > data(weights)
      
      > chart.StackedBar(weights)
      
      > x <- Return.portfolio(edhec["2000::", 1:11], weights = weights, 
      +     verbose = TRUE)
      
      > chart.CumReturns(x$returns)
      
      > chart.StackedBar(x$BOP.Weight)
      
      > chart.StackedBar(x$BOP.Value)
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
      1996-01-31     0.97427            NA      1.00087
      1996-02-29     0.98392            NA      1.02645
      1996-03-31     0.98967            NA      1.04292
      1996-04-30     0.96646            NA      1.07396
      1996-05-31     0.94931            NA      1.08391
      1996-06-30     0.94203            NA      1.04709
      1996-07-31     0.96283            NA      1.05859
      1996-08-31     0.98018     0.97923      1.08451
      1996-09-30     0.94157     1.01993      1.09375
      1996-10-31     0.94267     1.02608      1.10641
      1996-11-30     0.89009     1.02427      1.09715
      1996-12-31     0.92405     1.07610      1.14327
      1997-01-31     0.88813     1.09322      1.15898
      1997-02-28     0.88320     1.07586      1.10700
      1997-03-31     0.92971     1.09179      1.11566
      1997-04-30     0.88839     1.02400      1.08292
      1997-05-31     0.87407     1.01724      1.09823
      1997-06-30     0.85591     1.02737      1.05681
      1997-07-31     0.80502     1.06106      1.08471
      1997-08-31     0.87298     1.10186      1.14584
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
      [1] 0.00577
      
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
      [1] 0.39007
      
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
      [1] 0.52265
      
      > SFM.beta.bear(managers[, "HAM2"], managers[, "SP500 TR"], 
      +     Rf = managers[, "US 3m TR"])
      [1] 0.06982
      
      > TimingRatio(managers[, "HAM2"], managers[, "SP500 TR"], 
      +     Rf = managers[, "US 3m TR"])
      [1] 7.48522
      
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
      [1] 19.26492
      
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
      [1] -0.01416
      
      > data(managers)
      
      > print(Selectivity(managers["2002", 1], managers["2002", 
      +     8]))
      [1] 0.03780
      
      > print(Selectivity(managers["2002", 1:5], managers["2002", 
      +     8]))
                                           HAM1       HAM2        HAM3       HAM4
      Jensen's Alpha (Risk free = 0) 0.03780 -0.10158 -0.09419 0.09430
                                            HAM5
      Jensen's Alpha (Risk free = 0) -0.08667
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
      StdDev Sharpe (Rf=0.3%, p=95%): 0.32018
      
      > SharpeRatio(managers[, 1, drop = FALSE], Rf = managers[, 
      +     10, drop = FALSE], FUN = "StdDev")
                                          HAM1
      StdDev Sharpe (Rf=0.3%, p=95%): 0.30810
      
      > SharpeRatio(managers[, 1:6], Rf = 0.035/12, FUN = "StdDev")
                                           HAM1      HAM2      HAM3      HAM4
      StdDev Sharpe (Rf=0.3%, p=95%): 0.32018 0.30576 0.26101 0.15226
                                            HAM5      HAM6
      StdDev Sharpe (Rf=0.3%, p=95%): 0.02562 0.34175
      
      > SharpeRatio(managers[, 1:6], Rf = managers[, 10, drop = FALSE], 
      +     FUN = "StdDev")
                                          HAM1      HAM2      HAM3      HAM4
      StdDev Sharpe (Rf=0.3%, p=95%): 0.30810 0.29886 0.25253 0.14643
                                           HAM5      HAM6
      StdDev Sharpe (Rf=0.3%, p=95%): 0.03545 0.37853
      
      > data(edhec)
      
      > SharpeRatio(edhec[, 6, drop = FALSE], FUN = "VaR")
                                 Event Driven
      VaR Sharpe (Rf=0%, p=95%):    0.22540
      
      > SharpeRatio(edhec[, 6, drop = FALSE], Rf = 0.04/12, 
      +     FUN = "VaR")
                                   Event Driven
      VaR Sharpe (Rf=0.3%, p=95%):    0.11282
      
      > SharpeRatio(edhec[, 6, drop = FALSE], Rf = 0.04/12, 
      +     FUN = "VaR", method = "gaussian")
                                   Event Driven
      VaR Sharpe (Rf=0.3%, p=95%):     0.13556
      
      > SharpeRatio(edhec[, 6, drop = FALSE], FUN = "ES")
                                Event Driven
      ES Sharpe (Rf=0%, p=95%):   0.08225
      
      > SharpeRatio(managers[, 1:9], Rf = managers[, 10, drop = FALSE])
                                           HAM1      HAM2      HAM3       HAM4
      StdDev Sharpe (Rf=0.3%, p=95%): 0.30810 0.29886 0.25253 0.14643
      VaR Sharpe (Rf=0.3%, p=95%):    0.23068 0.39706 0.25049 0.09553
      ES Sharpe (Rf=0.3%, p=95%):     0.12950 0.17882 0.20933 0.06625
      SemiSD Sharpe (Rf=0.3%, p=95%): 0.29393        NA 0.27566 0.13937
                                            HAM5      HAM6 EDHEC LS EQ   SP500 TR
      StdDev Sharpe (Rf=0.3%, p=95%): 0.03545 0.37853   0.31426 0.12558
      VaR Sharpe (Rf=0.3%, p=95%):    0.02399 0.30229   0.27376 0.07957
      ES Sharpe (Rf=0.3%, p=95%):     0.01664 0.23084   0.18558 0.05760
      SemiSD Sharpe (Rf=0.3%, p=95%):         NA        NA          NA 0.11811
                                       US 10Y TR
      StdDev Sharpe (Rf=0.3%, p=95%): 0.05684
      VaR Sharpe (Rf=0.3%, p=95%):    0.03741
      ES Sharpe (Rf=0.3%, p=95%):     0.02610
      SemiSD Sharpe (Rf=0.3%, p=95%): 0.05544
      
      > SharpeRatio(edhec, Rf = 0.04/12)
                                      Convertible Arbitrage CTA Global
      StdDev Sharpe (Rf=0.3%, p=95%):            0.14668 0.04318
      VaR Sharpe (Rf=0.3%, p=95%):               0.09573 0.03071
      ES Sharpe (Rf=0.3%, p=95%):                0.02749 0.02436
      SemiSD Sharpe (Rf=0.3%, p=95%):            0.12742 0.04448
                                      Distressed Securities Emerging Markets
      StdDev Sharpe (Rf=0.3%, p=95%):            0.19243       0.10385
      VaR Sharpe (Rf=0.3%, p=95%):               0.12468       0.06357
      ES Sharpe (Rf=0.3%, p=95%):                0.05185       0.02935
      SemiSD Sharpe (Rf=0.3%, p=95%):            0.16964       0.09420
                                      Equity Market Neutral Event Driven
      StdDev Sharpe (Rf=0.3%, p=95%):            0.12208   0.17516
      VaR Sharpe (Rf=0.3%, p=95%):               0.09119   0.11282
      ES Sharpe (Rf=0.3%, p=95%):                0.02730   0.04117
      SemiSD Sharpe (Rf=0.3%, p=95%):            0.10912   0.15387
                                      Fixed Income Arbitrage Global Macro
      StdDev Sharpe (Rf=0.3%, p=95%):             0.09571    0.15484
      VaR Sharpe (Rf=0.3%, p=95%):                0.06182    0.16400
      ES Sharpe (Rf=0.3%, p=95%):                 0.02066    0.13349
      SemiSD Sharpe (Rf=0.3%, p=95%):             0.07810    0.17242
                                      Long/Short Equity Merger Arbitrage
      StdDev Sharpe (Rf=0.3%, p=95%):        0.16187       0.19589
      VaR Sharpe (Rf=0.3%, p=95%):           0.11467       0.14961
      ES Sharpe (Rf=0.3%, p=95%):            0.06966       0.04383
      SemiSD Sharpe (Rf=0.3%, p=95%):        0.15390       0.17840
                                      Relative Value Short Selling Funds of Funds
      StdDev Sharpe (Rf=0.3%, p=95%):     0.20179   -0.10095     0.07325
      VaR Sharpe (Rf=0.3%, p=95%):        0.13789   -0.07391     0.05102
      ES Sharpe (Rf=0.3%, p=95%):         0.05040   -0.06801     0.02566
      SemiSD Sharpe (Rf=0.3%, p=95%):     0.17429   -0.10985     0.06968
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
      Annualized Sharpe Ratio (Rf=3.5%) 1.11229
      
      > SharpeRatio.annualized(managers[, 1, drop = FALSE], 
      +     Rf = managers[, 10, drop = FALSE])
                                            HAM1
      Annualized Sharpe Ratio (Rf=3.9%) 1.06679
      
      > SharpeRatio.annualized(managers[, 1:6], Rf = 0.035/12)
                                            HAM1     HAM2      HAM3      HAM4
      Annualized Sharpe Ratio (Rf=3.5%) 1.11229 1.05908 0.88543 0.45125
                                              HAM5     HAM6
      Annualized Sharpe Ratio (Rf=3.5%) 0.01046 1.19414
      
      > SharpeRatio.annualized(managers[, 1:6], Rf = managers[, 
      +     10, drop = FALSE])
                                            HAM1     HAM2      HAM3      HAM4
      Annualized Sharpe Ratio (Rf=3.9%) 1.06679 1.03301 0.85397 0.42921
                                              HAM5     HAM6
      Annualized Sharpe Ratio (Rf=3.9%) 0.04441 1.33386
      
      > SharpeRatio.annualized(managers[, 1:6], Rf = managers[, 
      +     10, drop = FALSE], geometric = FALSE)
                                            HAM1     HAM2      HAM3      HAM4
      Annualized Sharpe Ratio (Rf=3.9%) 1.06729 1.03528 0.87479 0.50727
                                             HAM5     HAM6
      Annualized Sharpe Ratio (Rf=3.9%) 0.12282 1.31129
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
      [1] 0.03609
      
      $contribution
       [1]  0.00473 -0.00045  0.00453  0.00658  0.00112
       [6]  0.00498  0.00285  0.00119  0.00378  0.00266
      [11]  0.00315 -0.00245  0.00340
      
      $pct_contrib_MES
       [1]  0.13113 -0.01266  0.12564  0.18246  0.03104  0.13797
       [7]  0.07896  0.03304  0.10489  0.07378  0.08732 -0.06794
      [13]  0.09432
      
      
      > sigma <- cov(edhec)
      
      > m3 <- M3.MM(edhec)
      
      > m4 <- M4.MM(edhec)
      
      > ES(p = 0.95, portfolio_method = "component", weights = rep(1/p, 
      +     p), mu = mu, sigma = sigma, m3 = m3, m4 = m4)
      $MES
      [1] 0.03625
      
      $contribution
       [1]  0.00597 -0.00280  0.00528  0.00625  0.00090
       [6]  0.00589  0.00346  0.00013  0.00362  0.00346
      [11]  0.00377 -0.00315  0.00343
      
      $pct_contrib_MES
       [1]  0.16485 -0.07746  0.14569  0.17246  0.02501
       [6]  0.16268  0.09552  0.00369  0.10002  0.09545
      [11]  0.10420 -0.08699  0.09484
      
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
      [1] -0.03394
      
      > data(managers)
      
      > print(SkewnessKurtosisRatio(managers["1996"]))
                                  HAM1      HAM2       HAM3       HAM4 HAM5 HAM6
      SkewnessKurtosisRatio -0.13641 0.12790 -0.33226 -0.02646   NA   NA
                            EDHEC LS EQ    SP500 TR   US 10Y TR   US 3m TR
      SkewnessKurtosisRatio          NA -0.03981 -0.01634 -0.26267
      
      > print(SkewnessKurtosisRatio(managers["1996", 1]))
      [1] -0.13641
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
      0.62110 
      
      > SmoothingIndex(managers[, 1:8])
                          HAM1      HAM2      HAM3      HAM4 HAM5      HAM6
      Smoothing Index 0.62110 0.49203 0.66298 0.67105    1 0.49292
                      EDHEC LS EQ  SP500 TR
      Smoothing Index   0.53789 0.95284
      
      > SmoothingIndex(edhec)
                      Convertible Arbitrage CTA Global Distressed Securities
      Smoothing Index             0.42930  0.94935             0.45124
                      Emerging Markets Equity Market Neutral Event Driven
      Smoothing Index        0.56260             0.47561    0.53629
                      Fixed Income Arbitrage Global Macro Long/Short Equity
      Smoothing Index              0.43120    0.66319         0.59350
                      Merger Arbitrage Relative Value Short Selling Funds of Funds
      Smoothing Index        0.52454      0.46320     0.75356      0.54741
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
      [1] 0.03293
      
      > data(managers)
      
      > print(SpecificRisk(managers["1996", 1], managers["1996", 
      +     8]))
      [1] 0.04977
      
      > print(SpecificRisk(managers["1996", 1:5], managers["1996", 
      +     8]))
                             HAM1 HAM2       HAM3      HAM4 HAM5
      Specific Risk =  0.04977   NA 0.06225 0.08576   NA
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
      StdDev            0.01676 0.02278            0.01814       0.03270
             Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      StdDev           0.00820   0.01907             0.01145   0.01462
             Long/Short Equity Merger Arbitrage Relative Value Short Selling
      StdDev        0.02090       0.01147     0.01186    0.04550
             Funds of Funds
      StdDev     0.01608
      
      > StdDev(edhec, portfolio_method = "single")
             Convertible Arbitrage CTA Global Distressed Securities Emerging Markets
      StdDev            0.01676 0.02278            0.01814       0.03270
             Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      StdDev           0.00820   0.01907             0.01145   0.01462
             Long/Short Equity Merger Arbitrage Relative Value Short Selling
      StdDev        0.02090       0.01147     0.01186    0.04550
             Funds of Funds
      StdDev     0.01608
      
      > StdDev(edhec, clean = "boudt")
             Convertible Arbitrage CTA Global Distressed Securities Emerging Markets
      StdDev            0.01399 0.02278            0.01649       0.03081
             Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      StdDev           0.00739   0.01751            0.00951   0.01431
             Long/Short Equity Merger Arbitrage Relative Value Short Selling
      StdDev         0.02052       0.01037     0.01059     0.04352
             Funds of Funds
      StdDev     0.01589
      
      > StdDev(edhec, clean = "boudt", portfolio_method = "component")
    Message
      no weights passed in, assuming equal weighted portfolio
    Output
      $StdDev
      [1] 0.01016
      
      $contribution
       Convertible Arbitrage             CTA Global  Distressed Securities 
                0.00081           0.00064           0.00104 
            Emerging Markets  Equity Market Neutral           Event Driven 
                0.00192           0.00042           0.00117 
      Fixed Income Arbitrage           Global Macro      Long/Short Equity 
                0.00053           0.00090           0.00129 
            Merger Arbitrage         Relative Value          Short Selling 
                0.00057           0.00070          -0.00096 
              Funds of Funds 
                0.00109 
      
      $pct_contrib_StdDev
       Convertible Arbitrage             CTA Global  Distressed Securities 
                  0.08002             0.06383             0.10267 
            Emerging Markets  Equity Market Neutral           Event Driven 
                  0.18893             0.04206             0.11576 
      Fixed Income Arbitrage           Global Macro      Long/Short Equity 
                  0.05245             0.08865             0.12755 
            Merger Arbitrage         Relative Value          Short Selling 
                  0.05679             0.06926            -0.09529 
              Funds of Funds 
                  0.10728 
      
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
      Annualized Standard Deviation              0.05806 0.07894
                                    Distressed Securities Emerging Markets
      Annualized Standard Deviation            0.06285        0.11330
                                    Equity Market Neutral Event Driven
      Annualized Standard Deviation            0.02843   0.06606
                                    Fixed Income Arbitrage Global Macro
      Annualized Standard Deviation             0.03969   0.05066
                                    Long/Short Equity Merger Arbitrage Relative Value
      Annualized Standard Deviation        0.07241       0.03976     0.04111
                                    Short Selling Funds of Funds
      Annualized Standard Deviation     0.15762     0.05571
      
      > sd.annualized(edhec[, 6, drop = FALSE])
                                    Event Driven
      Annualized Standard Deviation   0.06606
      
      > sd.multiperiod(edhec[, 6, drop = FALSE], scale = 3)
                                    Event Driven
      Annualized Standard Deviation   0.03303
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
      [1] 0.00666
      
      $contribution
       [1] -2.83800e-04  1.07218e-03 -5.28465e-06  2.06902e-03 -4.84545e-04
       [6]  2.44437e-05 -5.79406e-04 -1.63968e-05  5.29764e-04 -4.91059e-04
      [11] -4.30305e-04  5.15166e-03  1.04734e-04
      
      $pct_contrib_MES
       [1] -0.04260  0.16096 -0.00079  0.31061 -0.07274
       [6]  0.00366 -0.08698 -0.00246  0.07953 -0.07372
      [11] -0.06460  0.77340  0.01572
      
      
      > sigma <- cov(edhec)
      
      > m3 <- M3.MM(edhec)
      
      > m4 <- M4.MM(edhec)
      
      > ES(p = 0.95, portfolio_method = "component", weights = rep(1/p, 
      +     p), mu = mu, sigma = sigma, m3 = m3, m4 = m4)
      $MES
      [1] 0.03625
      
      $contribution
       [1]  0.00597 -0.00280  0.00528  0.00625  0.00090
       [6]  0.00589  0.00346  0.00013  0.00362  0.00346
      [11]  0.00377 -0.00315  0.00343
      
      $pct_contrib_MES
       [1]  0.16485 -0.07746  0.14569  0.17246  0.02501
       [6]  0.16268  0.09552  0.00369  0.10002  0.09545
      [11]  0.10420 -0.08699  0.09484
      
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
      [1] 0.13280
      
      > data(managers)
      
      > print(SystematicRisk(managers["2002", 1], managers["2002", 
      +     8]))
      [1] 0.11036
      
      > print(SystematicRisk(managers["2002", 1:5], managers["2002", 
      +     8]))
                                                HAM1       HAM2       HAM3      HAM4
      Systematic Risk to SP500 TR (Rf = 0) 0.11036 0.02041 0.08939 0.16512
                                                 HAM5
      Systematic Risk to SP500 TR (Rf = 0) 0.02013
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
      [1] 0.13682
      
      > data(managers)
      
      > print(TotalRisk(managers["1996", 1], managers["1996", 
      +     8]))
      [1] 0.05627
      
      > print(TotalRisk(managers["1996", 1:5], managers["1996", 
      +     8]))
                          HAM1 HAM2      HAM3      HAM4 HAM5
      Total Risk =  0.05627   NA 0.10799 0.10993   NA
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
      [1] 0.11316
      
      > TrackingError(managers[, 1:6], managers[, 8, drop = FALSE])
                                    HAM1      HAM2      HAM3      HAM4      HAM5
      Tracking Error: SP500 TR 0.11316 0.15336 0.11586 0.15966 0.18002
                                   HAM6
      Tracking Error: SP500 TR 0.11283
      
      > TrackingError(managers[, 1:6], managers[, 8:7, drop = FALSE])
                                        HAM1       HAM2       HAM3      HAM4
      Tracking Error: SP500 TR    0.11316 0.15336 0.11586 0.15966
      Tracking Error: EDHEC LS EQ 0.07577 0.09071 0.08156 0.15690
                                       HAM5       HAM6
      Tracking Error: SP500 TR    0.18002 0.11283
      Tracking Error: EDHEC LS EQ 0.14215 0.05651
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
      [1] 0.78067
      
      > print(TreynorRatio(managers["2002", 1], managers["2002", 
      +     8], modified = TRUE))
      [1] -0.72754
      
      > print(TreynorRatio(managers["2002", 1:5], managers["2002", 
      +     8], modified = TRUE))
                                   HAM1      HAM2      HAM3       HAM4     HAM5
      Treynor Ratio: SP500 TR -0.72754 -6.04516 -2.12383 -0.49903 -5.37482
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
      SP500 TR Up Capture   0.63466
      SP500 TR Down Capture 0.20763
      SP500 TR Up Number    0.89411
      SP500 TR Down Number  0.51063
      SP500 TR Up Percent   0.29411
      SP500 TR Down Percent 0.80851
      
      > UpDownRatios(managers[, 1:6, drop = FALSE], managers[, 
      +     8, drop = FALSE])
                                 HAM1       HAM2      HAM3      HAM4      HAM5
      SP500 TR Up Capture   0.63466 0.66155 0.79139 0.92723 0.55296
      SP500 TR Down Capture 0.20763 0.04392 0.36693 0.70078 0.35203
      SP500 TR Up Number    0.89411 0.68354 0.87058 0.76470 0.70833
      SP500 TR Down Number  0.51063 0.69565 0.76595 0.65957 0.72413
      SP500 TR Up Percent   0.29411 0.36708 0.42352 0.45882 0.39583
      SP500 TR Down Percent 0.80851 0.86956 0.80851 0.53191 0.82758
                                 HAM6
      SP500 TR Up Capture   0.80684
      SP500 TR Down Capture 0.23977
      SP500 TR Up Number    0.83720
      SP500 TR Down Number  0.52380
      SP500 TR Up Percent   0.53488
      SP500 TR Down Percent 0.71428
      
      > UpDownRatios(managers[, 1, drop = FALSE], managers[, 
      +     8, drop = FALSE], method = "Capture")
                                 HAM1
      SP500 TR Up Capture   0.63466
      SP500 TR Down Capture 0.20763
      
      > UpDownRatios(managers[, 1, drop = FALSE], managers[, 
      +     8, drop = FALSE], side = "Up", method = "Capture")
      [1] 0.63466
      
      > UpDownRatios(managers[, 1, drop = FALSE], managers[, 
      +     8, drop = FALSE], side = "Down", method = "Capture")
      [1] 0.20763
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
      [1] 0.54166
      
      > data(managers)
      
      > print(UpsideFrequency(managers["1996"]))
                                  HAM1 HAM2      HAM3      HAM4 HAM5 HAM6 EDHEC LS EQ
      Upside Frequency (MAR = 0%) 0.75  0.8 0.83333 0.66666  NaN  NaN         NaN
                                   SP500 TR US 10Y TR US 3m TR
      Upside Frequency (MAR = 0%) 0.83333 0.41666        1
      
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
      Upside Potential (MAR = 0.4%)    0.54484
      
      > UpsidePotentialRatio(edhec[, 1:6], MAR = 0)
                                  Convertible Arbitrage CTA Global
      Upside Potential (MAR = 0%)             0.49885   1.05520
                                  Distressed Securities Emerging Markets
      Upside Potential (MAR = 0%)             0.69864        0.60747
                                  Equity Market Neutral Event Driven
      Upside Potential (MAR = 0%)             0.60772    0.59382
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
      [1] 0.02937
      
      > print(UpsideRisk(portfolio_bacon[, 1], MAR, stat = "variance"))
      [1] 0.00086
      
      > print(UpsideRisk(portfolio_bacon[, 1], MAR, stat = "potential"))
      [1] 0.01770
      
      > MAR = 0
      
      > data(managers)
      
      > print(UpsideRisk(managers["1996"], MAR, stat = "risk"))
                                   HAM1       HAM2       HAM3      HAM4 HAM5 HAM6
      Upside Risk (MAR = 0%) 0.01799 0.05916 0.04002 0.03217    0    0
                             EDHEC LS EQ   SP500 TR  US 10Y TR    US 3m TR
      Upside Risk (MAR = 0%)           0 0.03204 0.01397 0.00432
      
      > print(UpsideRisk(managers["1996", 1], MAR, stat = "risk"))
      [1] 0.01799
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
      VaR              -0.01506   -0.03148              -0.01978         -0.04232
          Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      VaR              -0.00836     -0.02552               -0.00722     -0.01494
          Long/Short Equity Merger Arbitrage Relative Value Short Selling
      VaR          -0.02622         -0.01066       -0.01144      -0.06678
          Funds of Funds
      VaR       -0.02032
      
      > VaR(edhec, p = 0.95, method = "gaussian")
          Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      VaR           -0.02173 -0.03310           -0.02296      -0.04698
          Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      VaR          -0.00914  -0.02464            -0.01438  -0.01841
          Long/Short Equity Merger Arbitrage Relative Value Short Selling
      VaR       -0.02760      -0.01326    -0.01376   -0.07597
          Funds of Funds
      VaR    -0.02190
      
      > VaR(edhec, p = 0.95, method = "modified")
          Convertible Arbitrage CTA Global Distressed Securities Emerging Markets
      VaR           -0.02568 -0.03204           -0.02800      -0.05343
          Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      VaR            -0.01098  -0.02960            -0.01773  -0.01380
          Long/Short Equity Merger Arbitrage Relative Value Short Selling
      VaR       -0.02950      -0.01502    -0.01736   -0.06215
          Funds of Funds
      VaR    -0.02309
      
      > VaR(edhec, p = 0.99)
          Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      VaR           -0.09538 -0.04561           -0.07097       -0.12613
          Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      VaR           -0.03875  -0.08433            -0.06036  -0.02309
          Long/Short Equity Merger Arbitrage Relative Value Short Selling
      VaR       -0.05658      -0.05760    -0.04882    -0.10938
          Funds of Funds
      VaR    -0.05423
      
      > VaR(edhec, p = 0.01)
          Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      VaR           -0.09538 -0.04561           -0.07097       -0.12613
          Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      VaR           -0.03875  -0.08433            -0.06036  -0.02309
          Long/Short Equity Merger Arbitrage Relative Value Short Selling
      VaR       -0.05658      -0.05760    -0.04882    -0.10938
          Funds of Funds
      VaR    -0.05423
      
      > VaR(edhec, clean = "boudt")
          Convertible Arbitrage CTA Global Distressed Securities Emerging Markets
      VaR           -0.01713 -0.03204            -0.02272      -0.04687
          Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      VaR          -0.00847  -0.02485            -0.01355   -0.01463
          Long/Short Equity Merger Arbitrage Relative Value Short Selling
      VaR       -0.02811      -0.01116    -0.01348   -0.06821
          Funds of Funds
      VaR    -0.02236
      
      > VaR(edhec, clean = "boudt", portfolio_method = "component")
    Message
      no weights passed in, assuming equal weighted portfolio
    Output
      $MVaR
      [1] 0.01241
      
      $contribution
       Convertible Arbitrage             CTA Global  Distressed Securities 
                0.00110           0.00039           0.00145 
            Emerging Markets  Equity Market Neutral           Event Driven 
                0.00286           0.00044           0.00166 
      Fixed Income Arbitrage           Global Macro      Long/Short Equity 
                0.00077           0.00089           0.00174 
            Merger Arbitrage         Relative Value          Short Selling 
                0.00059           0.00092          -0.00207 
              Funds of Funds 
                0.00162 
      
      $pct_contrib_MVaR
       Convertible Arbitrage             CTA Global  Distressed Securities 
                  0.08939             0.03212             0.11727 
            Emerging Markets  Equity Market Neutral           Event Driven 
                  0.23087             0.03593             0.13412 
      Fixed Income Arbitrage           Global Macro      Long/Short Equity 
                  0.06221             0.07227             0.14029 
            Merger Arbitrage         Relative Value          Short Selling 
                  0.04758             0.07426            -0.16723 
              Funds of Funds 
                  0.13088 
      
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
      [1,] 1.32304
      
      > print(VolatilitySkewness(portfolio_bacon[, 1], MAR, 
      +     stat = "variability"))
               [,1]
      [1,] 1.15023
      
      > MAR = 0
      
      > data(managers)
      
      > print(VolatilitySkewness(managers["1996", 1], MAR, 
      +     stat = "volatility"))
               [,1]
      [1,] 6.14942
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
      0.00740 0.01335 0.01406 0.00827 0.00814 0.00613 
       1996-07-31  1996-08-31  1996-09-30  1996-10-31  1996-11-30  1996-12-31 
      0.00195 0.00665 0.00754 0.00967 0.01020 0.01082 
       1997-01-31  1997-02-28  1997-03-31  1997-04-30  1997-05-31  1997-06-30 
      0.01162 0.01095 0.01084 0.01095 0.01288 0.01345 
       1997-07-31  1997-08-31  1997-09-30  1997-10-31  1997-11-30  1997-12-31 
      0.01355 0.01406 0.01443 0.01284 0.01336 0.01327 
       1998-01-31  1998-02-28  1998-03-31  1998-04-30  1998-05-31  1998-06-30 
      0.01296 0.01411 0.01493 0.01467 0.01337 0.01333 
       1998-07-31  1998-08-31  1998-09-30  1998-10-31  1998-11-30  1998-12-31 
      0.01220 0.00887 0.00936 0.01072 0.01078 0.01075 
       1999-01-31  1999-02-28  1999-03-31  1999-04-30  1999-05-31  1999-06-30 
      0.01020 0.01018 0.01111 0.01210 0.01220 0.01269 
       1999-07-31  1999-08-31  1999-09-30  1999-10-31  1999-11-30  1999-12-31 
      0.01262 0.01196 0.01159 0.01133 0.01116 0.01123 
       2000-01-31  2000-02-29  2000-03-31  2000-04-30  2000-05-31  2000-06-30 
      0.01080 0.01083 0.01174 0.01190 0.01232 0.01232 
       2000-07-31  2000-08-31  2000-09-30  2000-10-31  2000-11-30  2000-12-31 
      0.01218 0.01266 0.01246 0.01211 0.01208 0.01177 
       2001-01-31  2001-02-28  2001-03-31  2001-04-30  2001-05-31  2001-06-30 
      0.01170 0.01165 0.01130 0.01166 0.01237 0.01221 
       2001-07-31  2001-08-31  2001-09-30  2001-10-31  2001-11-30  2001-12-31 
      0.01234 0.01240 0.01176 0.01161 0.01193 0.01270 
       2002-01-31  2002-02-28  2002-03-31  2002-04-30  2002-05-31  2002-06-30 
      0.01271 0.01237 0.01229 0.01219 0.01201 0.01155 
       2002-07-31  2002-08-31  2002-09-30  2002-10-31  2002-11-30  2002-12-31 
      0.01045 0.01041 0.00957 0.00982 0.01050 0.00999 
       2003-01-31  2003-02-28  2003-03-31  2003-04-30  2003-05-31  2003-06-30 
      0.00938 0.00898 0.00930 0.00993 0.01020 0.01043 
       2003-07-31  2003-08-31  2003-09-30  2003-10-31  2003-11-30  2003-12-31 
      0.01051 0.01040 0.01038 0.01078 0.01085 0.01102 
       2004-01-31  2004-02-29  2004-03-31  2004-04-30  2004-05-31  2004-06-30 
      0.01096 0.01085 0.01083 0.01068 0.01065 0.01080 
       2004-07-31  2004-08-31  2004-09-30  2004-10-31  2004-11-30  2004-12-31 
      0.01070 0.01065 0.01063 0.01052 0.01079 0.01110 
       2005-01-31  2005-02-28  2005-03-31  2005-04-30  2005-05-31  2005-06-30 
      0.01100 0.01109 0.01081 0.01052 0.01047 0.01052 
       2005-07-31  2005-08-31  2005-09-30  2005-10-31  2005-11-30  2005-12-31 
      0.01051 0.01051 0.01065 0.01040 0.01050 0.01063 
       2006-01-31  2006-02-28  2006-03-31  2006-04-30  2006-05-31  2006-06-30 
      0.01112 0.01115 0.01138 0.01128 0.01097 0.01106 
       2006-07-31  2006-08-31  2006-09-30  2006-10-31  2006-11-30  2006-12-31 
      0.01086 0.01090 0.01087 0.01111 0.01111 0.01112 
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
      2006-03-31 0.01640
      2006-04-30 0.01456
      2006-05-31 0.01288
      2006-06-30 0.01263
      2006-07-31 0.01174
      2006-08-31 0.01218
      2006-09-30 0.01212
      2006-10-31 0.01197
      2006-11-30 0.01182
      2006-12-31 0.01138
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
      1996-01-31 -0.00372         NA  0.02245
      1996-02-29  0.00817         NA  0.02265
      1996-03-31  0.00437         NA  0.01335
      1996-04-30 -0.02022         NA  0.03245
      1996-05-31 -0.00352         NA  0.02285
      1996-06-30 -0.01502         NA -0.04274
      1996-07-31 -0.03422         NA -0.04614
      1996-08-31  0.02837 -0.01424  0.03365
      1996-09-30  0.00357  0.08605  0.05285
      1996-10-31  0.01767  0.01965  0.02705
      1996-11-30  0.00447  0.05955  0.05415
      1996-12-31  0.00647  0.01565  0.00895
      1997-01-31  0.01007  0.06525  0.06465
      1997-02-28 -0.00892 -0.02234 -0.04984
      1997-03-31 -0.00172 -0.04104 -0.04604
      1997-04-30  0.00147 -0.02024  0.01615
      1997-05-31  0.03267  0.03975  0.06345
      1997-06-30  0.01197  0.04105 -0.00704
      1997-07-31  0.00427  0.10085  0.09565
      1997-08-31  0.01257 -0.03384 -0.01524
      1997-09-30  0.01077  0.04345  0.04245
      1997-10-31 -0.03182 -0.03634 -0.04784
      1997-11-30  0.01387 -0.01924  0.00515
      1997-12-31 -0.00012  0.00505 -0.01274
      1998-01-31 -0.00552 -0.02534  0.03665
      1998-02-28  0.03177  0.08655  0.03415
      1998-03-31  0.02507  0.04835  0.00835
      1998-04-30 -0.00332 -0.01514  0.01095
      1998-05-31 -0.03422 -0.02484 -0.02604
      1998-06-30  0.00097  0.02505  0.02705
      1998-07-31 -0.03262 -0.04134 -0.01194
      1998-08-31 -0.10552 -0.01414 -0.08424
      1998-09-30  0.01367 -0.01874  0.05405
      1998-10-31  0.04467  0.02075 -0.01754
      1998-11-30  0.00147  0.05575  0.04305
      1998-12-31 -0.00142  0.07715  0.03395
      1999-01-31 -0.02042  0.06455  0.01445
      1999-02-28 -0.00172 -0.03714 -0.06544
      1999-03-31  0.03507  0.09405  0.00625
      1999-04-30  0.03987  0.00245  0.02925
      1999-05-31  0.00507 -0.01434 -0.00454
      1999-06-30  0.02147  0.05085  0.04225
      1999-07-31 -0.00132  0.01375 -0.00824
      1999-08-31 -0.02762  0.01435 -0.00024
      1999-09-30 -0.01562  0.01905 -0.00564
      1999-10-31 -0.01172  0.02805  0.06225
      1999-11-30 -0.00762  0.05555  0.09495
      1999-12-31  0.00357  0.13075  0.04555
      2000-01-31 -0.02132  0.02365 -0.01744
      2000-02-29  0.00117  0.14145  0.16715
      2000-03-31  0.04637 -0.05044 -0.00964
      2000-04-30  0.00907 -0.02394 -0.02514
      2000-05-31  0.02277 -0.02584 -0.03524
      2000-06-30  0.00117 -0.00364  0.02825
      2000-07-31 -0.00622  0.00465 -0.06494
      2000-08-31  0.02757  0.02055  0.06705
      2000-09-30 -0.00982 -0.04384 -0.06974
      2000-10-31 -0.01882 -0.04294  0.00375
      2000-11-30 -0.00072 -0.03614 -0.04924
      2000-12-31 -0.01792  0.00485  0.00445
      2001-01-31 -0.00322 -0.04664  0.01375
      2001-02-28 -0.00292 -0.03344 -0.04794
      2001-03-31 -0.02162 -0.01734 -0.02344
      2001-04-30  0.02337 -0.01604  0.00075
      2001-05-31  0.04677 -0.00664 -0.01774
      2001-06-30 -0.00902 -0.03484 -0.03614
      2001-07-31  0.00957 -0.01654 -0.01464
      2001-08-31  0.00497 -0.00584 -0.03704
      2001-09-30 -0.04232  0.01915 -0.01924
      2001-10-31 -0.01002 -0.03614 -0.03294
      2001-11-30  0.02287 -0.00594  0.01235
      2001-12-31  0.05647 -0.01444 -0.01064
      2002-01-31  0.00237 -0.03294 -0.02694
      2002-02-28 -0.02352 -0.05124 -0.05384
      2002-03-31 -0.00482  0.00795  0.00825
      2002-04-30 -0.00652 -0.02574 -0.03774
      2002-05-31 -0.01262 -0.04364 -0.01484
      2002-06-30 -0.03522 -0.04794 -0.05754
      2002-07-31 -0.08662 -0.02654 -0.04644
      2002-08-31 -0.00342 -0.01454 -0.01004
      2002-09-30 -0.06862 -0.01584 -0.05634
      2002-10-31  0.01857 -0.00984 -0.00224
      2002-11-30  0.05497 -0.02244  0.02255
      2002-12-31 -0.04342 -0.01634 -0.07764
      2003-01-31 -0.05232 -0.01444 -0.01394
      2003-02-28 -0.03622 -0.02904 -0.01034
      2003-03-31  0.02527 -0.01974 -0.00134
      2003-04-30  0.05397 -0.02604  0.04005
      2003-05-31  0.02257  0.03735  0.02985
      2003-06-30  0.01967  0.00935 -0.00154
      2003-07-31  0.00647  0.00925 -0.00384
      2003-08-31 -0.01082 -0.02254  0.01775
      2003-09-30 -0.00212 -0.01944 -0.01494
      2003-10-31  0.03697 -0.00074  0.04805
      2003-11-30  0.00577 -0.00274 -0.00064
      2003-12-31  0.01637  0.01475 -0.00294
      2004-01-31 -0.00582  0.00495 -0.00704
      2004-02-29 -0.01122 -0.00544  0.00185
      2004-03-31 -0.00242 -0.00134 -0.01624
      2004-04-30 -0.01542 -0.00594 -0.03244
      2004-05-31 -0.00292 -0.00664 -0.01914
      2004-06-30  0.01477 -0.00034 -0.01144
      2004-07-31 -0.01112 -0.00994 -0.04714
      2004-08-31 -0.00562 -0.02064 -0.01804
      2004-09-30 -0.00232 -0.00414 -0.01564
      2004-10-31 -0.01172  0.00055 -0.01354
      2004-11-30  0.02827  0.01105  0.04295
      2004-12-31  0.03287 -0.00584  0.00565
      2005-01-31 -0.01102 -0.01954 -0.00514
      2005-02-28  0.01037  0.00685  0.02925
      2005-03-31 -0.03182 -0.02104 -0.00314
      2005-04-30 -0.03202 -0.01804 -0.01434
      2005-05-31 -0.00682 -0.02804  0.01085
      2005-06-30  0.00497  0.00525 -0.00894
      2005-07-31 -0.00202  0.00575  0.00785
      2005-08-31  0.00017 -0.00184 -0.01444
      2005-09-30  0.01497  0.00385  0.00725
      2005-10-31 -0.02982 -0.03274 -0.02354
      2005-11-30  0.01197 -0.00474  0.00375
      2005-12-31  0.01497 -0.00564  0.00565
      2006-01-31  0.05807  0.06705  0.02265
      2006-02-28  0.00337 -0.04674  0.01415
      2006-03-31  0.02857  0.00095 -0.00054
      2006-04-30 -0.01222  0.00305 -0.00934
      2006-05-31 -0.03782 -0.01934 -0.03364
      2006-06-30  0.01047 -0.02574 -0.03134
      2006-07-31 -0.02552 -0.02724 -0.00224
      2006-08-31  0.00497 -0.02544  0.01285
      2006-09-30 -0.00432 -0.03724 -0.00524
      2006-10-31  0.03157  0.00255  0.00585
      2006-11-30  0.00057  0.00645  0.01445
      2006-12-31  0.00037 -0.02034 -0.00144
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
      2000-01-01            0.02500 0.14601             0.02500
      2001-01-01            0.15785 0.19577             0.02500
      2002-01-01            0.24431 0.02500             0.02500
      2003-01-01            0.21955 0.06590             0.02500
      2004-01-01            0.09780 0.02552             0.10507
      2005-01-01            0.02500 0.02500             0.24457
                 Emerging Markets Equity Market Neutral Event Driven
      2000-01-01            0.025             0.35000        0.025
      2001-01-01            0.025             0.35000        0.025
      2002-01-01            0.025             0.35000        0.025
      2003-01-01            0.025             0.28179        0.025
      2004-01-01            0.025             0.35000        0.025
      2005-01-01            0.025             0.20542        0.025
                 Fixed Income Arbitrage Global Macro Long/Short Equity
      2000-01-01              0.02500        0.025             0.025
      2001-01-01              0.02500        0.025             0.025
      2002-01-01              0.20568        0.025             0.025
      2003-01-01              0.25775        0.025             0.025
      2004-01-01              0.27158        0.025             0.025
      2005-01-01              0.35000        0.025             0.025
                 Merger Arbitrage Relative Value
      2000-01-01       0.07146      0.25752
      2001-01-01       0.12136      0.02500
      2002-01-01       0.02500      0.02500
      2003-01-01       0.02500      0.02500
      2004-01-01       0.02500      0.02500
      2005-01-01       0.02500      0.02500
      
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
       Min.   :1997-01-31   Min.   :-0.12370     Min.   :-0.05680  
       1st Qu.:2003-02-28   1st Qu.: 0.00020     1st Qu.:-0.01140  
       Median :2009-03-31   Median : 0.00650     Median : 0.00200  
       Mean   :2009-03-31   Mean   : 0.00579     Mean   : 0.00431  
       3rd Qu.:2015-04-30   3rd Qu.: 0.01370     3rd Qu.: 0.01990  
       Max.   :2021-05-31   Max.   : 0.06110     Max.   : 0.06910  
       Distressed Securities Emerging Markets   Equity Market Neutral
       Min.   :-0.10610     Min.   :-0.19220   Min.   :-0.05870    
       1st Qu.:-0.00210     1st Qu.:-0.00920   1st Qu.: 0.00090    
       Median : 0.00880     Median : 0.01000   Median : 0.00470    
       Mean   : 0.00682     Mean   : 0.00673   Mean   : 0.00433    
       3rd Qu.: 0.01790     3rd Qu.: 0.02570   3rd Qu.: 0.00830    
       Max.   : 0.05040     Max.   : 0.12300   Max.   : 0.02530    
        Event Driven       Fixed Income Arbitrage  Global Macro      
       Min.   :-0.12690   Min.   :-0.08670       Min.   :-0.03130  
       1st Qu.:-0.00120   1st Qu.: 0.00180       1st Qu.:-0.00390  
       Median : 0.00880   Median : 0.00550       Median : 0.00470  
       Mean   : 0.00667   Mean   : 0.00443       Mean   : 0.00559  
       3rd Qu.: 0.01680   3rd Qu.: 0.00930       3rd Qu.: 0.01280  
       Max.   : 0.06660   Max.   : 0.03650       Max.   : 0.07380  
       Long/Short Equity   Merger Arbitrage    Relative Value      Short Selling     
       Min.   :-0.08130   Min.   :-0.07900   Min.   :-0.06920   Min.   :-0.13400  
       1st Qu.:-0.00470   1st Qu.: 0.00070   1st Qu.: 0.00110   1st Qu.:-0.02510  
       Median : 0.00820   Median : 0.00590   Median : 0.00670   Median :-0.00320  
       Mean   : 0.00671   Mean   : 0.00558   Mean   : 0.00572   Mean   :-0.00126  
       3rd Qu.: 0.01950   3rd Qu.: 0.01110   3rd Qu.: 0.01300   3rd Qu.: 0.01810  
       Max.   : 0.07450   Max.   : 0.04720   Max.   : 0.03920   Max.   : 0.24630  
       Funds of Funds     
       Min.   :-0.07050  
       1st Qu.:-0.00330  
       Median : 0.00520  
       Mean   : 0.00451  
       3rd Qu.: 0.01270  
       Max.   : 0.06660  
      
      > tail(cumprod(1 + edhec), 1)
                 Convertible Arbitrage CTA Global Distressed Securities
      2021-05-31              5.20881   3.27801              6.98955
                 Emerging Markets Equity Market Neutral Event Driven
      2021-05-31         6.08835              3.51730     6.65401
                 Fixed Income Arbitrage Global Macro Long/Short Equity
      2021-05-31               3.58067     4.97781          6.67318
                 Merger Arbitrage Relative Value Short Selling Funds of Funds
      2021-05-31         5.01119       5.22224     0.51305       3.60102
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
       [1]  0.00000 -0.00770  0.00000 -0.01326  0.00000 -0.07069
       [7]  0.00000 -0.03877  0.00000 -0.03737  0.00000 -0.01627
      [13]  0.00000 -0.00150  0.00000 -0.02601  0.00000 -0.00040
      [19]  0.00000 -0.01742  0.00000 -0.01843  0.00000 -0.01490
      [25]  0.00000 -0.01655  0.00000 -0.02220  0.00000 -0.20591
      [31]  0.00000 -0.00400  0.00000 -0.00739  0.00000 -0.07837
      [37]  0.00000 -0.00070  0.00000 -0.05932  0.00000 -0.08276
      [43]  0.00000 -0.00190  0.00000 -0.00950  0.00000
      
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
       [1] -0.20591 -0.08276 -0.07837 -0.07069 -0.05932 -0.03877
       [7] -0.03737 -0.02601 -0.02220 -0.01843 -0.01742 -0.01655
      [13] -0.01627 -0.01490 -0.01326 -0.00950 -0.00770 -0.00739
      [19] -0.00400 -0.00190 -0.00150 -0.00070 -0.00040  0.00000
      [25]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
      [31]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
      [37]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
      [43]  0.00000  0.00000  0.00000  0.00000  0.00000
      
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
      1996-01-31   0.00380  0.00456
      1996-02-29  -0.03532  0.00398
      1996-03-31  -0.01057  0.00371
      1996-04-30  -0.01739  0.00428
      1996-05-31  -0.00543  0.00443
      1996-06-30   0.01507  0.00412
      
      > summary(managers)
           Index                 HAM1                HAM2         
       Min.   :1996-01-31   Min.   :-0.09440   Min.   :-0.03710  
       1st Qu.:1998-10-23   1st Qu.:-0.00002   1st Qu.:-0.00980  
       Median :2001-07-15   Median : 0.01115   Median : 0.00820  
       Mean   :2001-07-15   Mean   : 0.01112   Mean   : 0.01414  
       3rd Qu.:2004-04-07   3rd Qu.: 0.02485   3rd Qu.: 0.02520  
       Max.   :2006-12-31   Max.   : 0.06920   Max.   : 0.15560  
                                                NAs    :7         
            HAM3                HAM4               HAM5                HAM6          
       Min.   :-0.07180   Min.   :-0.17590   Min.   :-0.13200   Min.   :-0.04040  
       1st Qu.:-0.00537   1st Qu.:-0.01985   1st Qu.:-0.01640   1st Qu.:-0.00157  
       Median : 0.01020   Median : 0.01375   Median : 0.00380   Median : 0.01285  
       Mean   : 0.01244   Mean   : 0.01102   Mean   : 0.00408   Mean   : 0.01105  
       3rd Qu.: 0.03137   3rd Qu.: 0.04600   3rd Qu.: 0.03090   3rd Qu.: 0.02547  
       Max.   : 0.17960   Max.   : 0.15080   Max.   : 0.17470   Max.   : 0.05830  
                                              NAs    :55          NAs    :68         
        EDHEC LS EQ           SP500 TR           US 10Y TR            US 3m TR       
       Min.   :-0.05520   Min.   :-0.14460   Min.   :-0.07092   Min.   :0.00066  
       1st Qu.:-0.00317   1st Qu.:-0.01732   1st Qu.:-0.00851   1st Qu.:0.00157  
       Median : 0.01100   Median : 0.01095   Median : 0.00442   Median :0.00384  
       Mean   : 0.00954   Mean   : 0.00866   Mean   : 0.00438   Mean   :0.00322  
       3rd Qu.: 0.02145   3rd Qu.: 0.03802   3rd Qu.: 0.01666   3rd Qu.:0.00439  
       Max.   : 0.07450   Max.   : 0.09780   Max.   : 0.05055   Max.   :0.00658  
       NAs    :12                                                                    
      
      > tail(cumprod(1 + managers), 1)
                     HAM1 HAM2     HAM3    HAM4 HAM5 HAM6 EDHEC LS EQ SP500 TR
      2006-12-31 4.12667   NA 4.70673 3.52944   NA   NA          NA 2.76161
                 US 10Y TR US 3m TR
      2006-12-31  1.73403 1.52968
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
      Geometric Mean    0.00438
      
      > mean.stderr(edhec[, "Funds of Funds"])
                     Funds of Funds
      Standard Error   0.00093
      
      > mean.UCL(edhec[, "Funds of Funds"])
                             Funds of Funds
      Upper Confidence Level    0.00636
      
      > mean.LCL(edhec[, "Funds of Funds"])
                             Funds of Funds
      Lower Confidence Level    0.00266
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
       Min.   :2000-01-30   Min.   :-0.0650              Min.   :-0.06700    
       1st Qu.:2000-07-22   1st Qu.:-0.0110              1st Qu.:-0.00725    
       Median :2001-01-14   Median : 0.0130              Median : 0.01450    
       Mean   :2001-01-13   Mean   : 0.0090              Mean   : 0.01004    
       3rd Qu.:2001-07-06   3rd Qu.: 0.0295              3rd Qu.: 0.02850    
       Max.   :2001-12-30   Max.   : 0.0810              Max.   : 0.08300    
      
      > tail(cumprod(1 + portfolio_bacon), 1)
                 portfolio.monthly.return.... benchmark.return....
      2001-12-30                     1.21810             1.24988
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
       [1]  0.00000 -0.00770  0.00000 -0.01326  0.00000 -0.07069
       [7]  0.00000 -0.03877  0.00000 -0.03737  0.00000 -0.01627
      [13]  0.00000 -0.00150  0.00000 -0.02601  0.00000 -0.00040
      [19]  0.00000 -0.01742  0.00000 -0.01843  0.00000 -0.01490
      [25]  0.00000 -0.01655  0.00000 -0.02220  0.00000 -0.20591
      [31]  0.00000 -0.00400  0.00000 -0.00739  0.00000 -0.07837
      [37]  0.00000 -0.00070  0.00000 -0.05932  0.00000 -0.08276
      [43]  0.00000 -0.00190  0.00000 -0.00950  0.00000
      
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
       [1] -0.20591 -0.08276 -0.07837 -0.07069 -0.05932 -0.03877
       [7] -0.03737 -0.02601 -0.02220 -0.01843 -0.01742 -0.01655
      [13] -0.01627 -0.01490 -0.01326 -0.00950 -0.00770 -0.00739
      [19] -0.00400 -0.00190 -0.00150 -0.00070 -0.00040  0.00000
      [25]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
      [31]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
      [37]  0.00000  0.00000  0.00000  0.00000  0.00000  0.00000
      [43]  0.00000  0.00000  0.00000  0.00000  0.00000
      
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
      Modified VaR                          -0.02568 -0.03204
      Modified Expected Shortfall           -0.08941 -0.04038
                                  Distressed Securities Emerging Markets
      Modified VaR                          -0.02800      -0.05343
      Modified Expected Shortfall           -0.06732      -0.11570
                                  Equity Market Neutral Event Driven
      Modified VaR                          -0.01098  -0.02960
      Modified Expected Shortfall           -0.03670  -0.08113
                                  Fixed Income Arbitrage Global Macro
      Modified VaR                           -0.01773  -0.01380
      Modified Expected Shortfall            -0.05308  -0.01696
                                  Long/Short Equity Merger Arbitrage Relative Value
      Modified VaR                      -0.02950      -0.01502    -0.01736
      Modified Expected Shortfall       -0.04857      -0.05130    -0.04751
                                  Short Selling Funds of Funds
      Modified VaR                  -0.06215    -0.02309
      Modified Expected Shortfall   -0.06754    -0.04590
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
      HAM1     1.0068       0.6320
      HAM2     1.1774       0.8091
      HAM3     1.2443       1.5138
      HAM4     1.4855       2.3796
      HAM5     0.9575       1.2573
      HAM6     1.3205       0.8327
      
      > table.UpDownRatios(managers[, 1:6], managers[, 7, 
      +     drop = FALSE])
                          Up Capture Down Capture Up Number Down Number Up Percent
      HAM1 to EDHEC LS EQ     1.0068       0.6320    0.8675      0.5135     0.5663
      HAM2 to EDHEC LS EQ     1.1774       0.8091    0.6988      0.8378     0.4458
      HAM3 to EDHEC LS EQ     1.2443       1.5138    0.8193      0.8108     0.4819
      HAM4 to EDHEC LS EQ     1.4855       2.3796    0.7590      0.7297     0.6386
      HAM5 to EDHEC LS EQ     0.9575       1.2573    0.7143      0.7500     0.5510
      HAM6 to EDHEC LS EQ     1.3205       0.8327    0.8837      0.6190     0.6977
                          Down Percent
      HAM1 to EDHEC LS EQ       0.6486
      HAM2 to EDHEC LS EQ       0.4324
      HAM3 to EDHEC LS EQ       0.4324
      HAM4 to EDHEC LS EQ       0.3784
      HAM5 to EDHEC LS EQ       0.4286
      HAM6 to EDHEC LS EQ       0.4762
      
      > result = t(table.UpDownRatios(managers[, 1:6], managers[, 
      +     7, drop = FALSE]))
      
      > colnames(result) = colnames(managers[, 1:6])
      
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
      HAM1 to EDHEC LS EQ   0.58967 1.37859e-12 0.45894 0.69541
      HAM1 to SP500 TR      0.66006 7.39784e-18 0.55138 0.74671
      HAM2 to EDHEC LS EQ   0.70158 4.46836e-19 0.59747 0.78243
      HAM2 to SP500 TR      0.41282 1.71535e-06 0.25576 0.54866
      HAM3 to EDHEC LS EQ   0.80535 1.44389e-28 0.73174 0.86039
      HAM3 to SP500 TR      0.66086 6.54540e-18 0.55236 0.74734
      HAM4 to EDHEC LS EQ   0.61487 7.97041e-14 0.48958 0.71528
      HAM4 to SP500 TR      0.56018 2.87010e-12 0.43052 0.66719
      HAM5 to EDHEC LS EQ   0.44623 4.74963e-05 0.24694 0.60931
      HAM5 to SP500 TR      0.28444 1.21683e-02 0.06458 0.47797
      HAM6 to EDHEC LS EQ   0.72854 8.87981e-12 0.58804 0.82636
      HAM6 to SP500 TR      0.50915 1.73596e-05 0.30101 0.67098
      
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
      monthly  Std Dev        0.0256 0.0367 0.0365  0.0532 0.0457  0.0238      0.0205
      Skewness               -0.6588 1.4580 0.7908 -0.4311 0.0738 -0.2800      0.0177
      Kurtosis                5.3616 5.3794 5.6829  3.8632 5.3143  2.6511      3.9105
      Excess kurtosis         2.3616 2.3794 2.6829  0.8632 2.3143 -0.3489      0.9105
      Sample skewness        -0.6741 1.4937 0.8091 -0.4410 0.0768 -0.2936      0.0182
      Sample excess kurtosis  2.5004 2.5270 2.8343  0.9437 2.5541 -0.2778      1.0013
                             SP500 TR
      monthly  Std Dev         0.0433
      Skewness                -0.5531
      Kurtosis                 3.5598
      Excess kurtosis          0.5598
      Sample skewness         -0.5659
      Sample excess kurtosis   0.6285
      
      > require("Hmisc")
      
      > result = t(table.Distributions(managers[, 1:8]))
      
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
      
      > result = t(table.DownsideRiskRatio(managers[, 1:8]))
      
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
      Pain index     0.0157 0.0642 0.0674 0.0739 0.1452 0.0183      0.0178   0.1226
      Ulcer index    0.0362 0.1000 0.1114 0.1125 0.1828 0.0299      0.0325   0.1893
      Pain ratio     8.7789 2.7187 2.2438 1.6443 0.2570 7.4837      6.6466   0.7891
      Martin ratio   3.7992 1.7473 1.3572 1.0798 0.2042 4.5928      3.6345   0.5112
      
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
      1                                 0.55290                      0.44709
      2                                 0.62886                      0.37113
      3                                 0.63541                      0.36458
      4                                 0.64912                      0.35087
      5                                 0.62765                      0.37234
      6                                 0.64855                      0.35144
      7                                 0.61627                      0.38372
      
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
      
      > result = table.TrailingPeriods(edhec[, 10:13], periods = c(12, 
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
      
      > result = t(table.Variability(managers[, 1:8]))
      
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
      2002-12-31 -0.00404 -0.01280 -0.06003 -0.02103 -0.01650
      2003-12-31  0.01249  0.01194  0.07907  0.22039  0.03175
      2004-12-31  0.00718  0.01286  0.00578  0.05651  0.01053
      2005-12-31  0.00403  0.00616  0.04450  0.04027  0.01033
      2006-12-31  0.00998  0.00458  0.04255  0.05289  0.02315
                 Portfolio Return
      2002-12-31      -0.11441
      2003-12-31       0.35566
      2004-12-31       0.09289
      2005-12-31       0.10531
      2006-12-31       0.13318
      
      > to.yearly.contributions(res_qtr_rebal$contribution)
                         HAM1         HAM2         HAM3        HAM4        HAM5
      2002-12-31 -0.00404 -0.01280 -0.06003 -0.02103 -0.01650
      2003-12-31  0.01249  0.01194  0.07907  0.22039  0.03175
      2004-12-31  0.00718  0.01286  0.00578  0.05651  0.01053
      2005-12-31  0.00403  0.00616  0.04450  0.04027  0.01033
      2006-12-31  0.00998  0.00458  0.04255  0.05289  0.02315
                 Portfolio Return
      2002-12-31      -0.11441
      2003-12-31       0.35566
      2004-12-31       0.09289
      2005-12-31       0.10531
      2006-12-31       0.13318
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
      2000-01-01            0.02500 0.14601             0.02500
      2001-01-01            0.15785 0.19577             0.02500
      2002-01-01            0.24431 0.02500             0.02500
      2003-01-01            0.21955 0.06590             0.02500
      2004-01-01            0.09780 0.02552             0.10507
      2005-01-01            0.02500 0.02500             0.24457
                 Emerging Markets Equity Market Neutral Event Driven
      2000-01-01            0.025             0.35000        0.025
      2001-01-01            0.025             0.35000        0.025
      2002-01-01            0.025             0.35000        0.025
      2003-01-01            0.025             0.28179        0.025
      2004-01-01            0.025             0.35000        0.025
      2005-01-01            0.025             0.20542        0.025
                 Fixed Income Arbitrage Global Macro Long/Short Equity
      2000-01-01              0.02500        0.025             0.025
      2001-01-01              0.02500        0.025             0.025
      2002-01-01              0.20568        0.025             0.025
      2003-01-01              0.25775        0.025             0.025
      2004-01-01              0.27158        0.025             0.025
      2005-01-01              0.35000        0.025             0.025
                 Merger Arbitrage Relative Value
      2000-01-01       0.07146      0.25752
      2001-01-01       0.12136      0.02500
      2002-01-01       0.02500      0.02500
      2003-01-01       0.02500      0.02500
      2004-01-01       0.02500      0.02500
      2005-01-01       0.02500      0.02500
    Code
      dev.off()
    Output
      pdf 
        2 

