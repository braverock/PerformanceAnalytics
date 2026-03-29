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
      [1] 0.04078668
      
      > ActivePremium(managers[, 1, drop = FALSE], managers[, 
      +     8, drop = FALSE])
      [1] 0.04078668
      
      > ActivePremium(managers[, 1:6], managers[, 8, drop = FALSE])
                                     HAM1       HAM2       HAM3       HAM4       HAM5
      Active Premium: SP500 TR 0.04078668 0.07759873 0.05446935 0.02473443 0.02182245
                                     HAM6
      Active Premium: SP500 TR 0.07585993
      
      > ActivePremium(managers[, 1:6], managers[, 8:7, drop = FALSE])
                                        HAM1       HAM2       HAM3        HAM4
      Active Premium: SP500 TR    0.04078668 0.07759873 0.05446935  0.02473443
      Active Premium: EDHEC LS EQ 0.01965368 0.03776329 0.01043540 -0.00462594
                                         HAM5       HAM6
      Active Premium: SP500 TR     0.02182245 0.07585993
      Active Premium: EDHEC LS EQ -0.03237453 0.05463574
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
      Annualized Sharpe Ratio (Rf=0%)                    0.7591435
      
      > data(managers)
      
      > print(AdjustedSharpeRatio(managers["1996"]))
                                                HAM1    HAM2      HAM3     HAM4 HAM5
      Adjusted Sharpe ratio (Risk free = 0) 2.045968 14.5593 0.9322736 1.883368   NA
                                            HAM6 EDHEC LS EQ SP500 TR   US 10Y TR
      Adjusted Sharpe ratio (Risk free = 0)   NA          NA 1.986962 0.006312774
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
      [1] -0.4302756
      
      > print(AppraisalRatio(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2], method = "modified"))
      [1] -0.01418576
      
      > print(AppraisalRatio(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2], method = "alternative"))
      [1] -0.1066928
      
      > data(managers)
      
      > print(AppraisalRatio(managers["1996", 1], managers["1996", 
      +     8]))
      [1] 1.623025
      
      > print(AppraisalRatio(managers["1996", 1:5], managers["1996", 
      +     8]))
                                          HAM1 HAM2     HAM3      HAM4 HAM5
      Appraisal ratio (Risk free = 0) 1.623025   NA 3.527723 0.7070483   NA
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
      [1] 1.779783
      
      > data(managers)
      
      > print(BernardoLedoitRatio(managers["1996"]))
                                    HAM1 HAM2     HAM3     HAM4 HAM5 HAM6 EDHEC LS EQ
      Bernardo and Ledoit ratio 4.598338 2375 6.482812 3.615074  NaN  NaN         NaN
                                SP500 TR US 10Y TR US 3m TR
      Bernardo and Ledoit ratio 4.340625  1.028277     -Inf
      
      > print(BernardoLedoitRatio(managers["1996", 1]))
      [1] 4.598338
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
      [1] 0.3431621
      
      > BetaCoSkewness(managers[, "HAM2", drop = FALSE], managers[, 
      +     "SP500 TR", drop = FALSE])
      [1] 0.04542927
      
      > BetaCoKurtosis(managers[, "HAM2", drop = FALSE], managers[, 
      +     "SP500 TR", drop = FALSE])
      [1] 0.1988373
      
      > BetaCoKurtosis(managers[, 1:6], managers[, 8, drop = FALSE])
                                     HAM1      HAM2     HAM3      HAM4      HAM5
      Beta Cokurtosis: SP500 TR 0.4814681 0.1988373 0.506819 0.8483555 0.2738611
                                     HAM6
      Beta Cokurtosis: SP500 TR 0.1541281
      
      > BetaCoKurtosis(managers[, 1:6], managers[, 8:7])
                                        HAM1      HAM2     HAM3      HAM4      HAM5
      Beta Cokurtosis: SP500 TR    0.4814681 0.1988373 0.506819 0.8483555 0.2738611
      Beta Cokurtosis: EDHEC LS EQ 0.7100547 1.2676023 1.426660 1.4533001 1.2831205
                                        HAM6
      Beta Cokurtosis: SP500 TR    0.1541281
      Beta Cokurtosis: EDHEC LS EQ 0.8618328
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
      [1] 0.7447309
      
      > print(BurkeRatio(portfolio_bacon[, 1], modified = TRUE))
      [1] 3.648421
      
      > data(managers)
      
      > print(BurkeRatio(managers["1996"]))
                                      HAM1 HAM2     HAM3     HAM4 HAM5 HAM6
      Burke ratio (Risk free = 0) 4.779747  Inf 6.340485 4.048153   NA   NA
                                  EDHEC LS EQ SP500 TR   US 10Y TR US 3m TR
      Burke ratio (Risk free = 0)          NA 4.739828 0.006137083      Inf
      
      > print(BurkeRatio(managers["1996", 1]))
      [1] 4.779747
      
      > print(BurkeRatio(managers["1996"], modified = TRUE))
                                               HAM1 HAM2     HAM3     HAM4 HAM5 HAM6
      Modified Burke ratio (Risk free = 0) 16.55753  Inf 21.96408 14.02321   NA   NA
                                           EDHEC LS EQ SP500 TR  US 10Y TR US 3m TR
      Modified Burke ratio (Risk free = 0)          NA 16.41925 0.02125948      Inf
      
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
      Capital Market Line Slope: SP500 TR 0.1255829
      
      > CAPM.CML(managers[, "HAM1", drop = FALSE], managers[, 
      +     "SP500 TR", drop = FALSE], Rf = 0)
      [1] 0.002225442
      
      > CAPM.RiskPremium(managers[, "SP500 TR", drop = FALSE], 
      +     Rf = 0)
                              SP500 TR
      Risk Premium (Rf=0%) 0.008665341
      
      > CAPM.RiskPremium(managers[, "HAM1", drop = FALSE], 
      +     Rf = 0)
                                 HAM1
      Risk Premium (Rf=0%) 0.01112273
      
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
      HAM1 to SP500 TR     0.0070965                -0.196351               0.1665381
                       Average beta US 10Y TR beta at t - 1 US 3m TR beta at t - 1
      HAM1 to SP500 TR    0.3248015                3.493336              -63.74814
      
      > CAPM.dynamic(managers[80:120, 1:6], managers[80:120, 
      +     7, drop = FALSE], Rf = managers[80:120, 10, drop = FALSE], 
      +     Z = managers[80:120, 9:10])
                          Average alpha US 10Y TR alpha at t - 1
      HAM1 to EDHEC LS EQ -0.0001741347              -0.23890464
      HAM2 to EDHEC LS EQ -0.0027673634              -0.06632217
      HAM3 to EDHEC LS EQ  0.0062624783              -0.21733015
      HAM4 to EDHEC LS EQ -0.0033262023               0.16135997
      HAM5 to EDHEC LS EQ  0.0043380559               0.26882960
      HAM6 to EDHEC LS EQ -0.0053865004               0.05000616
                          US 3m TR alpha at t - 1 Average beta
      HAM1 to EDHEC LS EQ              -0.4385012    1.1793098
      HAM2 to EDHEC LS EQ              -4.0176982    0.7067390
      HAM3 to EDHEC LS EQ               7.6804829    0.4260623
      HAM4 to EDHEC LS EQ              -0.2091890    1.6367609
      HAM5 to EDHEC LS EQ               3.8497148    1.2224547
      HAM6 to EDHEC LS EQ              -3.0664314    1.6281908
                          US 10Y TR beta at t - 1 US 3m TR beta at t - 1
      HAM1 to EDHEC LS EQ                3.861212              -51.01409
      HAM2 to EDHEC LS EQ                5.682080              171.16658
      HAM3 to EDHEC LS EQ                1.507916             -705.20354
      HAM4 to EDHEC LS EQ               -7.622136             -565.85196
      HAM5 to EDHEC LS EQ                7.083956               39.70358
      HAM6 to EDHEC LS EQ              -11.035136              343.52891
      
      > CAPM.dynamic(managers[80:120, 1:6], managers[80:120, 
      +     8:7], managers[80:120, 10, drop = FALSE], Z = managers[80:120, 
      +     9:10])
                          Average alpha US 10Y TR alpha at t - 1
      HAM1 to SP500 TR     0.0036316941              -0.03538369
      HAM2 to SP500 TR     0.0016901086              -0.05484988
      HAM3 to SP500 TR     0.0072668556              -0.05978008
      HAM4 to SP500 TR    -0.0015875926               0.41314240
      HAM5 to SP500 TR     0.0083363515               0.35300102
      HAM6 to SP500 TR     0.0012839717               0.03521033
      HAM1 to EDHEC LS EQ -0.0001741347              -0.23890464
      HAM2 to EDHEC LS EQ -0.0027673634              -0.06632217
      HAM3 to EDHEC LS EQ  0.0062624783              -0.21733015
      HAM4 to EDHEC LS EQ -0.0033262023               0.16135997
      HAM5 to EDHEC LS EQ  0.0043380559               0.26882960
      HAM6 to EDHEC LS EQ -0.0053865004               0.05000616
                          US 3m TR alpha at t - 1 Average beta
      HAM1 to SP500 TR                 0.08506313   0.51861197
      HAM2 to SP500 TR                -2.91835013   0.05157528
      HAM3 to SP500 TR                 4.10231175   0.17720080
      HAM4 to SP500 TR                -6.04090381   1.20562924
      HAM5 to SP500 TR                 1.56695525   0.57212866
      HAM6 to SP500 TR                -1.72313785   0.59611332
      HAM1 to EDHEC LS EQ             -0.43850123   1.17930984
      HAM2 to EDHEC LS EQ             -4.01769818   0.70673900
      HAM3 to EDHEC LS EQ              7.68048289   0.42606233
      HAM4 to EDHEC LS EQ             -0.20918897   1.63676093
      HAM5 to EDHEC LS EQ              3.84971482   1.22245465
      HAM6 to EDHEC LS EQ             -3.06643145   1.62819081
                          US 10Y TR beta at t - 1 US 3m TR beta at t - 1
      HAM1 to SP500 TR                  -1.181057              -65.73676
      HAM2 to SP500 TR                   2.075534              -23.79983
      HAM3 to SP500 TR                   1.063350             -256.19346
      HAM4 to SP500 TR                  -1.812210              162.03456
      HAM5 to SP500 TR                   4.277306              183.06200
      HAM6 to SP500 TR                  -5.106318              189.51371
      HAM1 to EDHEC LS EQ                3.861212              -51.01409
      HAM2 to EDHEC LS EQ                5.682080              171.16658
      HAM3 to EDHEC LS EQ                1.507916             -705.20354
      HAM4 to EDHEC LS EQ               -7.622136             -565.85196
      HAM5 to EDHEC LS EQ                7.083956               39.70358
      HAM6 to EDHEC LS EQ              -11.035136              343.52891
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
      [1] -0.01313932
      
      > data(managers)
      
      > print(SFM.epsilon(managers["1996", 1], managers["1996", 
      +     8]))
      [1] 0.07425366
      
      > print(SFM.epsilon(managers["1996", 1:5], managers["1996", 
      +     8]))
                                               HAM1      HAM2      HAM3       HAM4
      Regression epsilon (Risk free = 0) 0.07425366 0.5399193 0.2048063 0.05570592
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
      [1] -0.01416944
      
      > data(managers)
      
      > print(SFM.jensenAlpha(managers["1996", 1], managers["1996", 
      +     8]))
      [1] 0.08077871
      
      > print(SFM.jensenAlpha(managers["1996", 1:5], managers["1996", 
      +     8]))
                                           HAM1 HAM2      HAM3       HAM4 HAM5
      Jensen's Alpha (Risk free = 0) 0.08077871   NA 0.2196026 0.06063837   NA
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
      0.1143761 
      
      > CDaR.alpha(edhec[, 1], edhec[, 2], type = "max")
            max 
      0.1114534 
      
      > CDaR.alpha(edhec[, 1], edhec[, 2], type = "average")
        average 
      0.1217059 
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
      -0.8031502 
      
      > CDaR.beta(edhec[, 1], edhec[, 2], type = "max")
             max 
      -0.7480649 
      
      > CDaR.beta(edhec[, 1], edhec[, 2], type = "average")
         average 
      -0.9412998 
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
      Calmar Ratio 0.9061697
      
      > CalmarRatio(managers[, 1:6])
                        HAM1     HAM2      HAM3      HAM4      HAM5     HAM6
      Calmar Ratio 0.9061697 0.728094 0.5225829 0.4227315 0.1095909 1.742525
      
      > SterlingRatio(managers[, 1, drop = FALSE])
                                         HAM1
      Sterling Ratio (Excess = 10%) 0.5462542
      
      > SterlingRatio(managers[, 1:6])
                                         HAM1      HAM2      HAM3      HAM4
      Sterling Ratio (Excess = 10%) 0.5462542 0.5138746 0.3883671 0.3136025
                                          HAM5      HAM6
      Sterling Ratio (Excess = 10%) 0.08471255 0.7678475
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
      [1] 0.0006641516
      
      > CoSkewness(managers[, "HAM2", drop = FALSE], managers[, 
      +     "SP500 TR", drop = FALSE])
      [1] -2.101289e-06
      
      > CoKurtosis(managers[, "HAM2", drop = FALSE], managers[, 
      +     "SP500 TR", drop = FALSE])
      [1] 2.579066e-06
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
      [1] 0.4013329
      
      > data(managers)
      
      > print(DRatio(managers["1996"]))
                    HAM1         HAM2       HAM3      HAM4 HAM5 HAM6 EDHEC LS EQ
      d ratio 0.07248996 0.0001052632 0.03085081 0.1383098  NaN  NaN         NaN
                SP500 TR US 10Y TR US 3m TR
      d ratio 0.04607631  1.361501        0
      
      > print(DRatio(managers["1996", 1]))
      [1] 0.07248996
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
      [1,] 0.02553674
      
      > DownsidePotential(portfolio_bacon[, 1], MAR)
                 [,1]
      [1,] 0.01370833
      
      > data(managers)
      
      > apply(managers[, 1:6], 2, sd, na.rm = TRUE)
            HAM1       HAM2       HAM3       HAM4       HAM5       HAM6 
      0.02562881 0.03671623 0.03651259 0.05319796 0.04573149 0.02381247 
      
      > DownsideDeviation(managers[, 1:6])
                                          HAM1      HAM2       HAM3       HAM4
      Downside Deviation (MAR = 0%) 0.01454078 0.0115736 0.01735454 0.03406781
                                         HAM5       HAM6
      Downside Deviation (MAR = 0%) 0.0304305 0.01214476
      
      > DownsideDeviation(managers[, 1:6], MAR = 0.04/12)
                                         HAM1       HAM2       HAM3       HAM4
      Downside Deviation (MAR = 0%) 0.0157602 0.01341739 0.01891525 0.03565192
                                         HAM5       HAM6
      Downside Deviation (MAR = 0%) 0.0320608 0.01366533
      
      > SemiDeviation(managers[, 1, drop = FALSE])
                          HAM1
      Semi-Deviation 0.0190795
      
      > SemiDeviation(managers[, 1:6])
                          HAM1       HAM2       HAM3       HAM4       HAM5       HAM6
      Semi-Deviation 0.0190795 0.02011968 0.02369306 0.03950215 0.03244118 0.01751678
      
      > SemiVariance(managers[, 1, drop = FALSE])
                            HAM1
      Semi-Variance 0.0007280549
      
      > SemiVariance(managers[, 1:6])
                            HAM1         HAM2        HAM3        HAM4        HAM5
      Semi-Variance 0.0007280549 0.0006657919 0.001015063 0.003322184 0.002077875
                           HAM6
      Semi-Variance 0.000677159
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
      [1] 0.4583333
      
      > data(managers)
      
      > print(DownsideFrequency(managers["1996"]))
                                    HAM1 HAM2      HAM3      HAM4 HAM5 HAM6
      Downside Frequency (MAR = 0%) 0.25  0.2 0.1666667 0.3333333  NaN  NaN
                                    EDHEC LS EQ  SP500 TR US 10Y TR US 3m TR
      Downside Frequency (MAR = 0%)         NaN 0.1666667 0.5833333        0
      
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
      Downside Sharpe Ratio 0.3001809 0.1951633 0.3315969 0.1866442 0.4720935
                                   ED       FIA       GM        LS        MA
      Downside Sharpe Ratio 0.3074177 0.3155078 0.426225 0.3055217 0.4428818
                                   RV          SS       FOF
      Downside Sharpe Ratio 0.4168768 -0.03014276 0.2668157
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
      ES              -0.03878   -0.04062              -0.04132      -0.07544667
         Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      ES              -0.01752  -0.04445333            -0.02906667  -0.02109333
         Long/Short Equity Merger Arbitrage Relative Value Short Selling
      ES       -0.04481333      -0.02333333    -0.02712667   -0.09484667
         Funds of Funds
      ES    -0.03569333
      
      > ES(edhec, p = 0.95, method = "gaussian")
         Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      ES           -0.02872442 -0.04260771            -0.0305384      -0.06062504
         Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      ES           -0.01256767  -0.03259857            -0.01916326  -0.02451761
         Long/Short Equity Merger Arbitrage Relative Value Short Selling
      ES       -0.03632667      -0.01805389    -0.01871098   -0.09495821
         Funds of Funds
      ES    -0.02861017
      
      > ES(edhec, p = 0.95, method = "modified")
         Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      ES           -0.08941788 -0.04038067           -0.06732969       -0.1157089
         Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      ES           -0.03670501  -0.08113553            -0.05308245  -0.01696439
         Long/Short Equity Merger Arbitrage Relative Value Short Selling
      ES       -0.04857352      -0.05130164    -0.04751282   -0.06754083
         Funds of Funds
      ES    -0.04590385
      
      > ES(edhec, p = 0.99)
         Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      ES           -0.09538713 -0.05203246           -0.07097989       -0.1261338
         Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      ES           -0.03875142  -0.08433448            -0.06036075  -0.02309801
         Long/Short Equity Merger Arbitrage Relative Value Short Selling
      ES       -0.06580245      -0.05760895    -0.04882532    -0.1941136
         Funds of Funds
      ES    -0.05423976
      
      > ES(edhec, p = 0.01)
         Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      ES           -0.09538713 -0.05203246           -0.07097989       -0.1261338
         Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      ES           -0.03875142  -0.08433448            -0.06036075  -0.02309801
         Long/Short Equity Merger Arbitrage Relative Value Short Selling
      ES       -0.06580245      -0.05760895    -0.04882532    -0.1941136
         Funds of Funds
      ES    -0.05423976
      
      > if (requireNamespace("robustbase", quietly = TRUE)) {
      +     ES(edhec, clean = "boudt")
      +     ES(edhec, clean = "boudt", portfolio_method = "component")
      + }
    Message
      no weights passed in, assuming equal weighted portfolio
    Output
      $MES
      [1] 0.02311949
      
      $contribution
       Convertible Arbitrage             CTA Global  Distressed Securities 
                0.0027822302          -0.0014768226           0.0029299284 
            Emerging Markets  Equity Market Neutral           Event Driven 
                0.0052670402           0.0009161824           0.0033955393 
      Fixed Income Arbitrage           Global Macro      Long/Short Equity 
                0.0022726928           0.0007904038           0.0030393739 
            Merger Arbitrage         Relative Value          Short Selling 
                0.0015214460           0.0020077593          -0.0034307248 
              Funds of Funds 
                0.0031044438 
      
      $pct_contrib_MES
       Convertible Arbitrage             CTA Global  Distressed Securities 
                  0.12034132            -0.06387781             0.12672979 
            Emerging Markets  Equity Market Neutral           Event Driven 
                  0.22781816             0.03962814             0.14686911 
      Fixed Income Arbitrage           Global Macro      Long/Short Equity 
                  0.09830202             0.03418777             0.13146369 
            Merger Arbitrage         Relative Value          Short Selling 
                  0.06580793             0.08684271            -0.14839101 
              Funds of Funds 
                  0.13427820 
      
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
      [1] 0.03549421
      
      $contribution
       [1]  3.839546e-03 -2.223544e-03  6.610487e-03  6.506026e-03  8.653283e-04
       [6]  7.276399e-03  2.324508e-03 -7.842543e-05  3.128017e-03  3.932343e-03
      [11]  3.630823e-03 -3.905603e-03  3.588303e-03
      
      $pct_contrib_MES
       [1]  0.108173873 -0.062645258  0.186241285  0.183298238  0.024379423
       [6]  0.205002427  0.065489790 -0.002209527  0.088127539  0.110788302
      [11]  0.102293383 -0.110034928  0.101095454
      
      
      > sigma <- cov(edhec)
      
      > m3 <- M3.MM(edhec)
      
      > m4 <- M4.MM(edhec)
      
      > ES(p = 0.95, portfolio_method = "component", weights = rep(1/p, 
      +     p), mu = mu, sigma = sigma, m3 = m3, m4 = m4)
      $MES
      [1] 0.03625114
      
      $contribution
       [1]  0.0059762619 -0.0028081561  0.0052816918  0.0062522301  0.0009068639
       [6]  0.0058974265  0.0034627291  0.0001339207  0.0036259163  0.0034602269
      [11]  0.0037776798 -0.0031537633  0.0034381082
      
      $pct_contrib_MES
       [1]  0.164857231 -0.077463949  0.145697277  0.172469910  0.025016152
       [6]  0.162682530  0.095520570  0.003694249  0.100022144  0.095451544
      [11]  0.104208592 -0.086997642  0.094841391
      
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
      portfolio.monthly.return....                     1.030395
      
      > data(managers)
      
      > print(FamaBeta(managers["1996", 1], managers["1996", 
      +     8]))
                HAM1
      HAM1 0.5351217
      
      > print(FamaBeta(managers["1996", 1:5], managers["1996", 
      +     8]))
                      HAM1 HAM2     HAM3     HAM4 HAM5
      Fama Beta  0.5351217   NA 1.007084 1.037632   NA
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
      [1] 0.3604125
      
      > InformationRatio(managers[, 1:6], managers[, 8, drop = FALSE])
                                       HAM1      HAM2      HAM3     HAM4      HAM5
      Information Ratio: SP500 TR 0.3604125 0.5059751 0.4701009 0.154914 0.1212162
                                       HAM6
      Information Ratio: SP500 TR 0.6722844
      
      > InformationRatio(managers[, 1:6], managers[, 8:7])
                                          HAM1      HAM2      HAM3       HAM4
      Information Ratio: SP500 TR    0.3604125 0.5059751 0.4701009  0.1549140
      Information Ratio: EDHEC LS EQ 0.2593770 0.4162701 0.1279329 -0.0294821
                                           HAM5      HAM6
      Information Ratio: SP500 TR     0.1212162 0.6722844
      Information Ratio: EDHEC LS EQ -0.2277438 0.9667207
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
      [1] 0.1566371
      
      > data(managers)
      
      > MAR = 0
      
      > print(Kappa(managers["1996"], MAR, l))
                           HAM1     HAM2     HAM3    HAM4 HAM5 HAM6 EDHEC LS EQ
      kappa (MAR = 0%) 1.492063 1061.685 2.235197 1.14188  NaN  NaN         NaN
                       SP500 TR  US 10Y TR US 3m TR
      kappa (MAR = 0%) 1.274332 0.01674457      Inf
      
      > print(Kappa(managers["1996", 1], MAR, l))
      [1] 1.492063
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
      Kelly Ratio 5.929483
      
      > KellyRatio(managers[, 1, drop = FALSE], Rf = managers[, 
      +     10, drop = FALSE])
                      HAM1
      Kelly Ratio 6.010854
      
      > KellyRatio(managers[, 1:6], Rf = managers[, 10, drop = FALSE])
                      HAM1     HAM2     HAM3     HAM4      HAM5     HAM6
      Kelly Ratio 6.010854 4.069873 3.458124 1.376354 0.3876476 7.948295
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
      Sortino Ratio (MAR = 0.5%)                    0.1034799
      
      > data(managers)
      
      > MAR = 0
      
      > print(MSquaredExcess(managers["1996", 1], managers["1996", 
      +     8], MAR))
                 SP500 TR
      SP500 TR 0.02027322
      
      > print(MSquaredExcess(managers["1996", 1:5], managers["1996", 
      +     8], MAR))
                                           HAM1 HAM2      HAM3        HAM4 HAM5
      MSquaredExcess (Risk free = 0) 0.02027322   NA 0.1409545 -0.02546609   NA
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
      SP500 TR 0.2544876
      
      > print(MSquared(managers["1996", 1:5], managers["1996", 
      +     8]))
                                    HAM1 HAM2      HAM3      HAM4 HAM5
      MSquared (Risk free = 0) 0.2544876   NA 0.4028725 0.1982483   NA
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
      benchmark.return....          -0.01553103
      
      > MSquaredExcess(portfolio_bacon[, 1], portfolio_bacon[, 
      +     2], Method = "arithmetic")
                           benchmark.return....
      benchmark.return....          -0.01736344
      
      > data(managers)
      
      > MSquaredExcess(managers["1996", 1], managers["1996", 
      +     8])
                 SP500 TR
      SP500 TR 0.02027322
      
      > MSquaredExcess(managers["1996", 1:5], managers["1996", 
      +     8])
                                           HAM1 HAM2      HAM3        HAM4 HAM5
      MSquaredExcess (Risk free = 0) 0.02027322   NA 0.1409545 -0.02546609   NA
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
      HAM1 to SP500 TR 0.008275839 0.3211407 0.1344417
      
      > MarketTiming(managers[80:120, 1:6], managers[80:120, 
      +     7], managers[80:120, 10])
                                  Alpha      Beta     Gamma
      HAM1 to EDHEC LS EQ -0.0005755802 1.3121058 -0.405150
      HAM2 to EDHEC LS EQ -0.0003616789 0.4370998  8.520620
      HAM3 to EDHEC LS EQ -0.0058148518 1.1898242 11.913786
      HAM4 to EDHEC LS EQ -0.0055113742 2.0616524 18.797340
      HAM5 to EDHEC LS EQ  0.0005125284 1.0703704 -5.077881
      HAM6 to EDHEC LS EQ  0.0003590925 1.2711094 -7.443428
      
      > MarketTiming(managers[80:120, 1:6], managers[80:120, 
      +     8:7], managers[80:120, 10], method = "TM")
                                  Alpha      Beta      Gamma
      HAM1 to SP500 TR     0.0048833318 0.5970167 -0.2801650
      HAM2 to SP500 TR     0.0050694247 0.1190405 -0.5000263
      HAM3 to SP500 TR     0.0032110848 0.5272982 -0.6645684
      HAM4 to SP500 TR     0.0094634771 0.8779523 -0.8155100
      HAM5 to SP500 TR     0.0087234498 0.2869943 -2.7728051
      HAM6 to SP500 TR     0.0048031173 0.2902262  0.6910898
      HAM1 to EDHEC LS EQ -0.0005755802 1.3121058 -0.4051500
      HAM2 to EDHEC LS EQ -0.0003616789 0.4370998  8.5206196
      HAM3 to EDHEC LS EQ -0.0058148518 1.1898242 11.9137857
      HAM4 to EDHEC LS EQ -0.0055113742 2.0616524 18.7973395
      HAM5 to EDHEC LS EQ  0.0005125284 1.0703704 -5.0778814
      HAM6 to EDHEC LS EQ  0.0003590925 1.2711094 -7.4434281
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
      Martin Ratio (Rf = 0) 14.81878 0.01003279      Inf
      
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
      [1] 0.03108333
      
      > data(managers)
      
      > print(MeanAbsoluteDeviation(managers["1996"]))
                                   HAM1     HAM2       HAM3       HAM4 HAM5 HAM6
      Mean absolute deviation 0.0125375 0.031576 0.02229444 0.02540972  NaN  NaN
                              EDHEC LS EQ SP500 TR  US 10Y TR US 3m TR
      Mean absolute deviation         NaN  0.02225 0.01611653  0.00021
      
      > print(MeanAbsoluteDeviation(managers["1996", 1]))
      [1] 0.0125375
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
      [1] 0.01678381
      
      > Modigliani(managers[, 1:6], managers[, 8, drop = FALSE], 
      +     managers[, 8, drop = FALSE])
                                                    HAM1       HAM2      HAM3
      Modigliani-Modigliani measure: SP500 TR 0.01281799 0.01505458 0.0131509
                                                    HAM4       HAM5       HAM6
      Modigliani-Modigliani measure: SP500 TR 0.01057959 0.01053081 0.01844616
      
      > Modigliani(managers[, 1:6], managers[, 8:7], managers[, 
      +     8, drop = FALSE])
                                                       HAM1       HAM2       HAM3
      Modigliani-Modigliani measure: SP500 TR    0.01281799 0.01505458 0.01315090
      Modigliani-Modigliani measure: EDHEC LS EQ 0.01062640 0.01168261 0.01078361
                                                       HAM4        HAM5       HAM6
      Modigliani-Modigliani measure: SP500 TR    0.01057959 0.010530812 0.01844616
      Modigliani-Modigliani measure: EDHEC LS EQ 0.00956933 0.009546295 0.01328426
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
      portfolio.monthly.return....                   -0.0178912
      
      > data(managers)
      
      > print(NetSelectivity(managers["1996", 1], managers["1996", 
      +     8]))
                 HAM1
      HAM1 0.01333906
      
      > print(NetSelectivity(managers["1996", 1:5], managers["1996", 
      +     8]))
                                            HAM1 HAM2      HAM3        HAM4 HAM5
      Net Selectivity (Risk free = 0) 0.01333906   NA 0.1745397 -0.03249043   NA
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
      Omega (L = 0%)              2.848491   1.618552              2.756588
                     Emerging Markets Equity Market Neutral Event Driven
      Omega (L = 0%)         1.752959              4.291785     2.630127
                     Fixed Income Arbitrage Global Macro Long/Short Equity
      Omega (L = 0%)               3.369045      2.89794          2.314433
                     Merger Arbitrage Relative Value Short Selling Funds of Funds
      Omega (L = 0%)         3.955367       3.662014     0.9247907       2.185667
      
      > if (requireNamespace("Hmisc", quietly = TRUE)) {
      +     Omega(edhec[, 13], method = "interp", output = "point")
      +     Omega(edhec[, 13], method = "interp", output = "full")
      + }
              Funds of Funds
      -0.0705    292.0000000
      -0.0705    292.0000000
      -0.0618    218.7500000
      -0.0616    166.4285714
      -0.06      132.1818182
      -0.0272    102.4117647
      -0.0269     81.0400000
      -0.0266     67.9411765
      -0.0264     58.9318182
      -0.0262     51.3214286
      -0.0252     45.7101449
      -0.0222     41.3614458
      -0.0205     37.8673469
      -0.0202     34.9824561
      -0.0192     32.5496183
      -0.0176     30.4630872
      -0.0163     28.6488095
      -0.0156     27.0531915
      -0.0149     25.6363636
      -0.0148     24.3679654
      -0.0142     23.2244094
      -0.0141     22.1870504
      -0.014      21.1677632
      -0.0138     20.2447130
      -0.0133     19.3472222
      -0.0132     18.5333333
      -0.0127     17.7909739
      -0.0126     17.1103753
      -0.0122     16.4835391
      -0.0119     15.9038462
      -0.0116     15.3657658
      -0.0108     14.8646362
      -0.0104     14.3964968
      -0.0099     13.9579580
      -0.0095     13.5254958
      -0.0093     13.1204819
      -0.0089     12.7401774
      -0.0083     12.3822115
      -0.0082     12.0445205
      -0.0079     11.7252986
      -0.0077     11.4229576
      -0.0074     11.1360947
      -0.0072     10.8634652
      -0.0071     10.5935252
      -0.007      10.3370593
      -0.0069     10.0930041
      -0.0068      9.8347758
      -0.0063      9.5823928
      -0.0062      9.3436599
      -0.0059      9.1174033
      -0.0054      8.9025845
      -0.0049      8.6982813
      -0.0044      8.4978593
      -0.004       8.3015873
      -0.0037      8.0993789
      -0.0036      7.9028757
      -0.0034      7.7165971
      -0.0033      7.5268440
      -0.0031      7.3471753
      -0.0028      7.1767442
      -0.0027      7.0147982
      -0.0025      6.8606664
      -0.0022      6.7105263
      -0.0021      6.5673931
      -0.0019      6.4307452
      -0.0018      6.3001133
      -0.0015      6.1750731
      -0.0012      6.0552408
      -0.001       5.9402678
      -9e-04       5.8298368
      -7e-04       5.7214863
      -6e-04       5.6173149
      -5e-04       5.5170628
      -4e-04       5.4204916
      -3e-04       5.3273827
      -2e-04       5.2375350
      1e-04        5.1507634
      4e-04        5.0668967
      6e-04        4.9842296
      8e-04        4.9042821
      9e-04        4.8269089
      0.0013       4.7492223
      0.0015       4.6740551
      0.0017       4.6012745
      0.0018       4.5295293
      0.0019       4.4600217
      0.0021       4.3914975
      0.0022       4.3217750
      0.0024       4.2542817
      0.0025       4.1878812
      0.0026       4.1225744
      0.0028       4.0593093
      0.003        3.9970658
      0.0031       3.9367270
      0.0032       3.8781984
      0.0033       3.8213918
      0.0034       3.7654259
      0.0035       3.7110747
      0.0037       3.6567667
      0.0039       3.6032993
      0.004        3.5506689
      0.0041       3.4988710
      0.0043       3.4478998
      0.0046       3.3983834
      0.005        3.3502545
      0.0051       3.3034502
      0.0052       3.2573330
      0.0053       3.2124601
      0.0057       3.1687769
      0.0058       3.1262322
      0.0059       3.0847777
      0.006        3.0443678
      0.0064       3.0049595
      0.0066       2.9660413
      0.0067       2.9280718
      0.0068       2.8901225
      0.0069       2.8526635
      0.007        2.8161148
      0.0071       2.7800304
      0.0072       2.7448078
      0.0073       2.7100251
      0.0075       2.6756812
      0.0076       2.6417745
      0.0077       2.6086611
      0.0078       2.5763109
      0.0079       2.5446952
      0.008        2.5131231
      0.0082       2.4819423
      0.0083       2.4511505
      0.0085       2.4210526
      0.0086       2.3916232
      0.0088       2.3628380
      0.0089       2.3346740
      0.009        2.3065521
      0.0091       2.2787633
      0.0092       2.2515708
      0.0093       2.2249538
      0.0094       2.1988924
      0.0095       2.1731204
      0.0096       2.1476366
      0.0097       2.1222037
      0.0099       2.0970671
      0.0104       2.0724500
      0.0106       2.0481144
      0.0108       2.0242739
      0.0109       2.0007015
      0.0111       1.9776011
      0.0113       1.9547561
      0.0114       1.9321647
      0.0119       1.9100179
      0.0121       1.8881128
      0.0125       1.8664478
      0.0126       1.8452018
      0.0127       1.8243615
      0.013        1.8039143
      0.0131       1.7838480
      0.0133       1.7641509
      0.0134       1.7444866
      0.0136       1.7251828
      0.0137       1.7062287
      0.0138       1.6876139
      0.0139       1.6693285
      0.014        1.6513629
      0.0142       1.6337079
      0.0145       1.6163546
      0.0147       1.5991557
      0.0148       1.5822471
      0.0152       1.5656208
      0.0153       1.5492689
      0.0156       1.5331842
      0.0157       1.5173592
      0.016        1.5017871
      0.0163       1.4863393
      0.0164       1.4711357
      0.0169       1.4561698
      0.0171       1.4410888
      0.0172       1.4262456
      0.0175       1.4116341
      0.0182       1.3972484
      0.0185       1.3830826
      0.0189       1.3691311
      0.0191       1.3553887
      0.0199       1.3418501
      0.0202       1.3285103
      0.0203       1.3153646
      0.0204       1.3024081
      0.0205       1.2895408
      0.0206       1.2768569
      0.0209       1.2643523
      0.0213       1.2520227
      0.0216       1.2398639
      0.0217       1.2278722
      0.0219       1.2160436
      0.022        1.2043745
      0.0222       1.1928613
      0.0223       1.1815005
      0.0225       1.1702888
      0.0233       1.1592229
      0.0244       1.1482996
      0.0256       1.1375160
      0.0267       1.1268689
      0.0274       1.1163556
      0.0275       1.1059732
      0.0282       1.0957189
      0.0286       1.0855903
      0.0303       1.0755846
      0.0311       1.0656994
      0.0312       1.0559323
      0.0313       1.0462808
      0.0317       1.0367428
      0.0334       1.0273160
      0.034        1.0179981
      0.0373       1.0087872
      0.0384       0.9996811
      0.04         0.9906778
      0.0435       0.9817755
      0.0483       0.9729721
      0.0622       0.9642659
      0.0666       0.9556551
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
      [1,] 0.08053795
      
      > data(managers)
      
      > MAR = 0
      
      > print(OmegaExcessReturn(managers["1996", 1], managers["1996", 
      +     8], MAR))
                [,1]
      [1,] 0.1325302
      
      > print(OmegaExcessReturn(managers["1996", 1:5], managers["1996", 
      +     8], MAR))
                                         HAM1 HAM2      HAM3      HAM4 HAM5
      Omega Excess Return (MAR = 0) 0.1325302   NA 0.3991416 0.1985718   NA
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
      [1,] 0.2917933
      
      > MAR = 0
      
      > data(managers)
      
      > print(OmegaSharpeRatio(managers["1996"], MAR))
                                      HAM1 HAM2     HAM3     HAM4 HAM5 HAM6
      OmegaSharpeRatio (MAR = 0%) 3.598338 2374 5.482813 2.615074   NA   NA
                                  EDHEC LS EQ SP500 TR  US 10Y TR US 3m TR
      OmegaSharpeRatio (MAR = 0%)          NA 3.340625 0.02827709      Inf
      
      > print(OmegaSharpeRatio(managers["1996", 1], MAR))
               [,1]
      [1,] 3.598338
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
      Pain Index                    0.0390113
      
      > data(managers)
      
      > print(PainIndex(100 * managers["1996"]))
                      HAM1  HAM2      HAM3      HAM4 HAM5 HAM6 EDHEC LS EQ  SP500 TR
      Pain Index 0.3714087 0.002 0.9421759 0.7421641  NaN  NaN         NaN 0.7336052
                 US 10Y TR US 3m TR
      Pain Index  3.697961        0
      
      > print(PainIndex(100 * managers["1996", 1]))
                      HAM1
      Pain Index 0.3714087
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
      Pain Index                     2.657647
      
      > data(managers)
      
      > print(PainRatio(managers["1996"]))
                             HAM1     HAM2     HAM3     HAM4 HAM5 HAM6 EDHEC LS EQ
      Pain Ratio (Rf = 0) 36.7226 36650.56 43.38967 28.17458   NA   NA          NA
                          SP500 TR  US 10Y TR US 3m TR
      Pain Ratio (Rf = 0) 31.62377 0.01188003      Inf
      
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
      Probabilistic Sharpe Ratio(p= 95 %):                          0.9209357
      
      $sr_confidence_interval
                            Lower Bound Sharpe Ratio Upper Bound
      Convertible Arbitrage      0.2109       0.3455      0.4802
      
      
      > ProbSharpeRatio(refSR = 1/12^0.5, Rf = 0, p = 0.95, 
      +     sr = 2/12^0.5, sk = -0.72, kr = 5.78, n = 59)
      $sr_prob
      [1] 0.9597434
      
      $sr_confidence_interval
        Lower Bound Sharpe Ratio Upper Bound
      1      0.3057       0.5774       0.849
      
      
      > ProbSharpeRatio(edhec[, 1:2], refSR = c(0.28, 0.24), 
      +     ignore_skewness = FALSE, ignore_kurtosis = FALSE)
      $sr_prob
      [1] 0.7601113
      
      $sr_confidence_interval
        Lower Bound Sharpe Ratio Upper Bound
      1       0.193       0.3455      0.4981
      
      
      > ProbSharpeRatio(edhec[, 1:2], refSR = c(0.28, 0.24), 
      +     ignore_skewness = FALSE, ignore_kurtosis = TRUE)
      $sr_prob
      [1] 0.7883343
      
      $sr_confidence_interval
        Lower Bound Sharpe Ratio Upper Bound
      1      0.2109       0.3455      0.4802
      
      
      > ProbSharpeRatio(edhec[, 1:2], refSR = c(0.28, 0.24), 
      +     ignore_skewness = TRUE, ignore_kurtosis = TRUE)
      $sr_prob
      [1] 0.8617196
      
      $sr_confidence_interval
        Lower Bound Sharpe Ratio Upper Bound
      1      0.2465       0.3455      0.4446
      
      
      > ProbSharpeRatio(edhec[, 1:2], refSR = 0.26, weights = c(0.5, 
      +     0.5), ignore_skewness = FALSE, ignore_kurtosis = FALSE)
      $sr_prob
                                           portfolio.returns (SR > 0.26 )
      Probabilistic Sharpe Ratio(p= 95 %):                      0.9552203
      
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
      [1,] -0.1347065
      
      > data(managers)
      
      > MAR = 0
      
      > print(ProspectRatio(managers["1996"], MAR))
                                     HAM1     HAM2     HAM3      HAM4 HAM5 HAM6
      Prospect ratio (MAR = 0%) 0.9737463 442.1359 1.725605 0.5960639   NA   NA
                                EDHEC LS EQ  SP500 TR  US 10Y TR US 3m TR
      Prospect ratio (MAR = 0%)          NA 0.7975008 -0.7234556      Inf
      
      > print(ProspectRatio(managers["1996", 1], MAR))
                [,1]
      [1,] 0.9737463
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
      RachevRatio 1.179419 1.338008 1.139389 0.9967392 1.514187 1.089176 1.135164
                        GM       LS     MA       RV       SS      FOF
      RachevRatio 2.014378 1.238694 1.5023 1.159463 1.134508 1.225545
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
      1996-02-29  0.022073985            NA  0.035101421
      1996-03-31  0.014614190            NA  0.025733921
      1996-04-30 -0.014834456            NA  0.045035710
      1996-05-31  0.011492903            NA  0.035231790
      1996-06-30 -0.006580742            NA -0.030766102
      1996-07-31 -0.027575673            NA -0.033724158
      1996-08-31  0.054092559            NA  0.046666996
      1996-09-30  0.008918922  0.1248798328  0.065436420
      1996-10-31  0.032086822  0.0174616062  0.039316685
      1996-11-30  0.012522975  0.0835177999  0.066792551
      1996-12-31  0.018066216  0.0189979595  0.021078844
      1997-01-31  0.022039189  0.0916045833  0.077495761
      1997-02-28 -0.002229051 -0.0297548689 -0.038213548
      1997-03-31  0.011078377 -0.0315013248 -0.033573000
      1997-04-30  0.013345946 -0.0009819489  0.029041945
      1997-05-31  0.051072969  0.0686636088  0.076236077
      1997-06-30  0.018274665  0.0555198782  0.004899082
      1997-07-31  0.013605069  0.1297143968  0.108829706
      1997-08-31  0.025634796 -0.0528443018 -0.003587969
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
      Annualized Return 0.03718883
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
      Cumulative Return 3.126671
      
      > Return.cumulative(managers[, 1:8])
                            HAM1     HAM2     HAM3    HAM4      HAM5      HAM6
      Cumulative Return 3.126671 4.348599 3.706732 2.52944 0.2650197 0.9858675
                        EDHEC LS EQ SP500 TR
      Cumulative Return    2.051197 1.761619
      
      > Return.cumulative(managers[, 1:8], geometric = FALSE)
                          HAM1   HAM2  HAM3   HAM4   HAM5   HAM6 EDHEC LS EQ SP500 TR
      Cumulative Return 1.4682 1.7679 1.643 1.4542 0.3148 0.7075      1.1454 1.143825
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
      1996-01-31  0.004066667
      1996-02-29  0.015966667
      1996-03-31  0.012166667
      1996-04-30 -0.012433333
      1996-05-31  0.004266667
      1996-06-30 -0.007233333
      
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
      1997-01-31       0.033400000
      1997-02-28       0.023762011
      1997-03-31      -0.001413340
      1997-04-30       0.003680000
      1997-05-31       0.017660872
      1997-06-30       0.025452430
      1997-07-31       0.036500000
      1997-08-31      -0.005136602
      1997-09-30       0.022049167
      1997-10-31      -0.010780000
      1997-11-30      -0.002621013
      1997-12-31       0.012985944
      
      > Return.portfolio(edhec["1997", 1:5], rebalance_on = "quarters", 
      +     verbose = TRUE)
      $returns
                 portfolio.returns
      1997-01-31       0.033400000
      1997-02-28       0.023762011
      1997-03-31      -0.001413340
      1997-04-30       0.003680000
      1997-05-31       0.017660872
      1997-06-30       0.025452430
      1997-07-31       0.036500000
      1997-08-31      -0.005136602
      1997-09-30       0.022049167
      1997-10-31      -0.010780000
      1997-11-30      -0.002621013
      1997-12-31       0.012985944
      
      $contribution
                 Convertible Arbitrage    CTA Global Distressed Securities
      1997-01-31           0.002380000  0.0078600000          0.0035600000
      1997-02-28           0.002408819  0.0059940275          0.0024031662
      1997-03-31           0.001510442 -0.0004248891         -0.0002337074
      1997-04-30           0.001720000 -0.0034000000          0.0006000000
      1997-05-31           0.003135294 -0.0002938187          0.0046568428
      1997-06-30           0.004252156  0.0016336242          0.0043610924
      1997-07-31           0.003860000  0.0118200000          0.0046800000
      1997-08-31           0.002635527 -0.0096662672          0.0029028423
      1997-09-30           0.002444218  0.0038748559          0.0070493383
      1997-10-31           0.002000000 -0.0019600000         -0.0012800000
      1997-11-30           0.000000000  0.0026626352          0.0010847819
      1997-12-31           0.001392218  0.0058170647          0.0014782579
                 Emerging Markets Equity Market Neutral
      1997-01-31      0.015820000          0.0037800000
      1997-02-28      0.010964341          0.0019916567
      1997-03-31     -0.002576485          0.0003112995
      1997-04-30      0.002380000          0.0023800000
      1997-05-31      0.006351596          0.0038109577
      1997-06-30      0.011874480          0.0033310776
      1997-07-31      0.011200000          0.0049400000
      1997-08-31     -0.001344834          0.0003361293
      1997-09-30      0.004659301          0.0040214532
      1997-10-31     -0.011440000          0.0019000000
      1997-11-30     -0.007205240          0.0008368108
      1997-12-31      0.002942265          0.0013561387
      
      $BOP.Weight
                 Convertible Arbitrage CTA Global Distressed Securities
      1997-01-31             0.2000000  0.2000000             0.2000000
      1997-02-28             0.1958390  0.2011419             0.1969808
      1997-03-31             0.1936464  0.2023282             0.1947562
      1997-04-30             0.2000000  0.2000000             0.2000000
      1997-05-31             0.2009804  0.1958792             0.1998645
      1997-06-30             0.2005734  0.1921911             0.2009720
      1997-07-31             0.2000000  0.2000000             0.2000000
      1997-08-31             0.1966811  0.2043608             0.1974723
      1997-09-30             0.2003458  0.1956998             0.2014097
      1997-10-31             0.2000000  0.2000000             0.2000000
      1997-11-30             0.2042013  0.2001981             0.2008855
      1997-12-31             0.2047379  0.2033939             0.2025011
                 Emerging Markets Equity Market Neutral
      1997-01-31        0.2000000             0.2000000
      1997-02-28        0.2088446             0.1971937
      1997-03-31        0.2147071             0.1945622
      1997-04-30        0.2000000             0.2000000
      1997-05-31        0.2016380             0.2016380
      1997-06-30        0.2043800             0.2018835
      1997-07-31        0.2000000             0.2000000
      1997-08-31        0.2037627             0.1977231
      1997-09-30        0.2034629             0.1990818
      1997-10-31        0.2000000             0.2000000
      1997-11-30        0.1906148             0.2041002
      1997-12-31        0.1838916             0.2054756
      
      $EOP.Weight
                 Convertible Arbitrage CTA Global Distressed Securities
      1997-01-31             0.1958390  0.2011419             0.1969808
      1997-02-28             0.1936464  0.2023282             0.1947562
      1997-03-31             0.1954330  0.2021890             0.1947978
      1997-04-30             0.2009804  0.1958792             0.1998645
      1997-05-31             0.2005734  0.1921911             0.2009720
      1997-06-30             0.1997416  0.1890138             0.2002366
      1997-07-31             0.1966811  0.2043608             0.1974723
      1997-08-31             0.2003458  0.1956998             0.2014097
      1997-09-30             0.1984151  0.1952691             0.2039618
      1997-10-31             0.2042013  0.2001981             0.2008855
      1997-11-30             0.2047379  0.2033939             0.2025011
      1997-12-31             0.2034876  0.2065290             0.2013644
                 Emerging Markets Equity Market Neutral
      1997-01-31        0.2088446             0.1971937
      1997-02-28        0.2147071             0.1945622
      1997-03-31        0.2124308             0.1951493
      1997-04-30        0.2016380             0.2016380
      1997-05-31        0.2043800             0.2018835
      1997-06-30        0.2108869             0.2001210
      1997-07-31        0.2037627             0.1977231
      1997-08-31        0.2034629             0.1990818
      1997-09-30        0.2036323             0.1987216
      1997-10-31        0.1906148             0.2041002
      1997-11-30        0.1838916             0.2054756
      1997-12-31        0.1844387             0.2041802
      
      $BOP.Value
                 Convertible Arbitrage CTA Global Distressed Securities
      1997-01-31             0.2000000  0.2000000             0.2000000
      1997-02-28             0.2023800  0.2078600             0.2035600
      1997-03-31             0.2048693  0.2140542             0.2060434
      1997-04-30             0.2112921  0.2112921             0.2112921
      1997-05-31             0.2131092  0.2077001             0.2119260
      1997-06-30             0.2164337  0.2073886             0.2168638
      1997-07-31             0.2213080  0.2213080             0.2213080
      1997-08-31             0.2255792  0.2343873             0.2264866
      1997-09-30             0.2286020  0.2233008             0.2298159
      1997-10-31             0.2332393  0.2332393             0.2332393
      1997-11-30             0.2355716  0.2309535             0.2317465
      1997-12-31             0.2355716  0.2340252             0.2329980
                 Emerging Markets Equity Market Neutral
      1997-01-31        0.2000000             0.2000000
      1997-02-28        0.2158200             0.2037800
      1997-03-31        0.2271506             0.2058382
      1997-04-30        0.2112921             0.2112921
      1997-05-31        0.2138065             0.2138065
      1997-06-30        0.2205414             0.2178474
      1997-07-31        0.2213080             0.2213080
      1997-08-31        0.2337012             0.2267743
      1997-09-30        0.2321588             0.2271598
      1997-10-31        0.2332393             0.2332393
      1997-11-30        0.2198980             0.2354550
      1997-12-31        0.2115858             0.2364204
      
      $EOP.Value
                 Convertible Arbitrage CTA Global Distressed Securities
      1997-01-31             0.2023800  0.2078600             0.2035600
      1997-02-28             0.2048693  0.2140542             0.2060434
      1997-03-31             0.2064673  0.2136047             0.2057962
      1997-04-30             0.2131092  0.2077001             0.2119260
      1997-05-31             0.2164337  0.2073886             0.2168638
      1997-06-30             0.2210221  0.2091514             0.2215698
      1997-07-31             0.2255792  0.2343873             0.2264866
      1997-08-31             0.2286020  0.2233008             0.2298159
      1997-09-30             0.2313909  0.2277221             0.2378595
      1997-10-31             0.2355716  0.2309535             0.2317465
      1997-11-30             0.2355716  0.2340252             0.2329980
      1997-12-31             0.2371735  0.2407183             0.2346988
                 Emerging Markets Equity Market Neutral
      1997-01-31        0.2158200             0.2037800
      1997-02-28        0.2271506             0.2058382
      1997-03-31        0.2244247             0.2061675
      1997-04-30        0.2138065             0.2138065
      1997-05-31        0.2205414             0.2178474
      1997-06-30        0.2333548             0.2214419
      1997-07-31        0.2337012             0.2267743
      1997-08-31        0.2321588             0.2271598
      1997-09-30        0.2374752             0.2317484
      1997-10-31        0.2198980             0.2354550
      1997-11-30        0.2115858             0.2364204
      1997-12-31        0.2149712             0.2379808
      
      
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
      1996-01-31     0.9742747            NA      1.000870
      1996-02-29     0.9839276            NA      1.026455
      1996-03-31     0.9896776            NA      1.042925
      1996-04-30     0.9664645            NA      1.073965
      1996-05-31     0.9493173            NA      1.083912
      1996-06-30     0.9420352            NA      1.047090
      1996-07-31     0.9628313            NA      1.058593
      1996-08-31     0.9801813     0.9792381      1.084511
      1996-09-30     0.9415791     1.0199354      1.093751
      1996-10-31     0.9426786     1.0260891      1.106417
      1996-11-30     0.8900934     1.0242766      1.097159
      1996-12-31     0.9240553     1.0761069      1.143275
      1997-01-31     0.8881367     1.0932234      1.158985
      1997-02-28     0.8832017     1.0758672      1.107005
      1997-03-31     0.9297150     1.0917993      1.115663
      1997-04-30     0.8883924     1.0240062      1.082921
      1997-05-31     0.8740730     1.0172496      1.098232
      1997-06-30     0.8559189     1.0273754      1.056817
      1997-07-31     0.8050204     1.0610629      1.084716
      1997-08-31     0.8729867     1.1018644      1.145846
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
      [1] 0.005774729
      
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
      [1] 0.3900712
      
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
      [1] 0.5226596
      
      > SFM.beta.bear(managers[, "HAM2"], managers[, "SP500 TR"], 
      +     Rf = managers[, "US 3m TR"])
      [1] 0.0698255
      
      > TimingRatio(managers[, "HAM2"], managers[, "SP500 TR"], 
      +     Rf = managers[, "US 3m TR"])
      [1] 7.485224
      
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
      [1] -0.01416944
      
      > data(managers)
      
      > print(Selectivity(managers["2002", 1], managers["2002", 
      +     8]))
      [1] 0.03780793
      
      > print(Selectivity(managers["2002", 1:5], managers["2002", 
      +     8]))
                                           HAM1       HAM2        HAM3       HAM4
      Jensen's Alpha (Risk free = 0) 0.03780793 -0.1015863 -0.09419238 0.09430126
                                            HAM5
      Jensen's Alpha (Risk free = 0) -0.08667631
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
      StdDev Sharpe (Rf=0.3%, p=95%): 0.3201889
      
      > SharpeRatio(managers[, 1, drop = FALSE], Rf = managers[, 
      +     10, drop = FALSE], FUN = "StdDev")
                                          HAM1
      StdDev Sharpe (Rf=0.3%, p=95%): 0.308102
      
      > SharpeRatio(managers[, 1:6], Rf = 0.035/12, FUN = "StdDev")
                                           HAM1      HAM2      HAM3      HAM4
      StdDev Sharpe (Rf=0.3%, p=95%): 0.3201889 0.3057649 0.2610141 0.1522615
                                            HAM5      HAM6
      StdDev Sharpe (Rf=0.3%, p=95%): 0.02562009 0.3417545
      
      > SharpeRatio(managers[, 1:6], Rf = managers[, 10, drop = FALSE], 
      +     FUN = "StdDev")
                                          HAM1      HAM2      HAM3      HAM4
      StdDev Sharpe (Rf=0.3%, p=95%): 0.308102 0.2988608 0.2525301 0.1464385
                                           HAM5      HAM6
      StdDev Sharpe (Rf=0.3%, p=95%): 0.0354554 0.3785371
      
      > data(edhec)
      
      > SharpeRatio(edhec[, 6, drop = FALSE], FUN = "VaR")
                                 Event Driven
      VaR Sharpe (Rf=0%, p=95%):    0.2254086
      
      > SharpeRatio(edhec[, 6, drop = FALSE], Rf = 0.04/12, 
      +     FUN = "VaR")
                                   Event Driven
      VaR Sharpe (Rf=0.3%, p=95%):    0.1128292
      
      > SharpeRatio(edhec[, 6, drop = FALSE], Rf = 0.04/12, 
      +     FUN = "VaR", method = "gaussian")
                                   Event Driven
      VaR Sharpe (Rf=0.3%, p=95%):     0.135566
      
      > SharpeRatio(edhec[, 6, drop = FALSE], FUN = "ES")
                                Event Driven
      ES Sharpe (Rf=0%, p=95%):   0.08225819
      
      > SharpeRatio(managers[, 1:9], Rf = managers[, 10, drop = FALSE])
                                           HAM1      HAM2      HAM3       HAM4
      StdDev Sharpe (Rf=0.3%, p=95%): 0.3081020 0.2988608 0.2525301 0.14643845
      VaR Sharpe (Rf=0.3%, p=95%):    0.2306863 0.3970699 0.2504936 0.09553906
      ES Sharpe (Rf=0.3%, p=95%):     0.1295014 0.1788256 0.2093343 0.06625013
      SemiSD Sharpe (Rf=0.3%, p=95%): 0.2939322        NA 0.2756616 0.13937238
                                            HAM5      HAM6 EDHEC LS EQ   SP500 TR
      StdDev Sharpe (Rf=0.3%, p=95%): 0.03545540 0.3785371   0.3142695 0.12558293
      VaR Sharpe (Rf=0.3%, p=95%):    0.02399862 0.3022965   0.2737607 0.07957460
      ES Sharpe (Rf=0.3%, p=95%):     0.01664487 0.2308450   0.1855867 0.05760917
      SemiSD Sharpe (Rf=0.3%, p=95%):         NA        NA          NA 0.11811218
                                       US 10Y TR
      StdDev Sharpe (Rf=0.3%, p=95%): 0.05684359
      VaR Sharpe (Rf=0.3%, p=95%):    0.03741555
      ES Sharpe (Rf=0.3%, p=95%):     0.02610548
      SemiSD Sharpe (Rf=0.3%, p=95%): 0.05544425
      
      > SharpeRatio(edhec, Rf = 0.04/12)
                                      Convertible Arbitrage CTA Global
      StdDev Sharpe (Rf=0.3%, p=95%):            0.14668811 0.04318355
      VaR Sharpe (Rf=0.3%, p=95%):               0.09573383 0.03071283
      ES Sharpe (Rf=0.3%, p=95%):                0.02749804 0.02436990
      SemiSD Sharpe (Rf=0.3%, p=95%):            0.12742933 0.04448386
                                      Distressed Securities Emerging Markets
      StdDev Sharpe (Rf=0.3%, p=95%):            0.19243015       0.10385437
      VaR Sharpe (Rf=0.3%, p=95%):               0.12468723       0.06357551
      ES Sharpe (Rf=0.3%, p=95%):                0.05185797       0.02935853
      SemiSD Sharpe (Rf=0.3%, p=95%):            0.16964280       0.09420548
                                      Equity Market Neutral Event Driven
      StdDev Sharpe (Rf=0.3%, p=95%):            0.12208608   0.17516507
      VaR Sharpe (Rf=0.3%, p=95%):               0.09119925   0.11282916
      ES Sharpe (Rf=0.3%, p=95%):                0.02730312   0.04117466
      SemiSD Sharpe (Rf=0.3%, p=95%):            0.10912571   0.15387917
                                      Fixed Income Arbitrage Global Macro
      StdDev Sharpe (Rf=0.3%, p=95%):             0.09571851    0.1548462
      VaR Sharpe (Rf=0.3%, p=95%):                0.06182798    0.1640095
      ES Sharpe (Rf=0.3%, p=95%):                 0.02066033    0.1334925
      SemiSD Sharpe (Rf=0.3%, p=95%):             0.07810721    0.1724268
                                      Long/Short Equity Merger Arbitrage
      StdDev Sharpe (Rf=0.3%, p=95%):        0.16187593       0.19589976
      VaR Sharpe (Rf=0.3%, p=95%):           0.11467174       0.14961866
      ES Sharpe (Rf=0.3%, p=95%):            0.06966206       0.04383052
      SemiSD Sharpe (Rf=0.3%, p=95%):        0.15390702       0.17840739
                                      Relative Value Short Selling Funds of Funds
      StdDev Sharpe (Rf=0.3%, p=95%):     0.20179571   -0.10095636     0.07325342
      VaR Sharpe (Rf=0.3%, p=95%):        0.13789129   -0.07391375     0.05102233
      ES Sharpe (Rf=0.3%, p=95%):         0.05040732   -0.06801430     0.02566824
      SemiSD Sharpe (Rf=0.3%, p=95%):     0.17429478   -0.10985959     0.06968278
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
      Annualized Sharpe Ratio (Rf=3.5%) 1.112293
      
      > SharpeRatio.annualized(managers[, 1, drop = FALSE], 
      +     Rf = managers[, 10, drop = FALSE])
                                            HAM1
      Annualized Sharpe Ratio (Rf=3.9%) 1.066795
      
      > SharpeRatio.annualized(managers[, 1:6], Rf = 0.035/12)
                                            HAM1     HAM2      HAM3      HAM4
      Annualized Sharpe Ratio (Rf=3.5%) 1.112293 1.059085 0.8854393 0.4512555
                                              HAM5     HAM6
      Annualized Sharpe Ratio (Rf=3.5%) 0.01046591 1.194146
      
      > SharpeRatio.annualized(managers[, 1:6], Rf = managers[, 
      +     10, drop = FALSE])
                                            HAM1     HAM2      HAM3      HAM4
      Annualized Sharpe Ratio (Rf=3.9%) 1.066795 1.033019 0.8539722 0.4292173
                                              HAM5     HAM6
      Annualized Sharpe Ratio (Rf=3.9%) 0.04441162 1.333868
      
      > SharpeRatio.annualized(managers[, 1:6], Rf = managers[, 
      +     10, drop = FALSE], geometric = FALSE)
                                            HAM1     HAM2      HAM3      HAM4
      Annualized Sharpe Ratio (Rf=3.9%) 1.067297 1.035284 0.8747901 0.5072777
                                             HAM5     HAM6
      Annualized Sharpe Ratio (Rf=3.9%) 0.1228211 1.311291
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
      [1] 0.03609444
      
      $contribution
       [1]  0.0047334086 -0.0004571893  0.0045351937  0.0065860529  0.0011205219
       [6]  0.0049801251  0.0028500976  0.0011928528  0.0037860411  0.0026631912
      [11]  0.0031519341 -0.0024524364  0.0034046495
      
      $pct_contrib_MES
       [1]  0.13113954 -0.01266647  0.12564798  0.18246723  0.03104417  0.13797484
       [7]  0.07896223  0.03304810  0.10489263  0.07378397  0.08732464 -0.06794499
      [13]  0.09432614
      
      
      > sigma <- cov(edhec)
      
      > m3 <- M3.MM(edhec)
      
      > m4 <- M4.MM(edhec)
      
      > ES(p = 0.95, portfolio_method = "component", weights = rep(1/p, 
      +     p), mu = mu, sigma = sigma, m3 = m3, m4 = m4)
      $MES
      [1] 0.03625114
      
      $contribution
       [1]  0.0059762619 -0.0028081561  0.0052816918  0.0062522301  0.0009068639
       [6]  0.0058974265  0.0034627291  0.0001339207  0.0036259163  0.0034602269
      [11]  0.0037776798 -0.0031537633  0.0034381082
      
      $pct_contrib_MES
       [1]  0.164857231 -0.077463949  0.145697277  0.172469910  0.025016152
       [6]  0.162682530  0.095520570  0.003694249  0.100022144  0.095451544
      [11]  0.104208592 -0.086997642  0.094841391
      
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
      [1] -0.03394204
      
      > data(managers)
      
      > print(SkewnessKurtosisRatio(managers["1996"]))
                                  HAM1      HAM2       HAM3       HAM4 HAM5 HAM6
      SkewnessKurtosisRatio -0.1364114 0.1279073 -0.3322627 -0.0264609   NA   NA
                            EDHEC LS EQ    SP500 TR   US 10Y TR   US 3m TR
      SkewnessKurtosisRatio          NA -0.03981589 -0.01634447 -0.2626715
      
      > print(SkewnessKurtosisRatio(managers["1996", 1]))
      [1] -0.1364114
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
      0.621107 
      
      > SmoothingIndex(managers[, 1:8])
                          HAM1      HAM2      HAM3      HAM4 HAM5      HAM6
      Smoothing Index 0.621107 0.4920349 0.6629845 0.6710542    1 0.4929217
                      EDHEC LS EQ  SP500 TR
      Smoothing Index   0.5378922 0.9528486
      
      > SmoothingIndex(edhec)
                      Convertible Arbitrage CTA Global Distressed Securities
      Smoothing Index             0.4293031  0.9493503             0.4512464
                      Emerging Markets Equity Market Neutral Event Driven
      Smoothing Index        0.5626008             0.4756124    0.5362955
                      Fixed Income Arbitrage Global Macro Long/Short Equity
      Smoothing Index              0.4312082    0.6631918         0.5935058
                      Merger Arbitrage Relative Value Short Selling Funds of Funds
      Smoothing Index        0.5245479      0.4632014     0.7535638      0.5474126
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
      [1] 0.03293109
      
      > data(managers)
      
      > print(SpecificRisk(managers["1996", 1], managers["1996", 
      +     8]))
      [1] 0.04977046
      
      > print(SpecificRisk(managers["1996", 1:5], managers["1996", 
      +     8]))
                             HAM1 HAM2       HAM3      HAM4 HAM5
      Specific Risk =  0.04977046   NA 0.06225051 0.0857627   NA
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
      StdDev            0.01676221 0.02278814            0.01814467       0.03270967
             Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      StdDev           0.008208647   0.01907188             0.01145756   0.01462496
             Long/Short Equity Merger Arbitrage Relative Value Short Selling
      StdDev        0.02090324       0.01147821     0.01186841    0.04550226
             Funds of Funds
      StdDev     0.01608486
      
      > StdDev(edhec, portfolio_method = "single")
             Convertible Arbitrage CTA Global Distressed Securities Emerging Markets
      StdDev            0.01676221 0.02278814            0.01814467       0.03270967
             Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      StdDev           0.008208647   0.01907188             0.01145756   0.01462496
             Long/Short Equity Merger Arbitrage Relative Value Short Selling
      StdDev        0.02090324       0.01147821     0.01186841    0.04550226
             Funds of Funds
      StdDev     0.01608486
      
      > StdDev(edhec, clean = "boudt")
             Convertible Arbitrage CTA Global Distressed Securities Emerging Markets
      StdDev            0.01399677 0.02278814            0.01649289       0.03081375
             Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      StdDev           0.007394652   0.01751999            0.009510082   0.01431055
             Long/Short Equity Merger Arbitrage Relative Value Short Selling
      StdDev         0.0205263       0.01037732     0.01059619     0.0435232
             Funds of Funds
      StdDev     0.01589405
      
      > StdDev(edhec, clean = "boudt", portfolio_method = "component")
    Message
      no weights passed in, assuming equal weighted portfolio
    Output
      $StdDev
      [1] 0.01016549
      
      $contribution
       Convertible Arbitrage             CTA Global  Distressed Securities 
                0.0008134515           0.0006489133           0.0010437732 
            Emerging Markets  Equity Market Neutral           Event Driven 
                0.0019206079           0.0004275764           0.0011767665 
      Fixed Income Arbitrage           Global Macro      Long/Short Equity 
                0.0005332522           0.0009011841           0.0012966454 
            Merger Arbitrage         Relative Value          Short Selling 
                0.0005773683           0.0007041049          -0.0009687674 
              Funds of Funds 
                0.0010906127 
      
      $pct_contrib_StdDev
       Convertible Arbitrage             CTA Global  Distressed Securities 
                  0.08002089             0.06383493             0.10267811 
            Emerging Markets  Equity Market Neutral           Event Driven 
                  0.18893414             0.04206157             0.11576093 
      Fixed Income Arbitrage           Global Macro      Long/Short Equity 
                  0.05245712             0.08865133             0.12755366 
            Merger Arbitrage         Relative Value          Short Selling 
                  0.05679691             0.06926425            -0.09529964 
              Funds of Funds 
                  0.10728581 
      
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
      Annualized Standard Deviation              0.058066 0.07894044
                                    Distressed Securities Emerging Markets
      Annualized Standard Deviation            0.06285498        0.1133096
                                    Equity Market Neutral Event Driven
      Annualized Standard Deviation            0.02843559   0.06606695
                                    Fixed Income Arbitrage Global Macro
      Annualized Standard Deviation             0.03969016   0.05066234
                                    Long/Short Equity Merger Arbitrage Relative Value
      Annualized Standard Deviation        0.07241095       0.03976167     0.04111338
                                    Short Selling Funds of Funds
      Annualized Standard Deviation     0.1576245     0.05571958
      
      > sd.annualized(edhec[, 6, drop = FALSE])
                                    Event Driven
      Annualized Standard Deviation   0.06606695
      
      > sd.multiperiod(edhec[, 6, drop = FALSE], scale = 3)
                                    Event Driven
      Annualized Standard Deviation   0.03303347
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
      [1] 0.00666102
      
      $contribution
       [1] -2.838003e-04  1.072186e-03 -5.284652e-06  2.069026e-03 -4.845456e-04
       [6]  2.444378e-05 -5.794060e-04 -1.639684e-05  5.297645e-04 -4.910597e-04
      [11] -4.303054e-04  5.151664e-03  1.047346e-04
      
      $pct_contrib_MES
       [1] -0.0426061378  0.1609642208 -0.0007933698  0.3106170025 -0.0727434504
       [6]  0.0036696757 -0.0869845869 -0.0024616107  0.0795320410 -0.0737213987
      [11] -0.0646005356  0.7734046409  0.0157235091
      
      
      > sigma <- cov(edhec)
      
      > m3 <- M3.MM(edhec)
      
      > m4 <- M4.MM(edhec)
      
      > ES(p = 0.95, portfolio_method = "component", weights = rep(1/p, 
      +     p), mu = mu, sigma = sigma, m3 = m3, m4 = m4)
      $MES
      [1] 0.03625114
      
      $contribution
       [1]  0.0059762619 -0.0028081561  0.0052816918  0.0062522301  0.0009068639
       [6]  0.0058974265  0.0034627291  0.0001339207  0.0036259163  0.0034602269
      [11]  0.0037776798 -0.0031537633  0.0034381082
      
      $pct_contrib_MES
       [1]  0.164857231 -0.077463949  0.145697277  0.172469910  0.025016152
       [6]  0.162682530  0.095520570  0.003694249  0.100022144  0.095451544
      [11]  0.104208592 -0.086997642  0.094841391
      
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
      [1] 0.132806
      
      > data(managers)
      
      > print(SystematicRisk(managers["2002", 1], managers["2002", 
      +     8]))
      [1] 0.1103665
      
      > print(SystematicRisk(managers["2002", 1:5], managers["2002", 
      +     8]))
                                                HAM1       HAM2       HAM3      HAM4
      Systematic Risk to SP500 TR (Rf = 0) 0.1103665 0.02041913 0.08939036 0.1651298
                                                 HAM5
      Systematic Risk to SP500 TR (Rf = 0) 0.02013523
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
      [1] 0.136828
      
      > data(managers)
      
      > print(TotalRisk(managers["1996", 1], managers["1996", 
      +     8]))
      [1] 0.05627721
      
      > print(TotalRisk(managers["1996", 1:5], managers["1996", 
      +     8]))
                          HAM1 HAM2      HAM3      HAM4 HAM5
      Total Risk =  0.05627721   NA 0.1079938 0.1099375   NA
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
      [1] 0.1131667
      
      > TrackingError(managers[, 1:6], managers[, 8, drop = FALSE])
                                    HAM1      HAM2      HAM3      HAM4      HAM5
      Tracking Error: SP500 TR 0.1131667 0.1533647 0.1158673 0.1596656 0.1800291
                                   HAM6
      Tracking Error: SP500 TR 0.112839
      
      > TrackingError(managers[, 1:6], managers[, 8:7, drop = FALSE])
                                        HAM1       HAM2       HAM3      HAM4
      Tracking Error: SP500 TR    0.11316666 0.15336472 0.11586735 0.1596656
      Tracking Error: EDHEC LS EQ 0.07577263 0.09071824 0.08156934 0.1569067
                                       HAM5       HAM6
      Tracking Error: SP500 TR    0.1800291 0.11283904
      Tracking Error: EDHEC LS EQ 0.1421533 0.05651657
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
      [1] 0.7806747
      
      > print(TreynorRatio(managers["2002", 1], managers["2002", 
      +     8], modified = TRUE))
      [1] -0.727545
      
      > print(TreynorRatio(managers["2002", 1:5], managers["2002", 
      +     8], modified = TRUE))
                                   HAM1      HAM2      HAM3       HAM4     HAM5
      Treynor Ratio: SP500 TR -0.727545 -6.045167 -2.123832 -0.4990387 -5.37482
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
      SP500 TR Up Capture   0.6346612
      SP500 TR Down Capture 0.2076304
      SP500 TR Up Number    0.8941176
      SP500 TR Down Number  0.5106383
      SP500 TR Up Percent   0.2941176
      SP500 TR Down Percent 0.8085106
      
      > UpDownRatios(managers[, 1:6, drop = FALSE], managers[, 
      +     8, drop = FALSE])
                                 HAM1       HAM2      HAM3      HAM4      HAM5
      SP500 TR Up Capture   0.6346612 0.66155321 0.7913921 0.9272348 0.5529663
      SP500 TR Down Capture 0.2076304 0.04392011 0.3669345 0.7007885 0.3520355
      SP500 TR Up Number    0.8941176 0.68354430 0.8705882 0.7647059 0.7083333
      SP500 TR Down Number  0.5106383 0.69565217 0.7659574 0.6595745 0.7241379
      SP500 TR Up Percent   0.2941176 0.36708861 0.4235294 0.4588235 0.3958333
      SP500 TR Down Percent 0.8085106 0.86956522 0.8085106 0.5319149 0.8275862
                                 HAM6
      SP500 TR Up Capture   0.8068482
      SP500 TR Down Capture 0.2397750
      SP500 TR Up Number    0.8372093
      SP500 TR Down Number  0.5238095
      SP500 TR Up Percent   0.5348837
      SP500 TR Down Percent 0.7142857
      
      > UpDownRatios(managers[, 1, drop = FALSE], managers[, 
      +     8, drop = FALSE], method = "Capture")
                                 HAM1
      SP500 TR Up Capture   0.6346612
      SP500 TR Down Capture 0.2076304
      
      > UpDownRatios(managers[, 1, drop = FALSE], managers[, 
      +     8, drop = FALSE], side = "Up", method = "Capture")
      [1] 0.6346612
      
      > UpDownRatios(managers[, 1, drop = FALSE], managers[, 
      +     8, drop = FALSE], side = "Down", method = "Capture")
      [1] 0.2076304
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
      [1] 0.5416667
      
      > data(managers)
      
      > print(UpsideFrequency(managers["1996"]))
                                  HAM1 HAM2      HAM3      HAM4 HAM5 HAM6 EDHEC LS EQ
      Upside Frequency (MAR = 0%) 0.75  0.8 0.8333333 0.6666667  NaN  NaN         NaN
                                   SP500 TR US 10Y TR US 3m TR
      Upside Frequency (MAR = 0%) 0.8333333 0.4166667        1
      
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
      Upside Potential (MAR = 0.4%)    0.5448414
      
      > UpsidePotentialRatio(edhec[, 1:6], MAR = 0)
                                  Convertible Arbitrage CTA Global
      Upside Potential (MAR = 0%)             0.4988543   1.055208
                                  Distressed Securities Emerging Markets
      Upside Potential (MAR = 0%)             0.6986485        0.6074726
                                  Equity Market Neutral Event Driven
      Upside Potential (MAR = 0%)             0.6077271    0.5938243
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
      [1] 0.02937332
      
      > print(UpsideRisk(portfolio_bacon[, 1], MAR, stat = "variance"))
      [1] 0.0008627917
      
      > print(UpsideRisk(portfolio_bacon[, 1], MAR, stat = "potential"))
      [1] 0.01770833
      
      > MAR = 0
      
      > data(managers)
      
      > print(UpsideRisk(managers["1996"], MAR, stat = "risk"))
                                   HAM1       HAM2       HAM3      HAM4 HAM5 HAM6
      Upside Risk (MAR = 0%) 0.01799111 0.05916453 0.04002169 0.0321782    0    0
                             EDHEC LS EQ   SP500 TR  US 10Y TR    US 3m TR
      Upside Risk (MAR = 0%)           0 0.03204533 0.01397565 0.004324785
      
      > print(UpsideRisk(managers["1996", 1], MAR, stat = "risk"))
      [1] 0.01799111
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
      VaR           -0.02173214 -0.03310173           -0.02296944      -0.04698035
          Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      VaR          -0.009143467  -0.02464282            -0.01438379  -0.01841688
          Long/Short Equity Merger Arbitrage Relative Value Short Selling
      VaR       -0.02760698      -0.01326581    -0.01376013   -0.07597714
          Funds of Funds
      VaR    -0.02190044
      
      > VaR(edhec, p = 0.95, method = "modified")
          Convertible Arbitrage CTA Global Distressed Securities Emerging Markets
      VaR           -0.02568389 -0.0320411           -0.02800272      -0.05343318
          Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      VaR            -0.0109887  -0.02960873            -0.01773794  -0.01380785
          Long/Short Equity Merger Arbitrage Relative Value Short Selling
      VaR       -0.02950798      -0.01502873    -0.01736871   -0.06215004
          Funds of Funds
      VaR    -0.02309324
      
      > VaR(edhec, p = 0.99)
          Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      VaR           -0.09538713 -0.04561466           -0.07097989       -0.1261338
          Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      VaR           -0.03875142  -0.08433448            -0.06036075  -0.02309801
          Long/Short Equity Merger Arbitrage Relative Value Short Selling
      VaR       -0.05658921      -0.05760895    -0.04882532    -0.1093869
          Funds of Funds
      VaR    -0.05423976
      
      > VaR(edhec, p = 0.01)
          Convertible Arbitrage  CTA Global Distressed Securities Emerging Markets
      VaR           -0.09538713 -0.04561466           -0.07097989       -0.1261338
          Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      VaR           -0.03875142  -0.08433448            -0.06036075  -0.02309801
          Long/Short Equity Merger Arbitrage Relative Value Short Selling
      VaR       -0.05658921      -0.05760895    -0.04882532    -0.1093869
          Funds of Funds
      VaR    -0.05423976
      
      > VaR(edhec, clean = "boudt")
          Convertible Arbitrage CTA Global Distressed Securities Emerging Markets
      VaR           -0.01713444 -0.0320411            -0.0227273      -0.04687401
          Equity Market Neutral Event Driven Fixed Income Arbitrage Global Macro
      VaR          -0.008476936  -0.02485092            -0.01355527   -0.0146318
          Long/Short Equity Merger Arbitrage Relative Value Short Selling
      VaR       -0.02811176      -0.01116024    -0.01348449   -0.06821057
          Funds of Funds
      VaR    -0.02236299
      
      > VaR(edhec, clean = "boudt", portfolio_method = "component")
    Message
      no weights passed in, assuming equal weighted portfolio
    Output
      $MVaR
      [1] 0.01241465
      
      $contribution
       Convertible Arbitrage             CTA Global  Distressed Securities 
                0.0011098370           0.0003987871           0.0014558713 
            Emerging Markets  Equity Market Neutral           Event Driven 
                0.0028661906           0.0004461122           0.0016651346 
      Fixed Income Arbitrage           Global Macro      Long/Short Equity 
                0.0007723257           0.0008972835           0.0017417050 
            Merger Arbitrage         Relative Value          Short Selling 
                0.0005907568           0.0009219361          -0.0020761974 
              Funds of Funds 
                0.0016249056 
      
      $pct_contrib_MVaR
       Convertible Arbitrage             CTA Global  Distressed Securities 
                  0.08939738             0.03212230             0.11727045 
            Emerging Markets  Equity Market Neutral           Event Driven 
                  0.23087168             0.03593434             0.13412660 
      Fixed Income Arbitrage           Global Macro      Long/Short Equity 
                  0.06221084             0.07227619             0.14029435 
            Merger Arbitrage         Relative Value          Short Selling 
                  0.04758547             0.07426196            -0.16723772 
              Funds of Funds 
                  0.13088616 
      
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
      [1,] 1.323046
      
      > print(VolatilitySkewness(portfolio_bacon[, 1], MAR, 
      +     stat = "variability"))
               [,1]
      [1,] 1.150238
      
      > MAR = 0
      
      > data(managers)
      
      > print(VolatilitySkewness(managers["1996", 1], MAR, 
      +     stat = "volatility"))
               [,1]
      [1,] 6.149423
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
      0.007400000 0.013350000 0.014066667 0.008275000 0.008140000 0.006133333 
       1996-07-31  1996-08-31  1996-09-30  1996-10-31  1996-11-30  1996-12-31 
      0.001957143 0.006650000 0.007544444 0.009670000 0.010209091 0.010825000 
       1997-01-31  1997-02-28  1997-03-31  1997-04-30  1997-05-31  1997-06-30 
      0.011623077 0.010950000 0.010846667 0.010956250 0.012888235 0.013455556 
       1997-07-31  1997-08-31  1997-09-30  1997-10-31  1997-11-30  1997-12-31 
      0.013557895 0.014065000 0.014438095 0.012840909 0.013369565 0.013270833 
       1998-01-31  1998-02-28  1998-03-31  1998-04-30  1998-05-31  1998-06-30 
      0.012964000 0.014115385 0.014933333 0.014678571 0.013375862 0.013333333 
       1998-07-31  1998-08-31  1998-09-30  1998-10-31  1998-11-30  1998-12-31 
      0.012209677 0.008878125 0.009360606 0.010726471 0.010780000 0.010750000 
       1999-01-31  1999-02-28  1999-03-31  1999-04-30  1999-05-31  1999-06-30 
      0.010208108 0.010186842 0.011110256 0.012107500 0.012207317 0.012692857 
       1999-07-31  1999-08-31  1999-09-30  1999-10-31  1999-11-30  1999-12-31 
      0.012625581 0.011963636 0.011597778 0.011332609 0.011165957 0.011239583 
       2000-01-31  2000-02-29  2000-03-31  2000-04-30  2000-05-31  2000-06-30 
      0.010802041 0.010832000 0.011747059 0.011909615 0.012324528 0.012324074 
       2000-07-31  2000-08-31  2000-09-30  2000-10-31  2000-11-30  2000-12-31 
      0.012189091 0.012662500 0.012463158 0.012115517 0.012086441 0.011771667 
       2001-01-31  2001-02-28  2001-03-31  2001-04-30  2001-05-31  2001-06-30 
      0.011708197 0.011651613 0.011300000 0.011662500 0.012373846 0.012218182 
       2001-07-31  2001-08-31  2001-09-30  2001-10-31  2001-11-30  2001-12-31 
      0.012344776 0.012400000 0.011768116 0.011615714 0.011930986 0.012704167 
       2002-01-31  2002-02-28  2002-03-31  2002-04-30  2002-05-31  2002-06-30 
      0.012715068 0.012375676 0.012294667 0.012193421 0.012015584 0.011552564 
       2002-07-31  2002-08-31  2002-09-30  2002-10-31  2002-11-30  2002-12-31 
      0.010450633 0.010416250 0.009577778 0.009823171 0.010501205 0.009991667 
       2003-01-31  2003-02-28  2003-03-31  2003-04-30  2003-05-31  2003-06-30 
      0.009389412 0.008988372 0.009303448 0.009937500 0.010204494 0.010433333 
       2003-07-31  2003-08-31  2003-09-30  2003-10-31  2003-11-30  2003-12-31 
      0.010512088 0.010401087 0.010386022 0.010787234 0.010851579 0.011025000 
       2004-01-31  2004-02-29  2004-03-31  2004-04-30  2004-05-31  2004-06-30 
      0.010965979 0.010853061 0.010831313 0.010680000 0.010655446 0.010804902 
       2004-07-31  2004-08-31  2004-09-30  2004-10-31  2004-11-30  2004-12-31 
      0.010700000 0.010650000 0.010632381 0.010526415 0.010796262 0.011103704 
       2005-01-31  2005-02-28  2005-03-31  2005-04-30  2005-05-31  2005-06-30 
      0.011002752 0.011098182 0.010811712 0.010528571 0.010473451 0.010522807 
       2005-07-31  2005-08-31  2005-09-30  2005-10-31  2005-11-30  2005-12-31 
      0.010510435 0.010517241 0.010650427 0.010401695 0.010508403 0.010638333 
       2006-01-31  2006-02-28  2006-03-31  2006-04-30  2006-05-31  2006-06-30 
      0.011122314 0.011150000 0.011382114 0.011281452 0.010977600 0.011061905 
       2006-07-31  2006-08-31  2006-09-30  2006-10-31  2006-11-30  2006-12-31 
      0.010861417 0.010902344 0.010870543 0.011115385 0.011119847 0.011122727 
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
      2006-03-31 0.01640556
      2006-04-30 0.01456667
      2006-05-31 0.01288889
      2006-06-30 0.01263333
      2006-07-31 0.01174444
      2006-08-31 0.01218333
      2006-09-30 0.01212222
      2006-10-31 0.01197222
      2006-11-30 0.01182778
      2006-12-31 0.01138333
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
      1996-01-31 -0.0037227273         NA  0.0224530303
      1996-02-29  0.0081772727         NA  0.0226530303
      1996-03-31  0.0043772727         NA  0.0133530303
      1996-04-30 -0.0202227273         NA  0.0324530303
      1996-05-31 -0.0035227273         NA  0.0228530303
      1996-06-30 -0.0150227273         NA -0.0427469697
      1996-07-31 -0.0342227273         NA -0.0461469697
      1996-08-31  0.0283772727 -0.0142432  0.0336530303
      1996-09-30  0.0035772727  0.0860568  0.0528530303
      1996-10-31  0.0176772727  0.0196568  0.0270530303
      1996-11-30  0.0044772727  0.0595568  0.0541530303
      1996-12-31  0.0064772727  0.0156568  0.0089530303
      1997-01-31  0.0100772727  0.0652568  0.0646530303
      1997-02-28 -0.0089227273 -0.0223432 -0.0498469697
      1997-03-31 -0.0017227273 -0.0410432 -0.0460469697
      1997-04-30  0.0014772727 -0.0202432  0.0161530303
      1997-05-31  0.0326772727  0.0397568  0.0634530303
      1997-06-30  0.0119772727  0.0410568 -0.0070469697
      1997-07-31  0.0042772727  0.1008568  0.0956530303
      1997-08-31  0.0125772727 -0.0338432 -0.0152469697
      1997-09-30  0.0107772727  0.0434568  0.0424530303
      1997-10-31 -0.0318227273 -0.0363432 -0.0478469697
      1997-11-30  0.0138772727 -0.0192432  0.0051530303
      1997-12-31 -0.0001227273  0.0050568 -0.0127469697
      1998-01-31 -0.0055227273 -0.0253432  0.0366530303
      1998-02-28  0.0317772727  0.0865568  0.0341530303
      1998-03-31  0.0250772727  0.0483568  0.0083530303
      1998-04-30 -0.0033227273 -0.0151432  0.0109530303
      1998-05-31 -0.0342227273 -0.0248432 -0.0260469697
      1998-06-30  0.0009772727  0.0250568  0.0270530303
      1998-07-31 -0.0326227273 -0.0413432 -0.0119469697
      1998-08-31 -0.1055227273 -0.0141432 -0.0842469697
      1998-09-30  0.0136772727 -0.0187432  0.0540530303
      1998-10-31  0.0446772727  0.0207568 -0.0175469697
      1998-11-30  0.0014772727  0.0557568  0.0430530303
      1998-12-31 -0.0014227273  0.0771568  0.0339530303
      1999-01-31 -0.0204227273  0.0645568  0.0144530303
      1999-02-28 -0.0017227273 -0.0371432 -0.0654469697
      1999-03-31  0.0350772727  0.0940568  0.0062530303
      1999-04-30  0.0398772727  0.0024568  0.0292530303
      1999-05-31  0.0050772727 -0.0143432 -0.0045469697
      1999-06-30  0.0214772727  0.0508568  0.0422530303
      1999-07-31 -0.0013227273  0.0137568 -0.0082469697
      1999-08-31 -0.0276227273  0.0143568 -0.0002469697
      1999-09-30 -0.0156227273  0.0190568 -0.0056469697
      1999-10-31 -0.0117227273  0.0280568  0.0622530303
      1999-11-30 -0.0076227273  0.0555568  0.0949530303
      1999-12-31  0.0035772727  0.1307568  0.0455530303
      2000-01-31 -0.0213227273  0.0236568 -0.0174469697
      2000-02-29  0.0011772727  0.1414568  0.1671530303
      2000-03-31  0.0463772727 -0.0504432 -0.0096469697
      2000-04-30  0.0090772727 -0.0239432 -0.0251469697
      2000-05-31  0.0227772727 -0.0258432 -0.0352469697
      2000-06-30  0.0011772727 -0.0036432  0.0282530303
      2000-07-31 -0.0062227273  0.0046568 -0.0649469697
      2000-08-31  0.0275772727  0.0205568  0.0670530303
      2000-09-30 -0.0098227273 -0.0438432 -0.0697469697
      2000-10-31 -0.0188227273 -0.0429432  0.0037530303
      2000-11-30 -0.0007227273 -0.0361432 -0.0492469697
      2000-12-31 -0.0179227273  0.0048568  0.0044530303
      2001-01-31 -0.0032227273 -0.0466432  0.0137530303
      2001-02-28 -0.0029227273 -0.0334432 -0.0479469697
      2001-03-31 -0.0216227273 -0.0173432 -0.0234469697
      2001-04-30  0.0233772727 -0.0160432  0.0007530303
      2001-05-31  0.0467772727 -0.0066432 -0.0177469697
      2001-06-30 -0.0090227273 -0.0348432 -0.0361469697
      2001-07-31  0.0095772727 -0.0165432 -0.0146469697
      2001-08-31  0.0049772727 -0.0058432 -0.0370469697
      2001-09-30 -0.0423227273  0.0191568 -0.0192469697
      2001-10-31 -0.0100227273 -0.0361432 -0.0329469697
      2001-11-30  0.0228772727 -0.0059432  0.0123530303
      2001-12-31  0.0564772727 -0.0144432 -0.0106469697
      2002-01-31  0.0023772727 -0.0329432 -0.0269469697
      2002-02-28 -0.0235227273 -0.0512432 -0.0538469697
      2002-03-31 -0.0048227273  0.0079568  0.0082530303
      2002-04-30 -0.0065227273 -0.0257432 -0.0377469697
      2002-05-31 -0.0126227273 -0.0436432 -0.0148469697
      2002-06-30 -0.0352227273 -0.0479432 -0.0575469697
      2002-07-31 -0.0866227273 -0.0265432 -0.0464469697
      2002-08-31 -0.0034227273 -0.0145432 -0.0100469697
      2002-09-30 -0.0686227273 -0.0158432 -0.0563469697
      2002-10-31  0.0185772727 -0.0098432 -0.0022469697
      2002-11-30  0.0549772727 -0.0224432  0.0225530303
      2002-12-31 -0.0434227273 -0.0163432 -0.0776469697
      2003-01-31 -0.0523227273 -0.0144432 -0.0139469697
      2003-02-28 -0.0362227273 -0.0290432 -0.0103469697
      2003-03-31  0.0252772727 -0.0197432 -0.0013469697
      2003-04-30  0.0539772727 -0.0260432  0.0400530303
      2003-05-31  0.0225772727  0.0373568  0.0298530303
      2003-06-30  0.0196772727  0.0093568 -0.0015469697
      2003-07-31  0.0064772727  0.0092568 -0.0038469697
      2003-08-31 -0.0108227273 -0.0225432  0.0177530303
      2003-09-30 -0.0021227273 -0.0194432 -0.0149469697
      2003-10-31  0.0369772727 -0.0007432  0.0480530303
      2003-11-30  0.0057772727 -0.0027432 -0.0006469697
      2003-12-31  0.0163772727  0.0147568 -0.0029469697
      2004-01-31 -0.0058227273  0.0049568 -0.0070469697
      2004-02-29 -0.0112227273 -0.0054432  0.0018530303
      2004-03-31 -0.0024227273 -0.0013432 -0.0162469697
      2004-04-30 -0.0154227273 -0.0059432 -0.0324469697
      2004-05-31 -0.0029227273 -0.0066432 -0.0191469697
      2004-06-30  0.0147772727 -0.0003432 -0.0114469697
      2004-07-31 -0.0111227273 -0.0099432 -0.0471469697
      2004-08-31 -0.0056227273 -0.0206432 -0.0180469697
      2004-09-30 -0.0023227273 -0.0041432 -0.0156469697
      2004-10-31 -0.0117227273  0.0005568 -0.0135469697
      2004-11-30  0.0282772727  0.0110568  0.0429530303
      2004-12-31  0.0328772727 -0.0058432  0.0056530303
      2005-01-31 -0.0110227273 -0.0195432 -0.0051469697
      2005-02-28  0.0103772727  0.0068568  0.0292530303
      2005-03-31 -0.0318227273 -0.0210432 -0.0031469697
      2005-04-30 -0.0320227273 -0.0180432 -0.0143469697
      2005-05-31 -0.0068227273 -0.0280432  0.0108530303
      2005-06-30  0.0049772727  0.0052568 -0.0089469697
      2005-07-31 -0.0020227273  0.0057568  0.0078530303
      2005-08-31  0.0001772727 -0.0018432 -0.0144469697
      2005-09-30  0.0149772727  0.0038568  0.0072530303
      2005-10-31 -0.0298227273 -0.0327432 -0.0235469697
      2005-11-30  0.0119772727 -0.0047432  0.0037530303
      2005-12-31  0.0149772727 -0.0056432  0.0056530303
      2006-01-31  0.0580772727  0.0670568  0.0226530303
      2006-02-28  0.0033772727 -0.0467432  0.0141530303
      2006-03-31  0.0285772727  0.0009568 -0.0005469697
      2006-04-30 -0.0122227273  0.0030568 -0.0093469697
      2006-05-31 -0.0378227273 -0.0193432 -0.0336469697
      2006-06-30  0.0104772727 -0.0257432 -0.0313469697
      2006-07-31 -0.0255227273 -0.0272432 -0.0022469697
      2006-08-31  0.0049772727 -0.0254432  0.0128530303
      2006-09-30 -0.0043227273 -0.0372432 -0.0052469697
      2006-10-31  0.0315772727  0.0025568  0.0058530303
      2006-11-30  0.0005772727  0.0064568  0.0144530303
      2006-12-31  0.0003772727 -0.0203432 -0.0014469697
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
      2000-01-01            0.02500000 0.14601749             0.0250000
      2001-01-01            0.15785710 0.19577551             0.0250000
      2002-01-01            0.24431295 0.02500000             0.0250000
      2003-01-01            0.21955470 0.06590151             0.0250000
      2004-01-01            0.09780634 0.02552822             0.1050766
      2005-01-01            0.02500000 0.02500000             0.2445763
                 Emerging Markets Equity Market Neutral Event Driven
      2000-01-01            0.025             0.3500000        0.025
      2001-01-01            0.025             0.3500000        0.025
      2002-01-01            0.025             0.3500000        0.025
      2003-01-01            0.025             0.2817930        0.025
      2004-01-01            0.025             0.3500000        0.025
      2005-01-01            0.025             0.2054237        0.025
                 Fixed Income Arbitrage Global Macro Long/Short Equity
      2000-01-01              0.0250000        0.025             0.025
      2001-01-01              0.0250000        0.025             0.025
      2002-01-01              0.2056871        0.025             0.025
      2003-01-01              0.2577508        0.025             0.025
      2004-01-01              0.2715888        0.025             0.025
      2005-01-01              0.3500000        0.025             0.025
                 Merger Arbitrage Relative Value
      2000-01-01       0.07146246      0.2575201
      2001-01-01       0.12136740      0.0250000
      2002-01-01       0.02500000      0.0250000
      2003-01-01       0.02500000      0.0250000
      2004-01-01       0.02500000      0.0250000
      2005-01-01       0.02500000      0.0250000
      
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
       Min.   :1997-01-31   Min.   :-0.123700     Min.   :-0.056800  
       1st Qu.:2003-02-28   1st Qu.: 0.000200     1st Qu.:-0.011400  
       Median :2009-03-31   Median : 0.006500     Median : 0.002000  
       Mean   :2009-03-31   Mean   : 0.005792     Mean   : 0.004317  
       3rd Qu.:2015-04-30   3rd Qu.: 0.013700     3rd Qu.: 0.019900  
       Max.   :2021-05-31   Max.   : 0.061100     Max.   : 0.069100  
       Distressed Securities Emerging Markets   Equity Market Neutral
       Min.   :-0.106100     Min.   :-0.19220   Min.   :-0.058700    
       1st Qu.:-0.002100     1st Qu.:-0.00920   1st Qu.: 0.000900    
       Median : 0.008800     Median : 0.01000   Median : 0.004700    
       Mean   : 0.006825     Mean   : 0.00673   Mean   : 0.004335    
       3rd Qu.: 0.017900     3rd Qu.: 0.02570   3rd Qu.: 0.008300    
       Max.   : 0.050400     Max.   : 0.12300   Max.   : 0.025300    
        Event Driven       Fixed Income Arbitrage  Global Macro      
       Min.   :-0.126900   Min.   :-0.08670       Min.   :-0.031300  
       1st Qu.:-0.001200   1st Qu.: 0.00180       1st Qu.:-0.003900  
       Median : 0.008800   Median : 0.00550       Median : 0.004700  
       Mean   : 0.006674   Mean   : 0.00443       Mean   : 0.005598  
       3rd Qu.: 0.016800   3rd Qu.: 0.00930       3rd Qu.: 0.012800  
       Max.   : 0.066600   Max.   : 0.03650       Max.   : 0.073800  
       Long/Short Equity   Merger Arbitrage    Relative Value      Short Selling     
       Min.   :-0.081300   Min.   :-0.079000   Min.   :-0.069200   Min.   :-0.13400  
       1st Qu.:-0.004700   1st Qu.: 0.000700   1st Qu.: 0.001100   1st Qu.:-0.02510  
       Median : 0.008200   Median : 0.005900   Median : 0.006700   Median :-0.00320  
       Mean   : 0.006717   Mean   : 0.005582   Mean   : 0.005728   Mean   :-0.00126  
       3rd Qu.: 0.019500   3rd Qu.: 0.011100   3rd Qu.: 0.013000   3rd Qu.: 0.01810  
       Max.   : 0.074500   Max.   : 0.047200   Max.   : 0.039200   Max.   : 0.24630  
       Funds of Funds     
       Min.   :-0.070500  
       1st Qu.:-0.003300  
       Median : 0.005200  
       Mean   : 0.004512  
       3rd Qu.: 0.012700  
       Max.   : 0.066600  
      
      > tail(cumprod(1 + edhec), 1)
                 Convertible Arbitrage CTA Global Distressed Securities
      2021-05-31              5.208815   3.278012              6.989556
                 Emerging Markets Equity Market Neutral Event Driven
      2021-05-31         6.088353              3.517302     6.654019
                 Fixed Income Arbitrage Global Macro Long/Short Equity
      2021-05-31               3.580675     4.977817          6.673183
                 Merger Arbitrage Relative Value Short Selling Funds of Funds
      2021-05-31         5.011198       5.222248     0.5130537       3.601022
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
       [1]  0.00000000 -0.00770000  0.00000000 -0.01326634  0.00000000 -0.07069135
       [7]  0.00000000 -0.03877182  0.00000000 -0.03737506  0.00000000 -0.01627767
      [13]  0.00000000 -0.00150000  0.00000000 -0.02601701  0.00000000 -0.00040000
      [19]  0.00000000 -0.01742180  0.00000000 -0.01843796  0.00000000 -0.01490000
      [25]  0.00000000 -0.01655473  0.00000000 -0.02220000  0.00000000 -0.20591447
      [31]  0.00000000 -0.00400000  0.00000000 -0.00739256  0.00000000 -0.07837768
      [37]  0.00000000 -0.00070000  0.00000000 -0.05932734  0.00000000 -0.08276940
      [43]  0.00000000 -0.00190000  0.00000000 -0.00950000  0.00000000
      
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
       [1] -0.20591447 -0.08276940 -0.07837768 -0.07069135 -0.05932734 -0.03877182
       [7] -0.03737506 -0.02601701 -0.02220000 -0.01843796 -0.01742180 -0.01655473
      [13] -0.01627767 -0.01490000 -0.01326634 -0.00950000 -0.00770000 -0.00739256
      [19] -0.00400000 -0.00190000 -0.00150000 -0.00070000 -0.00040000  0.00000000
      [25]  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
      [31]  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
      [37]  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
      [43]  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
      
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
       Min.   :1996-01-31   Min.   :-0.094400   Min.   :-0.03710  
       1st Qu.:1998-10-23   1st Qu.:-0.000025   1st Qu.:-0.00980  
       Median :2001-07-15   Median : 0.011150   Median : 0.00820  
       Mean   :2001-07-15   Mean   : 0.011123   Mean   : 0.01414  
       3rd Qu.:2004-04-07   3rd Qu.: 0.024850   3rd Qu.: 0.02520  
       Max.   :2006-12-31   Max.   : 0.069200   Max.   : 0.15560  
                                                NA's   :7         
            HAM3                HAM4               HAM5                HAM6          
       Min.   :-0.071800   Min.   :-0.17590   Min.   :-0.132000   Min.   :-0.040400  
       1st Qu.:-0.005375   1st Qu.:-0.01985   1st Qu.:-0.016400   1st Qu.:-0.001575  
       Median : 0.010200   Median : 0.01375   Median : 0.003800   Median : 0.012850  
       Mean   : 0.012447   Mean   : 0.01102   Mean   : 0.004088   Mean   : 0.011055  
       3rd Qu.: 0.031375   3rd Qu.: 0.04600   3rd Qu.: 0.030900   3rd Qu.: 0.025475  
       Max.   : 0.179600   Max.   : 0.15080   Max.   : 0.174700   Max.   : 0.058300  
                                              NA's   :55          NA's   :68         
        EDHEC LS EQ           SP500 TR           US 10Y TR            US 3m TR       
       Min.   :-0.055200   Min.   :-0.144600   Min.   :-0.070920   Min.   :0.000660  
       1st Qu.:-0.003175   1st Qu.:-0.017327   1st Qu.:-0.008515   1st Qu.:0.001570  
       Median : 0.011000   Median : 0.010950   Median : 0.004425   Median :0.003845  
       Mean   : 0.009545   Mean   : 0.008665   Mean   : 0.004385   Mean   :0.003226  
       3rd Qu.: 0.021450   3rd Qu.: 0.038025   3rd Qu.: 0.016660   3rd Qu.:0.004395  
       Max.   : 0.074500   Max.   : 0.097800   Max.   : 0.050550   Max.   :0.006580  
       NA's   :12                                                                    
      
      > tail(cumprod(1 + managers), 1)
                     HAM1 HAM2     HAM3    HAM4 HAM5 HAM6 EDHEC LS EQ SP500 TR
      2006-12-31 4.126671   NA 4.706732 3.52944   NA   NA          NA 2.761619
                 US 10Y TR US 3m TR
      2006-12-31  1.734037 1.529681
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
      Geometric Mean    0.004382331
      
      > mean.stderr(edhec[, "Funds of Funds"])
                     Funds of Funds
      Standard Error   0.0009396873
      
      > mean.UCL(edhec[, "Funds of Funds"])
                             Funds of Funds
      Upper Confidence Level    0.006361023
      
      > mean.LCL(edhec[, "Funds of Funds"])
                             Funds of Funds
      Lower Confidence Level    0.002662185
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
      2001-12-30                     1.218106             1.249887
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
       [1]  0.00000000 -0.00770000  0.00000000 -0.01326634  0.00000000 -0.07069135
       [7]  0.00000000 -0.03877182  0.00000000 -0.03737506  0.00000000 -0.01627767
      [13]  0.00000000 -0.00150000  0.00000000 -0.02601701  0.00000000 -0.00040000
      [19]  0.00000000 -0.01742180  0.00000000 -0.01843796  0.00000000 -0.01490000
      [25]  0.00000000 -0.01655473  0.00000000 -0.02220000  0.00000000 -0.20591447
      [31]  0.00000000 -0.00400000  0.00000000 -0.00739256  0.00000000 -0.07837768
      [37]  0.00000000 -0.00070000  0.00000000 -0.05932734  0.00000000 -0.08276940
      [43]  0.00000000 -0.00190000  0.00000000 -0.00950000  0.00000000
      
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
       [1] -0.20591447 -0.08276940 -0.07837768 -0.07069135 -0.05932734 -0.03877182
       [7] -0.03737506 -0.02601701 -0.02220000 -0.01843796 -0.01742180 -0.01655473
      [13] -0.01627767 -0.01490000 -0.01326634 -0.00950000 -0.00770000 -0.00739256
      [19] -0.00400000 -0.00190000 -0.00150000 -0.00070000 -0.00040000  0.00000000
      [25]  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
      [31]  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
      [37]  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
      [43]  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000
      
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
      Modified VaR                          -0.02568389 -0.03204110
      Modified Expected Shortfall           -0.08941788 -0.04038067
                                  Distressed Securities Emerging Markets
      Modified VaR                          -0.02800272      -0.05343318
      Modified Expected Shortfall           -0.06732969      -0.11570887
                                  Equity Market Neutral Event Driven
      Modified VaR                          -0.01098870  -0.02960873
      Modified Expected Shortfall           -0.03670501  -0.08113553
                                  Fixed Income Arbitrage Global Macro
      Modified VaR                           -0.01773794  -0.01380785
      Modified Expected Shortfall            -0.05308245  -0.01696439
                                  Long/Short Equity Merger Arbitrage Relative Value
      Modified VaR                      -0.02950798      -0.01502873    -0.01736871
      Modified Expected Shortfall       -0.04857352      -0.05130164    -0.04751282
                                  Short Selling Funds of Funds
      Modified VaR                  -0.06215004    -0.02309324
      Modified Expected Shortfall   -0.06754083    -0.04590385
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
      HAM1 to EDHEC LS EQ   0.5896798 1.378595e-12 0.45894685 0.6954188
      HAM1 to SP500 TR      0.6600671 7.397842e-18 0.55138376 0.7467191
      HAM2 to EDHEC LS EQ   0.7015847 4.468363e-19 0.59747795 0.7824328
      HAM2 to SP500 TR      0.4128282 1.715350e-06 0.25576240 0.5486602
      HAM3 to EDHEC LS EQ   0.8053564 1.443898e-28 0.73174468 0.8603967
      HAM3 to SP500 TR      0.6608633 6.545409e-18 0.55236590 0.7473433
      HAM4 to EDHEC LS EQ   0.6148700 7.970417e-14 0.48958597 0.7152802
      HAM4 to SP500 TR      0.5601846 2.870109e-12 0.43052170 0.6671932
      HAM5 to EDHEC LS EQ   0.4462382 4.749637e-05 0.24694054 0.6093171
      HAM5 to SP500 TR      0.2844487 1.216830e-02 0.06458459 0.4779755
      HAM6 to EDHEC LS EQ   0.7285463 8.879813e-12 0.58804644 0.8263670
      HAM6 to SP500 TR      0.5091542 1.735968e-05 0.30101889 0.6709863
      
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
      1                                 0.5529010                      0.4470990
      2                                 0.6288660                      0.3711340
      3                                 0.6354167                      0.3645833
      4                                 0.6491228                      0.3508772
      5                                 0.6276596                      0.3723404
      6                                 0.6485507                      0.3514493
      7                                 0.6162791                      0.3837209
      
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
      2002-12-31 -0.004045348 -0.012800051 -0.060038137 -0.02103051 -0.01650314
      2003-12-31  0.012493183  0.011940516  0.079079298  0.22039789  0.03175587
      2004-12-31  0.007188399  0.012866800  0.005786203  0.05651752  0.01053252
      2005-12-31  0.004033565  0.006166386  0.044503812  0.04027608  0.01033577
      2006-12-31  0.009985784  0.004586611  0.042558683  0.05289248  0.02315896
                 Portfolio Return
      2002-12-31      -0.11441719
      2003-12-31       0.35566675
      2004-12-31       0.09289143
      2005-12-31       0.10531560
      2006-12-31       0.13318252
      
      > to.yearly.contributions(res_qtr_rebal$contribution)
                         HAM1         HAM2         HAM3        HAM4        HAM5
      2002-12-31 -0.004045348 -0.012800051 -0.060038137 -0.02103051 -0.01650314
      2003-12-31  0.012493183  0.011940516  0.079079298  0.22039789  0.03175587
      2004-12-31  0.007188399  0.012866800  0.005786203  0.05651752  0.01053252
      2005-12-31  0.004033565  0.006166386  0.044503812  0.04027608  0.01033577
      2006-12-31  0.009985784  0.004586611  0.042558683  0.05289248  0.02315896
                 Portfolio Return
      2002-12-31      -0.11441719
      2003-12-31       0.35566675
      2004-12-31       0.09289143
      2005-12-31       0.10531560
      2006-12-31       0.13318252
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
      2000-01-01            0.02500000 0.14601749             0.0250000
      2001-01-01            0.15785710 0.19577551             0.0250000
      2002-01-01            0.24431295 0.02500000             0.0250000
      2003-01-01            0.21955470 0.06590151             0.0250000
      2004-01-01            0.09780634 0.02552822             0.1050766
      2005-01-01            0.02500000 0.02500000             0.2445763
                 Emerging Markets Equity Market Neutral Event Driven
      2000-01-01            0.025             0.3500000        0.025
      2001-01-01            0.025             0.3500000        0.025
      2002-01-01            0.025             0.3500000        0.025
      2003-01-01            0.025             0.2817930        0.025
      2004-01-01            0.025             0.3500000        0.025
      2005-01-01            0.025             0.2054237        0.025
                 Fixed Income Arbitrage Global Macro Long/Short Equity
      2000-01-01              0.0250000        0.025             0.025
      2001-01-01              0.0250000        0.025             0.025
      2002-01-01              0.2056871        0.025             0.025
      2003-01-01              0.2577508        0.025             0.025
      2004-01-01              0.2715888        0.025             0.025
      2005-01-01              0.3500000        0.025             0.025
                 Merger Arbitrage Relative Value
      2000-01-01       0.07146246      0.2575201
      2001-01-01       0.12136740      0.0250000
      2002-01-01       0.02500000      0.0250000
      2003-01-01       0.02500000      0.0250000
      2004-01-01       0.02500000      0.0250000
      2005-01-01       0.02500000      0.0250000
    Code
      dev.off()
    Output
      pdf 
        2 

