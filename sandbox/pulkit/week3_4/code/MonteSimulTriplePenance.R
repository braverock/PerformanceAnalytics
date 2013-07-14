#' @title
#' Monte Carlo Simulation for the Triple Penance Rule
#'
#' @param R Hedge Fund log Returns
#' 
#' @reference Bailey, David H. and Lopez de Prado, Marcos, Drawdown-Based Stop-Outs and the ‘Triple Penance’ Rule(January 1, 2013).


 
monte_simul<-function(size){
    
  phi = 0.5
  mu = 1
  sigma = 2 
  dp0 = 1
  bets = 25
  confidence = 0.95
  
  q_value = getQ(bets, phi, mu, sigma, dp0, confidence)
  ms = NULL
  
  for(i in 1:size){
    ms[i] = sum((1-phi)*mu + rnorm(bets)*sigma + delta*phi)
  }
  q_ms = quantile(ms,(1-confidence)*100)
  diff = q_value - q_ms 

  print(q_value)
  print(q_ms)
  print(q_value - q_ms)
}



