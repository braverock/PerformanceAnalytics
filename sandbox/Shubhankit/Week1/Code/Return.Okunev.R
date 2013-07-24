quad <- function(R,d)
{
  coeff = as.numeric(acf(as.numeric(edhec[,1]), plot = FALSE)[1:2][[1]])
b=-(1+coeff[2]-2*d*coeff[1])
c=(coeff[1]-d)
  ans= (-b-sqrt(b*b-4*c*c))/(2*c)
  a <- a[!is.na(a)]
  return(c(ans))               
}
Return.Okunev<-function(R,q=3)
{
  column.okunev=R
  column.okunev <- column.okunev[!is.na(column.okunev)]
  for(i in 1:q)
  {
    lagR = lag(column.okunev, k=i)
    column.okunev= (column.okunev-(lagR*quad(lagR,0)))/(1-quad(lagR,0))
  }
  return(c(column.okunev))
}
