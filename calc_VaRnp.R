
#
#	Alessandro Solbiati - EWMA_RiskMetrics GITHUB project
#	reference: Quantitative Finance for R (Bee, Santi 2013)
#	reference: RiskMetrics(TM) Technical Document (JPMorgan and Retuters 1996) 
#
#	--------------------------------------------------------------------------
#
# 	compute Non Parametric Value at Risk (Var), if VaR==FALSE compute Non Parametric Expected Shortfall
#

calc_VaRnp<- function(Serie,conf,VaR,s_startdate,s_enddate,debug=0)
{
  
  require(timeSeries)

  cc<- -(Serie) 
  if(debug) cat("\n--> in calc_VaRnp: cc = ", cc)
  print(head(cc))
  print(tail(cc))
  cc<- window(cc,start=s_startdate,end=s_enddate)
  if(debug) cat("\n--> in calc_VaRnp: cc = ", cc)
  cc<- sort(as.vector(cc))
  if(debug) cat("\n--> in calc_VaRnp: cc = ", cc)
  out<- cc[ceiling(conf*length(cc))] #NP VaR
  if (!VaR) { out<- mean(cc[cc>out]) }  #NP ES
  if(debug) cat("\n--> in calc_VaRnp: out =", out, "\n")
  out
}
