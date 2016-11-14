#
#	Alessandro Solbiati - EWMA_RiskMetrics GITHUB project - 26/06/2016 
#	reference: Quantitative Finance for R (Bee, Santi 2013)
#	reference: RiskMetrics(TM) Technical Document (JPMorgan and Retuters 1996) 
#
#	--------------------------------------------------------------------------
#
#	study() : study a timeSerie defining the best method/confidence couple and return the last 10 VaR and ES
# 

source("EWMA_RiskMetrics.R")
source("max_pv.R")

study <- function(serie,start,end,debug=0,N_day=length(serie)){
	
	end<-as.Date(end)
	if(debug) cat("starting study function\n")
	vect <- max_pv(serie,start,end,debug=debug)
	usage <- vect[2]
	col <- vect[3]
	
	if(col==1){conf <- 0.9}
	if(col==2){conf <- 0.95}
	if(col==3){conf <- 0.99}
	if(col==4){conf <- 0.995}
	
	ewma_VaR <- EWMA_RiskMetrics(serie, conf, usage, start, end, VaR=TRUE,debug=debug,Nt=N_day)
	ewma_ES <- EWMA_RiskMetrics(serie, conf, usage, start, end, VaR=FALSE,debug=debug,Nt=N_day)
	l=length(ewma_VaR[,1])
	
	out <- cbind("Value at Risk"=ewma_VaR[,4],"Expected Shortfall"=ewma_ES[,4],"p-value"=ewma_VaR[,5])
	out <- cbind(out,"confidence"=conf)
	out
	
}