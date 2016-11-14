#
#	Alessandro Solbiati - EWMA_RiskMetrics GITHUB project - 26/06/2016 
#	reference: Quantitative Finance for R (Bee, Santi 2013)
#	reference: RiskMetrics(TM) Technical Document (JPMorgan and Retuters 1996) 
#
#	--------------------------------------------------------------------------
#
#	getprice() : return a timeSerie obj with the historical data from YahooFinance
#

getprice <- function(name_code){
	
	require(tseries)
	require(timeSeries)
		
	
	pp <- get.hist.quote(instrument=name_code,quote="Close")
	pp <- as.timeSeries(pp)
	pp
	

}

#return the laste l days of the title

getprice_last <- function(name_code, l){
	end <- Sys.Date()
	start <- end-l
	pp <- get.hist.quote(instrument=name_code,quote="Close",start=start)
	pp
}

getOHLC_last <- function(name_code, l){
	end <- Sys.Date()
	start <- end-l
	cat("\n\nok1",name_code,l)
	pp <- get.hist.quote(instrument=name_code,start=start)
	cat("\n\nok2")
	pp
}