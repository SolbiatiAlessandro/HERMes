source("HTA_signal.R")
source("proceed.R")
source("interest.R")

trade <- function(name, l=500, tit="BACKTEST", plot=TRUE,capital=1000,cap_med_fact=0.7 ,n_mean=100){
	print("starting to trade")
	library(tseries)
	print("getting prices")
	stock <- get.hist.quote(name)
	print("calc HTA_signals")
	stock_hta <- HTA_signal(stock, l)
	print("calc variations in prices from signal to signal")
	stock_r <- proceed(stock,stock_hta)
	print("calc interest with capital risk modulation")
	stock_p <- interest(stock,stock_hta,stock_r,3,end_delta=150,cap=capital,cap_med_fact=cap_med_fact, n_mean=n_mean, plot=plot)
	print("done")
	stock_p}