source("hma_sign.R")
source("tether.R")

HTA_signal <- function(data_OHLC, length_sample, tt_t=25, hma_n=20, adx_n=14){
	
	library(tseries)
	library(zoo)
	library(TTR)
	
	
	end <- length(data_OHLC[,4])
	start <- end-(length_sample)
	
	data_hma <- HMA(data_OHLC[,4],n=hma_n)
	data_hma_sign <- hma_sign(data_hma)
	data_adx <- ADX(cbind(data_OHLC[,2],data_OHLC[,3],data_OHLC[,4]),n=adx_n)
	data_tt <- tether(cbind(data_OHLC[,2],data_OHLC[,3]),t=tt_t)
	
	data <- cbind(data_OHLC[,4],data_hma,data_hma_sign,data_adx[,4],data_tt)
	
	up_signals <- c()
	up_c <- 1
	sell_signals <- c()
	sell_c <- 1
	down_signals <- c()
	down_c <- 1
	buy_signals <- c()
	buy_c <- 1
	
	
	for(t in start:end){
		if(data[(t-1),5]>data[(t-1),1] && data[t,5]<data[t,1] && data[t,3]==1 && data[t,4]>20){
			#up_signal
			up_signals[up_c] <- t
			up_c <- up_c+1
			flag_up=1
			for(tt in t:end){
				#print(cbind(tt,data[(tt-1),5],data[(tt-1),1]))  CHECK
				if(data[(tt-1),5]<data[(tt-1),1] && data[tt,5]>data[tt,1] && flag_up==1){
					#quit
					sell_signals[sell_c] <- tt
					sell_c <- sell_c+1
					flag_up <- 0
				}
			}
		}
		if(data[(t-1),5]<data[(t-1),1] && data[t,5]>data[t,1] && data[t,3]==0 && data[t,4]>20){
			#down_signal
			down_signals[down_c] <- t
			down_c <- down_c+1
			flag_down=1
			for(tt in t:end){
				if(data[(tt-1),5]>data[(tt-1),1] && data[tt,5]<data[tt,1] && flag_down==1){
					#quit
					buy_signals[buy_c] <- tt
					buy_c <- buy_c+1
					flag_down <- 0
				}
			}
		}
	}	
	
	cbind(up_signals, sell_signals, down_signals, buy_signals)
}

