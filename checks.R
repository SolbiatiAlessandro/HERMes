source("hma_sign.R")
source("tether.R")
source("portafoglio.R")

daily_check_part <- function(titoli, updateProgress=NULL){
	
	signals <- c() #check_signal()
	for(i in 1:length(titoli)){
		
		#for the UI progress bar
		if(is.function(updateProgress)){
			text <- paste0("\ncurrently downloading and analysing ",titoli[i],"..")
			updateProgress(detail=text)
		}

		cat("\n\nesamino il num", i, "\n");
		signals[i] <- check_signal(titoli[i])
		if(signals[i]!=0) print("*********************** SIGNAL ************************")
		else print(signals[i])
				
	}
	signals	
}
check_signal <- function(name){
	
	#HMA -> 20p
	#tether -> 25p
	#ADX -> 14p
	
	
	library(tseries)
	library(zoo)
	library(TTR)
	
	
	#print("check")
	
	data_OHLC <- get.hist.quote(name, start="2016-05-01")
	end <- length(data_OHLC[,4])
	
	#print("check")

	data_hma <- HMA(data_OHLC[(end-24):end,4],n=20)
	
	
	#print("check")
	
	data_hma_sign <- hma_sign(data_hma)
	
	
	#print("check")
	
	data_adx <- ADX(cbind(data_OHLC[(end-50):end,2],data_OHLC[(end-50):end,3],data_OHLC[(end-50):end,4]),n=14)
	
	
	#print("check")
	
	data_tt <- tether(cbind(data_OHLC[(end-51):end,2],data_OHLC[(end-51):end,3]),t=25)
	
	
	#print("check j")
	cat("\n tether line ieri\t")
	print(data_tt[(length(data_tt)-1)])

	cat("\n prezzo ieri\t")
	print(data_OHLC[(end-1),4])

	cat("\n tether line oggi\t")
	print(data_tt[length(data_tt)])

	cat("\n prezzo oggi\t")
	print(data_OHLC[end,4])

	cat("\n adx\t")
	print(as.numeric(data_adx[length(data_adx[,4]),4]))

	cat("\n hma_signal\t")
	print(data_hma_sign[length(data_hma_sign)])
	
	if(data_tt[(length(data_tt)-1)]>data_OHLC[(end-1),4] && data_tt[length(data_tt)]<data_OHLC[end,4] && data_adx[length(data_adx[,4]),4]>20 && data_hma_sign[length(data_hma_sign)]==1){		segnale <- 0.5		}
	
	else if(data_tt[(length(data_tt)-1)]<data_OHLC[(end-1),4] && data_tt[length(data_tt)]>data_OHLC[end,4] && data_adx[length(data_adx[,4]),4]>20 && data_hma_sign[length(data_hma_sign)]==0){		segnale <- -0.5		}
	
	else segnale <- 0

	cat("\n [segnale]\t")
	print(segnale)
	
	return(segnale)	
}
price_break_up <- function(name){
	
	#HMA -> 20p
	#tether -> 25p
	#ADX -> 14p
	
	
	library(tseries)
	library(zoo)
	library(TTR)
	
	
	#print("check")
	
	data_OHLC <- get.hist.quote(name, start="2016-05-01")
	end <- length(data_OHLC[,4])	
	#print("check")
	
	data_tt <- tether(cbind(data_OHLC[(end-51):end,2],data_OHLC[(end-51):end,3]),t=25)
	
	#print("check j")
	#print(data_tt[(length(data_tt)-1)])
	#print(data_OHLC[(end-1),4])
	#print(data_tt[length(data_tt)])
	#print(data_OHLC[end,4])
	
	if(data_tt[length(data_tt)]<data_OHLC[end,4]){
		
		segnale <- 1
		
	}	
	
	else segnale <- 0

	cbind(segnale, data_tt[length(data_tt)-1],data_tt[length(data_tt)], data_OHLC[end,4])
}
price_break_down <- function(name){
	
		
	#HMA -> 20p
	#tether -> 25p
	#ADX -> 14p
	
	
	library(tseries)
	library(zoo)
	library(TTR)
	
	
	#print("check")
	
	data_OHLC <- get.hist.quote(name, start="2016-05-01")
	end <- length(data_OHLC[,4])	
	#print("check")
	
	data_tt <- tether(cbind(data_OHLC[(end-51):end,2],data_OHLC[(end-51):end,3]),t=25)
	
	print("check j")
	print(data_tt[(length(data_tt)-1)])
	print(data_OHLC[(end-1),4])
	print(data_tt[length(data_tt)])
	print(data_OHLC[end,4])
	print(data_OHLC[end,1])
	
	if(data_tt[length(data_tt)]>data_OHLC[end,4]){
		
		segnale <- 1
		
	}	
	
	else segnale <- 0

	
	cbind(segnale, data_tt[length(data_tt)-1],data_tt[length(data_tt)], data_OHLC[end,4])	
}
daily_check_complete <- function(titoli, ups = c(), downs = c(), updateProgress=NULL){
	
	signs <- daily_check_part(titoli,updateProgress)
	n_titoli <-length(signs)
	print(signs)
	for(i in 1:n_titoli){

		if(signs[i]==0.5) cat("se domani apre sopra thether ENTRA LONG su", titoli[i])
		else if(signs[i]==-0.5) cat("se domani apre sotto theter ENTRA SHORT su", titoli[i])
		else cat("su",titoli[i],"oggi non c'é segnale\n")

	}
	
	if(length(ups)){
		for(i in 1:length(ups)){
			n_titoli <- n_titoli+1
			if(price_break_down(ups[i])==1){ 
				cat("\n\ntether line tagliata in basso:", ups[i])
				signs[n_titoli] <- -1;
			}

			else{ 
				cat("\n\nposizione resta invariata:", ups[i],"\n\n")
				signs[n_titoli] <- 0;
			}
		}
	}
	
	if(length(downs)){
		for(i in 1:length(downs)){
			n_titoli <- n_titoli+1
			if(price_break_up(downs[i])==1){
				cat("\n\ntether line tagliata in alto:", downs[i])
				signs[n_titoli] = +1;
			}
			else{ 
				cat("\n\nposizione resta invariata: ", downs[i],"\n\n")
				signs[n_titoli] = 0
			}

		}
	}
	signs
}

no_internet_test <- function(titoli,ups = c(),downs = c(), updateProgress=updateProgress){
	vect <- c(0,0,0,0,0,0,0,0.5,0,0)
	for (i in 1:10) {
		if(is.function(updateProgress)){
			Sys.sleep(0.2)
			text <- paste0("\ncurrently downloading and analysing niente..")
			updateProgress(detail=text)
		}
	
	}
	vect
}