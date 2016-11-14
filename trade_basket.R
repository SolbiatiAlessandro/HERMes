source("trade.R")

trade_basket <- function(titoli,l,single_plot=FALSE,fact=0.8){
	
	basket <- c(trade(titoli[1],l,plot=FALSE))
	earnings <- c()
	count_earnings <- 1
	plot(basket,xlab="time",ylab="variations",main="basket backtesting",type="l",ylim=c(400,1600),xlim=c(1,30))
	for(i in 2:length(titoli)){
		new_tit <- trade(titoli[i],l,plot=FALSE,cap_med_fact=fact)
		new_tit <- new_tit[!is.na(new_tit)]
		lines(new_tit)
		basket[i] <- new_tit
		earnings[count_earnings] <- new_tit[length(new_tit)]
		count_earnings <- count_earnings+1
		
	}
	
	earnings
}