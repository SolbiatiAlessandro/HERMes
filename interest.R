source("date_sign.R")
source("study.R")

count_date <- function(sst, date = "0000-00-00"){
	l <- length(time(sst))
	i <- l
	num <- 0
	while(i>0){
		if(as.Date(time(sst)[i])==date) num <- i
		i<-i-1
	}	
	if(num==0) print("count_date function says: out of bound")	
	else num
}
var_to_cap <- function(VaR,cap_max,cap_med,var_med=0.2){
 	cap <- cap_max - ((cap_max-cap_med)/(var_med))*VaR
	cap}
clean_date <- function(date_up,date_down,debug=0){
	if(debug) cat("starting clean_date function\n")
	date <- c(date_up,date_down)
	date <- sort(date)
	date <- c(date,as.Date("2020-01-01"):as.Date("2021-01-01"))
	if(debug) cat("eliminate duplicates\n")
	l <- length(date_up)+length(date_down)
	for(i in 1:l){
		if(debug) cat("in the loop ", i, "on ", l, ":date array ", date,"\n")
		if(date[i+1]==date[i]){
			for(j in i:(2*l)){
				date[j] <- date[j+1]
			}
			l <- l-1
		}
		if(debug) cat("end loop\n")
	}
	date <- date[1:l]
	date}
interest <- function(data_ohlc, hta_signals, perc,mode=1,yinf=0.5,ysup=1.5,title="Backtest",plot=TRUE, end_delta=130, cap=1000, cap_med_fact=0.7, n_mean, debug=0){
	
	## STUDY THE CAPITAL EVOLUTION
	## mode 1 -> cumulative sum
	## mode 2 -> exponential growth
	## mode 3 -> EWMA capitaliazion

	if(mode==1){
		li <- sum(perc)
		if(plot==TRUE){
			plot(cumsum(perc),type='l',ylab="variation",xlab="time",main=title)
			}	
		return(li)
		}	
	if(mode==2){
		capt <- c(cap)
		for(i in 1:length(perc)){capt[i+1] <- capt[i]*(1+perc[i])}
		if(plot==TRUE){
			plot(x=c(1:length(capt)),y=capt, type='l', ylim=c(yinf*cap,ysup*cap), ylab="capital", xlab="time",main=title)
			}
		return(capt)
		}
		
	if(mode==3){       # <- EWMA_RiskMetrics()®
		
		sst_closure <- data_ohlc[,4]
		capt <- c(cap)
		if(debug) cat("getting cleaned date signals\n")
		date_signals <- clean_date(date_sign(sst_closure,hta_signals,1),date_sign(sst_closure,hta_signals,3),debug=debug)
		
		for(i in 1:length(perc)){
			if(debug){ 
				cat("\n\nstarting calc new position for\n")
				print(perc[i])
				print(date_signals[i])
			}
			day_numeric <- count_date(sst_closure, date_signals[i])
			if(debug) print(day_numeric)
			
			## EWMA part (study function)
			print(time(sst_closure))
			if(debug) cat("getting VaR serie\nend date:",as.Date(time(sst_closure)[day_numeric-end_delta]),"\n")

			sst_study <- study(serie=as.timeSeries(sst_closure),start="1900-01-01", end= time(sst_closure)[day_numeric-end_delta],  N_day = day_numeric, debug=debug)
			var <- sst_study[length(sst_study[,1]),1]
			if(debug){ 
				cat("calc var =",var,"\n")
				print(sst_study)
				cat("calc mean_var\n")
			}
			
			
			mean_with_na <- sst_study[(length(sst_study[,1])-n_mean):length(sst_study[,1]),1]
			cat("\njj\n")
			var_mean <- mean(mean_with_na[!is.na(mean_with_na)])

			
			if(debug) cat("mean_var=",var_mean,"\n\ncapital accounting")
			
			
			cap_invest <- as.numeric(var_to_cap(VaR=var,cap_max=capt[i],cap_med=cap_med_fact*capt[i],var_med=var_mean))
			cap_left <- capt[i]-cap_invest
			capt[i+1] <- cap_left+cap_invest*(1+perc[i])
			
			
			
			print(cap_invest)
			print(cap_left)
			print(capt[i+1])
			print("---------")
			
			}
			
		if(plot==TRUE){
			plot(x=c(1:length(capt)),y=capt, type='l', ylim=c(yinf*cap,ysup*cap), ylab="capital", xlab="time",main=title)
			}
		return(capt)
	
	}}