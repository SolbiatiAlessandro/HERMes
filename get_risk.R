#given the ohlc and the date return a five element vector with VaR, EF, pvalue, confidence, and meanVaR
source("study.R")
source("interest.R")
get_risk <-function(data_ohlc,date,end_delta=100,debug=0,n_mean=100,capt=1000,cap_med_fact=0.7){
	
	sst_closure <- data_ohlc[,4]
	#date format "2000-01-01"
	day_numeric <- count_date(sst_closure, date)	
	#replacement has length zero -> start is too back in time
	sst_study <- study(serie=as.timeSeries(sst_closure),start="1900-01-01", end= time(sst_closure)[day_numeric-end_delta],  N_day = day_numeric, debug=debug)
	var <- sst_study[length(sst_study[,1]),1]
			
			
			
	mean_with_na <- sst_study[(length(sst_study[,1])-n_mean):length(sst_study[,1]),1]
	var_mean <- mean(mean_with_na[!is.na(mean_with_na)])

	if(debug) cat("mean_var=",var_mean)

	out<-cbind(sst_study,"mean var"=var_mean)[length(sst_study[,1]),]
	out
			

}

