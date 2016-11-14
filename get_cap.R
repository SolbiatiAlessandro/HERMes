source("get_risk.R")
#takes as input the outcome of get_risk() and return the capital suggested for the position
get_cap <- function(get_risk_outcome,debug=0,capt=1000,cap_med_fact=0.7){

	var_mean <- get_risk_outcome[5]
	var <- get_risk_outcome[1]

	var_to_cap_nested <- function(VaR,cap_max,cap_med,var_med=0.2){
 		cap <- cap_max - ((cap_max-cap_med)/(var_med))*VaR
		cap
	}

	cap_invest <- as.numeric(var_to_cap_nested(VaR=var,cap_max=capt,cap_med=cap_med_fact*capt,var_med=var_mean))
}