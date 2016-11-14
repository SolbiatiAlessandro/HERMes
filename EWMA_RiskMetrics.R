#
#	Alessandro Solbiati - EWMA_RiskMetrics GITHUB project - 26/06/2016 
#	reference: Quantitative Finance for R (Bee, Santi 2013)
#	reference: RiskMetrics(TM) Technical Document (JPMorgan and Reuters 1996) 
#
#	--------------------------------------------------------------------------
#
#	EWMA_RiskMetrics main function: computes and tests all the processes of the EWMA Volatility and VaR estimations
#
#	functions required: (in the same folder)
#
#	calc_Volatility()			[calc_Volatility.R]
#	calc_VaRnp()				  [calc_VaRnp.R     ]
#	calc_VaRnorm()				[calc_VaRnorm.R   ]
#	calc_VaRt()					  [calc_VaRt.R      ]
#	exe_backtesting()			[exe_backtesting.R]
#
#     The function return an matrix 'EWMA_result'
#     To obtain the p-value of the binomial test on the effectiviness of the computed Value at Risk use "as.numeric(EWMA_RiskMetrics(...)%p.value[1])"
#     For further informations see the documentations on the project page

source("calc_Volatility.R")
source("calc_VaRnp.R")
source("calc_VaRnorm.R")
source("calc_VaRt.R")
source("exe_backtesting.R")

EWMA_RiskMetrics<- function(Serie,conf,usage,s_startdate,s_enddate,VaR,debug=0,Nt=length(Serie))
{
  
  if(debug) cat("\n\t->Starting EWMA_RiskMetrics\n")
  require(timeSeries)
  #end <- as.Date(end)
  # Settings: calculate returns of the prices and put it in a vector SSt
  SSt<- cbind(Price_t=Serie,X_t=c(NA,returns(Serie)),sigma_t=NA,VaR=NA)
  Ns<- length(window(SSt[,2],start=s_startdate,end=s_enddate)) #length of the statistical sample
  
  if(debug) cat("total length:",Nt,'\n'," sample length:", Ns,'\n',"head(SSt$X_t)",head(SSt$X_t),'\n',
                "s_startdate:",s_startdate,'\n',"s_enddate",s_enddate,'\n')
  # Compute the conditioned volatility with the EWMA formula
  SSt$sigma_t=calc_Volatility(SSt$X_t,Nt)

  SSt <- window(SSt,start=s_startdate,end=time(SSt)[Nt])
  Nt <- length(SSt[,1])
  if(debug){
    cat("tail(SSt$sigma_t):",tail(SSt$sigma_t),'\n')
    print(head(SSt))
    print(tail(SSt))
  }

  
  # Compute standardized residuals 
  Z_t<- as.timeSeries(SSt$X_t[77:Ns]/SSt$sigma_t[77:Ns])
  rownames(Z_t)<- rownames(SSt)[77:Ns] 
  
  if(debug) cat("tail(SSt):",tail(SSt),'\n')
  #VaR_Z estimations

  if (usage==1) { VaR_Z<- calc_VaRnp(-Z_t,conf,VaR,s_startdate=s_startdate,s_enddate=s_enddate,debug=debug) }
  if (usage==2) { VaR_Z<- calc_VaRnorm(-Z_t,conf,VaR,s_startdate=s_startdate,s_enddate=s_enddate,debug=debug) }
  if (usage==3) { VaR_Z<- calc_VaRt(-Z_t,conf,VaR,s_startdate=s_startdate,s_enddate=s_enddate,debug=debug) }
  #in the 3rd VaR_Z method (student's t) there could be a convergence error due to the initial value of the computing algorithm, 
  #to fix this issue just need to add the argument "init=c(a,b,c)" where a, b and c are three numbers you can aribtrarily choose
  
  if(debug) cat("VaR_Z (if NULL have problems!!):",VaR_Z,'\n',"usage:",usage,'\n')
  #Compute conditioned VaR serie
  
  SSt$VaR<- SSt$sigma_t*VaR_Z

  if(debug){
    cat("Ns:",Ns," |Nt:",Nt," |SSt$VaR[(Ns+1):Nt]:",(-SSt$VaR[(Ns+1):Nt]),'\n',"SSt$X_t[(Ns+1):Nt]",SSt$X_t[(Ns+1):Nt],'\n')
    print(head(SSt))
    print(tail(SSt))
  }
  

  # backtesting
  test=exe_backtesting(X_t = SSt$X_t, VaR = SSt$VaR, Ns=Ns, Nt=Nt, conf=conf)
  
  if(debug) test
  #output
  EWMA_result=cbind(SSt,test$p.value,test$estimate)
  colnames(EWMA_result)=c("Prices","Returns","Volatility(>77th)","VaR(>77th)","p.value","exceed freq.")
  

  if(debug){ 
    print(head(EWMA_result))
    print(tail(EWMA_result))
    cat("end of EWMA_RiskMetrics\n\n")
  
  }
  #to get the test$p.value use the command "as.numeric(EWMA_RiskMetrics(...)$p.value[1])"
  EWMA_result
}
