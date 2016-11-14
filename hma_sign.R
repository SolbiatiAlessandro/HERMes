hma_sign <- function(hma){
	sign <- hma
	l <- length(hma)
	for(i in 24:l){
		if(hma[i]>hma[i-1]){sign[i]=1}
		else{sign[i]=0}
	}
	sign
}