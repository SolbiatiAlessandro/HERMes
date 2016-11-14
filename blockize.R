#divide the vector of the hma_sign in blocks, output vect with [block_length, datestart, dateend]
# as.Date([blockize[i,2],origin="1970-01-01")

blockize <- function(my_hma){


	v <- matrix(ncol=4, nrow=length(my_hma))
	t=1
	count=0

	for(i in 25:length(my_hma)){
		cat(i)
		if(as.numeric(my_hma[i])!=as.numeric(my_hma[i-1])){
			count<-count+1
			v[count,1] <- t
			v[count,2] <- as.Date(time(my_hma)[i])
			v[count,3] <- as.Date(time(my_hma)[i-t])
			v[count,4] <- my_hma[i]	
			t=0
		}
	t<-t+1	
	}
	v[1:count,]
}