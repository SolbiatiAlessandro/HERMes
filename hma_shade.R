#shade the dygraph accordingly to the hma sign
#accept a blocks_mat from the blockize() function

hma_shade <- function(blocks_mat, my_dygraph){
	len<-length(blocks_mat[,1])
	for(i in 1:len-1){
		shading <- list()
    		shading$from <- as.Date(blocks_mat[i,2])
    		shading$to <- as.Date(blocks_mat[i,3])
		ifelse(blocks_mat[i,4]==0, shading$color <- "#a6ffa3", shading$color <- "#ffa5a5")
    		shading$axis <- "x"
		my_dygraph$x$shadings[[length(my_dygraph$x$shadings) + 1]] <- shading
	}
	print(my_dygraph$x$shadings)
	my_dygraph
}