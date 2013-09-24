pacman<-function(ab_mat,top=5,bottom=3){
	outlier<-rep(0,nrow(ab_mat))
	for(j in 1:ncol(ab_mat)){
		sp_ab <- as.vector(ab_mat[,j])
		nb_top <- sum(sp_ab)*top/100
		nb_bottom <- sum(sp_ab)*bottom/100
		out <- sp_ab
		sp_ab[!(cumsum(sp_ab)>=nb_top & rev(cumsum(rev(sp_ab)))>=nb_bottom)] <- 0
		out <- out - sp_ab
		ab_mat[,j] <- sp_ab
		outlier <- outlier + out
		}
	names(outlier)<-row.names(ab_mat)
	list(Trimming=ab_mat,Profiling=outlier)->paclist
	return(paclist)
	}