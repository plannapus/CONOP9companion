prep.conop <-
function(Input, lofolist, relaxed=FALSE, paired=TRUE){
	eventfile <- Input@Eventfile
	for(k in 1:length(lofolist)){
		j <- 1
		siteevent <- array(dim=c(length(lofolist[[k]][!is.na(lofolist[[k]])]),8))
		n<-length(lofolist[[k]][!is.na(lofolist[[k]])])
		siteevent <- data.frame(rep(NA,n),rep(NA,n),rep(k,n),rep(NA,n),rep(NA,n),rep(NA,n),rep(NA,n),rep(NA,n))
		siteevent[,3] <- k
		for(i in 1:nrow(lofolist[[k]])){
			if(paired==TRUE){
				siteevent[j:(j+1),1] <- eventfile[eventfile[,3]==rownames(lofolist[[k]])[i],1]
				siteevent[j,2] <- 1
				siteevent[j+1,2] <- 2
				siteevent[j,4] <- lofolist[[k]][i,1]
				siteevent[j+1,4] <- lofolist[[k]][i,2]
				if(relaxed==FALSE){
					siteevent[j,6] <- 1
					siteevent[j+1,6] <- 2
					}
				if(relaxed==TRUE){siteevent[j:(j+1),6] <- 3}
				siteevent[j:(j+1),7:8] <- "1.00"
				j <- j+2
				}
			if(paired==FALSE){
				siteevent[j,1] <- eventfile[eventfile[,3]==rownames(lofolist[[k]])[i],1]
				if(is.na(lofolist[[k]][i,1])){
					siteevent[j,2] <- -1
					siteevent[j,4] <- lofolist[[k]][i,1]
					if(relaxed==FALSE){siteevent[j,6] <- 1}
					if(relaxed==TRUE){siteevent[j,6] <- 3}
					siteevent[j,7:8] <- "1.00"
					j <- j+1
					}
				if(is.na(lofolist[[k]][i,2])){
					siteevent[j,2] <- -2
					siteevent[j,4] <- lofolist[[k]][i,2]
					if(relaxed==FALSE){siteevent[j,6] <- 2}
					if(relaxed==TRUE){siteevent[j,6] <- 3}
					siteevent[j,7:8] <- "1.00"
					j <- j+1
					}
				}
			}
		horizons <- sort(unique(siteevent[,4]))
		siteevent[,5] <- sapply(siteevent[,4],function(x)which(horizons==x))
		if(k==1){loadfile <- siteevent}else{loadfile <- rbind(loadfile,siteevent)}
		}
	loadfile <- loadfile[order(as.numeric(loadfile[,1]),as.numeric(loadfile[,2]),as.numeric(loadfile[,3])),]
	Input@Loadfile <- as.data.frame(loadfile)
	return(Input)
	}