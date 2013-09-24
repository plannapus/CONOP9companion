lofo <-
function(site,mbsf,remove.na=TRUE){
	site[,colSums(site)>0]->site
	site[order(mbsf),]->site
	mbsf[order(mbsf)]->mbsf
	lofo<-array(dim=c(ncol(site),2))
	rownames(lofo)<-colnames(site)
	colnames(lofo)<-c("FO","LO")
	for(i in 1:ncol(site)){
		f <- as.numeric(mbsf[c(TRUE,cumsum(site[,i])==0) & c(site[,i]!=0,FALSE)])
		l <- as.numeric(mbsf[(c(rev(cumsum(rev(site[,i])))==0, FALSE) & c(FALSE,site[,i]!=0))[-1]])
		lofo[i,1] <- ifelse(length(f)>0, f, NA)
		lofo[i,2] <- ifelse(length(l)>0, l, NA)
		}
	if(remove.na==TRUE){
		lofo[is.na(lofo[,1]),1] <- mbsf[1]
		lofo[is.na(lofo[,2]),2] <- mbsf[nrow(site)]
		}
	return(lofo)
	}