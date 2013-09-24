fence <-
function(CONOP, events="observed", tag=1){
	CONOP@Sectfile->sectfile
	invisible(ifelse(events=="observed", plcdfile <- CONOP@Obsdfile, ifelse(events=="placed", plcdfile <- CONOP@Plcdfile, stop("The \'events\' argument should be \"observed\" (to use events as observed) or \"placed\" (to use events as placed by the optimization process)"))))
	plot(c(0, 10*nrow(sectfile)-5), rev(range(plcdfile[,3:ncol(plcdfile)],na.rm=T)), ylim=range(plcdfile[,3:ncol(plcdfile)],na.rm=T),
		yaxt="n", bty="n", xaxt="n", ylab="Depth", xlab="", type="n")
	tag <- switch(tag,2,4)
	for(i in 1:nrow(sectfile)){
		rect((sectfile[i,3]-1)*10, max(plcdfile[,2+i],na.rm=T), 5+(sectfile[i,3]-1)*10, min(plcdfile[,2+i],na.rm=T), col="white")
		sapply(unique(plcdfile[,2+i]), function(x){
			lines(c((sectfile[i,3]-1)*10,5+(sectfile[i,3]-1)*10),c(x,x))	
			})
		text(2.5+(sectfile[i,3]-1)*10,max(plcdfile[,2+i],na.rm=T)+5,sectfile[i,tag])
		}
	axis(2,at=seq(floor(min(plcdfile[,3:ncol(plcdfile)],na.rm=T)),ceiling(max(plcdfile[,3:ncol(plcdfile)],na.rm=T)),by=50),las=2,cex=0.8)
	for(i in 1:(nrow(sectfile)-1)){
		for(j in 1:nrow(plcdfile)){
			if(plcdfile[j,2]!=5){
				lines(c(5+(i-1)*10,10+(i-1)*10),c(plcdfile[j,as.vector(sectfile[sectfile[,3]==i,1])+2],plcdfile[j,as.vector(sectfile[sectfile[,3]==i+1,1])+2]),col="blue",lwd=1)
				}
			if(plcdfile[j,2]==5){
				lines(c(5+(i-1)*10,10+(i-1)*10),c(plcdfile[j,as.vector(sectfile[sectfile[,3]==i,1])+2],plcdfile[j,as.vector(sectfile[sectfile[,3]==i+1,1])+2]),col="orange",lwd=1)
				}
		}}
	}
