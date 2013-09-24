sect.range<-function(CONOP, section, cex=0.8){
	sectfile <- CONOP@Sectfile
	eventfile <- CONOP@Eventfile
	obsdfile <- CONOP@Obsdfile
	plcdfile <- CONOP@Plcdfile
	Nsect <- CONOP@Sectfile[grep(section,sectfile[,4]),3]
	J <- which(eventfile[,1]%in%obsdfile[!is.na(obsdfile[,Nsect+2]) & obsdfile[,2]%in%c(1,2),1])
	par(mar=c(1,5,10,1))
	plot(NA,type="n",xaxs="i",yaxs="i",yaxt="n",xaxt="n",xlab="",ylab="",
		xlim=c(0,length(J)+1),
		ylim=c(min(plcdfile[,Nsect+2],na.rm=TRUE),max(plcdfile[,Nsect+2],na.rm=TRUE)))
	mtext(sectfile[Nsect,4],side=2, line=3)
	abline(h=unique(obsdfile[,Nsect+2]),col="grey95")
	axis(2, las=2, cex.axis=cex, at=unique(obsdfile[,Nsect+2]))
	for(j in seq_along(J)){
		O <- obsdfile[obsdfile[,1]==J[j] & obsdfile[,2]==1,Nsect+2]
		segments(j,plcdfile[plcdfile[,1]==J[j] & plcdfile[,2]==1,Nsect+2],
				j,plcdfile[plcdfile[,1]==J[j] & plcdfile[,2]==2,Nsect+2], lwd=4, col="red")
		segments(j,obsdfile[obsdfile[,1]==J[j] & obsdfile[,2]==1,Nsect+2],
				j,obsdfile[obsdfile[,1]==J[j] & obsdfile[,2]==2,Nsect+2], lwd=2, col="black")
		mtext(eventfile[j,3],las=2, at=j, cex=cex, line=0.5)
		}
	}