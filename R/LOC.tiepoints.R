LOC.tiepoints <-
function(CONOP,saveLOC=FALSE, events){
	composfile <- CONOP@Composfile
	invisible(ifelse(events=="observed", obsdfile <- CONOP@Obsdfile, ifelse(events=="placed",obsdfile <- CONOP@Plcdfile, stop("The \'events\' argument should be \"observed\" (to use events as observed) or \"placed\" (to use events as placed by the optimization process)"))))
	sectfile <- CONOP@Sectfile
	obsdfile[obsdfile==0]<-NA
	nrow(sectfile)->Nsect
	tie.points<-list()
	a <- rev(composfile[,4])
	for(SITE in 1:Nsect){
		b <- obsdfile[,SITE+2]
		type <- obsdfile[,2]
		abs(type)->type
		type[type>2] <- -1
		
		layout(matrix(c(0,1,2,3),nrow=2,byrow=T),width=c(3,20),height=c(3,20))
		par(mar=c(0,0,5,5))
		plot(0,0,type="n",xlim=range(a,na.rm=TRUE),ylim=c(0,10),xaxt="n",yaxt="n",yaxs="i",xaxs="i",xlab="",ylab="",bty="n")
		invisible(sapply(unique(a), function(x) lines(c(x,x), c(0,10), lty=1,col="black")))
		axis(3,at=pretty(a))
		box()
		
		par(mar=c(5,5,0,0))
		plot(0,0,type="n",ylim=range(b,na.rm=T),xlim=c(0,10),xaxt="n",yaxt="n",yaxs="i",xaxs="i",xlab="",ylab="",bty="n")
		invisible(sapply(unique(b)[!is.na(unique(b))], function(x) lines(c(0,10), c(x,x), lty=1,col="black")))
		axis(2,at=pretty(b),las=2)
		box()
		
		par(mar=c(5,0,0,5))
		palette(c("red","blue"))
		plot(0,0,xaxs="i",yaxs="i",yaxt="n",xlab="",ylab="",xaxt="n",bty="o",
			 ylim=range(b,na.rm=TRUE),xlim=range(a, na.rm=TRUE),type="n")
		points(a, b, pch=23+type, bg=alpha(abs(type), 0.1), cex=3)
		title(sub=paste(sectfile[SITE,4]," vs Composite Section",sep=""),line=2,cex.sub=2)
		box()

		locator(type="o",pch=3,lty=2)->tie.points[[SITE]]
		if(saveLOC==TRUE){dev.copy2pdf(width=10,height=10,file=paste(sectfile[SITE,2],"_LOC.pdf",sep=""))}
		dev.off()
		}
	names(tie.points) <- sectfile[,4]
	return(tie.points)
	}
